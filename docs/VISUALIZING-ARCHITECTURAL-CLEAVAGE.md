# Visualizing Architectural Cleavage: A Real Test Case

## The Problem

The hylograph-selection library contained ~4,700 lines of dead code (the
Selection API) that nobody imported. This was invisible in all existing
CodeExplorer views despite the tool having rich dependency data. The cleavage
was discovered through manual grep-based investigation, not through
visualization.

This is exactly the kind of structural insight a code cartography tool should
surface. How would we make it visible?

---

## What We're Looking For

The pattern is: **a subsystem of modules that are internally connected but have
no incoming edges from the rest of the codebase**. More precisely:

- A cluster of modules that import each other
- Zero (or near-zero) imports from modules outside the cluster
- The cluster may still export symbols and have outgoing imports to shared
  infrastructure, but nothing depends on it

This is different from:
- **Unused declarations** (finer grained - within a module)
- **Orphan modules** (single modules with no connections - easier to spot)
- **Circular dependencies** (about cycles, not isolation)

It's a **dead subgraph** in the import graph: structurally intact, functionally
disconnected.

---

## Why Current Views Miss It

### Treemap
Shows containment (package -> module -> declaration) and sizes. The Selection
modules appear as cells in the package treemap, looking just like any other
module. Nothing distinguishes "large and used" from "large and dead." Treemaps
encode *size* and *containment* but not *connectivity*.

### Beeswarm (by topological layer)
Positions modules by dependency depth. Selection modules have the same topo
layers as HATS modules (they depend on similar infrastructure). They'd be
interleaved in the same layers, indistinguishable. Topo layer encodes *depth in
the dependency chain* but not *whether anything depends on you*.

### Force Graph
Shows dependency edges directly. The Selection modules would appear as a
subgraph. But in a large force graph, every cluster of connected modules looks
like every other. Without visual encoding of *directionality* or
*external connectivity*, the dead subgraph blends into the visual noise.

### Chord Diagram / Adjacency Matrix
Shows pairwise connections. The Selection modules would show connections to each
other and to shared infrastructure, looking like any other cohesive subsystem.
The matrix doesn't distinguish "well-connected and used" from "well-connected
and orphaned."

### The Common Failure Mode

All these views encode **internal structure** (what connects to what) but not
**external relevance** (does anything outside care about this?). They answer
"what does the dependency graph look like?" but not "what would we lose if we
deleted this?"

---

## Evidence: The Minard Screenshot (Before Reachability Coloring)

The circle-packing view of `hylograph-selection` in Minard illustrates the
problem concretely. Looking at the package overview:

- **Selection** (bottom center) is one of the most visually prominent clusters
  in the entire package. It's a large, dense bubble group with a distinctive
  orange/amber highlighted circle, conveying "substantial, cohesive subsystem."
  It looks important.

- **HATS** (center-left), **InterpreterTick** and the interpreters (English,
  Mermaid, SemiQuine, MetaAST, TreeToCode - right side), **Attribute**,
  **Operations**, **Scale**, **FP** - all the actively-used module groups -
  look visually identical in kind. Same bubble-cluster treatment, same visual
  weight relative to their size.

- There is **nothing** in the coloring, sizing, or layout that distinguishes
  Selection (dead, ~4,700 lines, zero external consumers) from HATS (alive,
  the core API, imported by every downstream package). They are rendered with
  equal visual status.

- The smaller module groups scattered around the edges (FFI adapters, Helpers,
  Join, Query, Zoom, Pointer, etc.) similarly give no signal about whether
  they're reachable from outside the package.

This is the "before" state. With reachability coloring applied, the Selection
cluster and its dependent modules (Join, Query, Capabilities, the D3
interpreter) would desaturate to gray while the rest of the package retains its
normal coloring. The dead subgraph would become immediately obvious - a gray
island in a sea of color.

The plan is to implement reachability coloring first, capture the before/after
comparison, and then proceed with the actual refactoring (removing the Selection
API). The contrast between the two screenshots - one where dead code hides in
plain sight, one where it's unmistakable - should demonstrate the value of
evaluative visualization clearly.

---

## Design Approaches

### Approach 1: Reachability Coloring ("Dead Code Heat Map")

**Concept**: Color every module by whether it's reachable from the "public API
surface" - the set of modules actually imported by downstream consumers.

**How it works**:
1. Define the "roots": modules imported by at least one downstream package
   (showcases, apps, simulation, etc.)
2. Trace forward through the import graph from those roots
3. Modules reachable from roots = alive (cool colors)
4. Modules not reachable = dead (warm colors / desaturated)

**Applied to our test case**: The HATS modules, scales, behaviors,
interactions, expressions, and interpreters would all be colored "alive" (they're
reachable from showcase imports). The Selection modules (Operations, Join,
Query, Capabilities, D3 interpreter) would be colored "dead" - nothing reaches
them from the roots.

**Visual encoding**:
- Apply as an overlay to any existing view (treemap, beeswarm, force graph)
- Alive modules: normal coloring (topo layer, package, etc.)
- Dead modules: desaturated gray, or a distinct warning color
- Partially dead: modules reachable only via dead paths could get an
  intermediate color

**Strengths**:
- Works with any existing layout (additive, not a new view type)
- Binary signal is easy to interpret
- Immediately actionable ("these gray modules can be deleted")

**Weaknesses**:
- Requires defining "roots" (what counts as public API?)
- Binary (alive/dead) doesn't show *degree* of usage
- Doesn't reveal the *structure* of the dead subgraph

**Implementation sketch**:
```sql
-- In DuckDB: find modules imported by downstream packages
WITH root_modules AS (
  SELECT DISTINCT target_module
  FROM module_imports
  WHERE source_package IN ('hylograph-simulation', 'psd3-prim-zoo-mosh', ...)
    AND target_package = 'hylograph-selection'
),
reachable AS (
  -- Recursive CTE: trace imports from roots
  SELECT module_name FROM root_modules
  UNION
  SELECT mi.target_module
  FROM module_imports mi
  JOIN reachable r ON mi.source_module = r.module_name
  WHERE mi.target_package = 'hylograph-selection'
)
SELECT m.module_name,
       CASE WHEN r.module_name IS NOT NULL THEN 'alive' ELSE 'dead' END AS status
FROM modules m
LEFT JOIN reachable r ON m.module_name = r.module_name
WHERE m.package_name = 'hylograph-selection'
```

---

### Approach 2: Import Flow Sankey ("Who Depends on What?")

**Concept**: A Sankey diagram where the left column is downstream consumers,
the middle column is modules in the target package, and the right column is
infrastructure dependencies. Flow width = number of import edges.

**Applied to our test case**: The left column (consumers) would show fat flows
into HATS, InterpreterTick, Scale, Behavior modules - and zero flow into
Selection modules. The Selection cluster would appear as an isolated island with
no incoming flow from the left, only outgoing flow to shared infrastructure on
the right.

**Visual encoding**:
- Left: downstream packages/modules that import from hylograph-selection
- Middle: hylograph-selection modules, vertically grouped by cluster
- Right: upstream dependencies (prelude, web-dom, etc.)
- Flow width: number of import relationships
- Modules with no incoming flow from the left = dead

**Strengths**:
- Flow direction makes the asymmetry obvious (inflow vs no inflow)
- Sankey already exists in the visualization toolkit
- Shows *how much* each module is used, not just alive/dead
- Grouping in the middle column would naturally reveal clusters

**Weaknesses**:
- Only works for one package at a time
- Can get cluttered with many modules
- Doesn't work well in the existing navigation hierarchy (it's a
  cross-cutting view)

---

### Approach 3: Community Detection + Edge Cutting ("The Cleavage View")

**Concept**: Run community detection on the module import graph within a
package, then visualize the communities and the edges between them. Dead
subgraphs appear as communities with only outgoing inter-community edges
(they depend on other communities) but no incoming ones (nothing depends on
them).

**How it works**:
1. Build the import graph for all modules in a package
2. Run community detection (Louvain, label propagation, etc.)
3. Layout communities as groups (force-directed with group constraints, or
   nested circles)
4. Color inter-community edges by direction:
   - Bidirectional: healthy coupling (gray)
   - Only A->B, never B->A: potential cleavage point (red)
   - No edges at all: isolated community (warning)

**Applied to our test case**: Community detection would identify at least two
communities: {HATS, Interpreters, InterpreterTick, Friendly, ...} and
{Selection.Types, Selection.Operations, Selection.Join, Selection.Query,
Capabilities.Selection, Capabilities.Transition, Interpreter.D3}. The edges
between them would be almost entirely one-directional: HATS imports
`ElementType` from Selection.Types, but Selection never imports from HATS. The
Selection community would have zero incoming edges from outside.

**Visual encoding**:
- Communities as colored groups (bubble pack or convex hull)
- Within-community edges: thin, low opacity
- Cross-community edges: thick, colored by directionality
- Community "health" indicator: ratio of incoming to outgoing edges
- Dead communities: desaturated or outlined in a warning color

**Strengths**:
- Reveals the *structure* of the cleavage, not just that it exists
- Algorithmically identifies boundaries (no manual "root" definition)
- Generalizes to any package, not just ones with known dead code
- Could reveal partial cleavages (subsystems coupled by only 1-2 edges)

**Weaknesses**:
- Community detection algorithms have parameters that affect results
- Needs a new layout mode (grouped force or nested)
- More complex to implement and interpret

---

### Approach 4: The Pruning Simulator ("What If We Deleted This?")

**Concept**: An interactive view where you select a module (or cluster) and the
visualization shows what would break. If nothing would break, the module is
safe to delete. If many things would break, it's critical infrastructure.

**How it works**:
1. Select a module or drag a selection box around a group
2. The system computes: which other modules import anything from the selected
   set?
3. Importers outside the selected set are highlighted as "would break"
4. If no importers exist outside the set: "safe to remove" indicator
5. Can iteratively expand the selection to find the minimal set that's
   independently removable

**Applied to our test case**: Select `Selection.Operations`. The system shows
its importers: `Selection.Query`, `InterpreterTick`, `Interpreter.D3`. But
`InterpreterTick` only imports `createElementWithNS` (a trivially extractable
function). Select the whole Selection cluster - the system shows that only
`ElementType` leaks out, and only to be used as a shared enum. The "impact" is
near zero.

**Visual encoding**:
- Selected modules: highlighted
- "Would break" modules: red glow or connecting lines
- "Safe to remove" set: green border or checkmark
- Impact score: number of external importers x imported symbols

**Strengths**:
- Directly answers the question: "can I delete this?"
- Interactive: supports exploration and hypothesis testing
- Educational: teaches developers about dependency structure
- Useful for refactoring, not just dead code detection

**Weaknesses**:
- Requires symbol-level import data (not just module-level)
- Interactive rather than at-a-glance
- Doesn't proactively identify dead code (you have to ask about each module)

---

## Recommendation: Layer the Approaches

These aren't mutually exclusive. The strongest solution combines them:

### Minimum Viable Feature: Reachability Coloring (Approach 1)

Add a new coloring mode to the existing views:

```purescript
data ColoringMode
  = DefaultUniform
  | ProjectScope
  | FullRegistryTopo
  | ProjectScopeTopo
  | GitStatus
  | Reachability    -- NEW: color by external reachability
```

This plugs into the existing coloring infrastructure. In the package treemap
or beeswarm, dead modules immediately stand out. No new layout needed, no new
view - just a new way to color the existing views.

**Data requirement**: For each module in the target package, compute whether
it's transitively imported by any module in any other package. This is a
straightforward graph reachability query against the existing `module_imports`
table.

### Next Step: Directional Community View (Approach 3)

Once reachability coloring reveals that dead modules exist, the natural follow-up
question is "why?" and "where's the boundary?" A community detection view
answers this by showing the subsystem structure and the (missing) connections
between subsystems.

This would be a new view mode at the Package level, alongside the existing
treemap/chord/matrix options.

### Power Feature: Pruning Simulator (Approach 4)

For active refactoring, the pruning simulator turns CodeExplorer from a
read-only cartography tool into a refactoring planning tool. This is the
highest-effort but also highest-value addition.

---

## The Meta-Lesson

This test case reveals a general principle: **structural visualizations
(what connects to what) are necessary but not sufficient for architectural
insight. You also need evaluative visualizations (what matters? what's alive?
what would break?).**

The difference is between a map that shows all roads and a map that shows
traffic. Both are useful. But only the traffic map reveals that the highway
through the abandoned district carries zero cars.

CodeExplorer currently excels at the structural map. Reachability coloring
would add the first evaluative layer - and this one test case suggests it
would immediately surface actionable insights.
