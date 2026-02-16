# Reachability Analysis: Findings and Roadmap

**Date**: 2026-02-14
**Context**: Post-Phase 8 (Selection API deletion), analysing the Reach feature's output in Minard Code Explorer

---

## What Happened

After deleting ~4,040 lines of Selection API dead code (Phase 8), we turned on the **Reach** feature in Minard to get a before/after screenshot. The "after" view showed only 18 of 71 modules as reachable, with the other 53 grayed out. Investigation revealed this was partly a data bug and partly genuine - but the picture is more nuanced than "reachable = alive, unreachable = dead."

## Bug Found: Ghost Modules from Stale Snapshots

**Priority: Fix next**

### The problem

The `minard-loader` creates new snapshots but doesn't clean up old `package_version` entries and their associated modules. When hylograph-selection was reloaded after Phase 8, the old snapshot's modules persisted in the `modules` table with zero import data.

The bulk `/api/v2/all-imports` endpoint returns **all** modules via an unscoped `FROM modules` query. This produces duplicate entries:

```
Hylograph.HATS: 21 imports   (module_id 2013, current)
Hylograph.HATS:  0 imports   (module_id  815, stale)
```

The frontend builds an import map as a `Map String (Set String)` keyed by module name. When there are duplicates, whichever entry processes last wins. If the stale empty entry overwrites the real one, that module appears to import nothing, and BFS can't traverse through it.

**Result**: All 18 reachable modules appeared as entry points only. Zero transitive edges were found. The entire internal import graph was invisible.

### The fix

Two parts:

1. **Loader**: When loading a new snapshot for an existing project, clean up orphaned `package_version` entries and their child data (modules, declarations, imports, calls) that aren't referenced by any remaining snapshot.

2. **Server** (defense in depth): Scope the `getAllImports` query to the active snapshot's package set, or at minimum deduplicate by preferring the entry with more data. The per-module endpoint (`/api/v2/module-imports/:id`) already works correctly because it queries by specific module ID.

## Corrected Reachability Picture

With ghost modules fixed (by merging duplicate import sets), the real numbers:

| Status | Count | Description |
|--------|-------|-------------|
| **Entry points** | 19 | Directly imported by external packages (Minard frontend, showcases, etc.) |
| **Transitively reachable** | 7 | Internal modules reached via BFS from entry points |
| **Unreachable library** | 34 | Not reachable from any external consumer in the DB |
| **Test modules** | 11 | Expected - tests are consumers, not consumed |

The 7 transitively reachable modules include `HATS.Transitions`, `Internal.Element.Operations`, `Internal.Transition.Types`, `Expr.Animation`, `Expr.Path.Generators`, `Expr.Units`, and `Unified.DataDSL`. These are genuine internal dependencies correctly identified by BFS.

## The 34 "Unreachable" Library Modules: What Are They?

### Genuinely superseded (candidates for future deletion)

| Module | LOC | Notes |
|--------|-----|-------|
| `Internal.Transition.Manager` | 498 | Old transition infrastructure, superseded by `hylograph-transitions` package |
| `Internal.Transition.FFI` | 71 | Same - old transition FFI |
| `Internal.Transition.Scene` | 118 | Same - old declarative transition specs |
| `Data.DependencyGraph` | 24 | Utility never adopted |

**~711 LOC** of genuinely dead code identified. A follow-up Phase 9 could remove these.

### Optional interpreters (alive, just not consumed by loaded projects)

| Module | LOC |
|--------|-----|
| `Interpreter.English` | 121 |
| `Interpreter.Mermaid` | 218 |
| `Interpreter.MetaHATS` | 660 |
| `Interpreter.SemiQuine` | 214 |
| `Interpreter.SemiQuine.TreeToCode` | 187 |
| `Interpreter.SemiQuine.Types` | 84 |

These are the point of a finally-tagless architecture: consumers choose which interpreter to import. They're alive in the API surface, just not exercised by the projects currently in the DB. Loading the polyglot website (which uses SemiQuine) or showcase projects would make some of these light up.

### Expression DSL layer (used by tests, available to consumers)

`Expr.Attr`, `Expr.Integration`, `Expr.Interpreter.CodeGen`, `Expr.Interpreter.SVG`, `Expr.Interpreter.PureSVG`, `Expr.Interpreter.Meta`, `Expr.Sugar` - these are consumed by tests and available for downstream use.

### Available interaction/shape/utility modules

`Interaction.Brush`, `Interaction.Pointer`, `Shape.Arc`, `Shape.Pie`, `Shape.Polygon`, `Brush.*`, `Classify`, `Tooltip`, `Scale.FP`, `Data.Graph.*`, `Data.Tree`, `Unified.*` - library surface area, not consumed by currently-loaded projects.

## Toward Actionable Classification

The current Reach feature draws a binary line: reachable or not. But as this investigation showed, "unreachable" conflates several very different situations that call for different responses. Data visualization is about actionable classification - the tool should tell you *what to do*, not just *what is*.

### A richer taxonomy

The reachability tool has all the data it needs to produce a multi-level classification automatically. Here's what it could distinguish:

| Classification | Signal | What it means | Action |
|---|---|---|---|
| **Dead subsystem** | Unreachable cluster of mutually-importing modules | A whole capability was superseded | Delete the cluster |
| **Dead leaf** | Unreachable, no internal importers, no test consumers | Orphaned code, nothing touches it | Delete or investigate |
| **Test-only** | Only imported by test modules, not by library or external code | Lives in `src/` but only consumed by `test/` | Move to test directory |
| **Available** | Unreachable from loaded projects but part of published API surface (interpreters, shapes) | Optional capability, consumers choose to import it | Leave alone - this is the library working as designed |
| **Stale available** | Available + not modified in a long time (git data) | Optional capability that may have bitrotted | Review: still compiles? Still relevant? |
| **Recently written** | Unreachable but recently modified | New code not yet integrated by consumers | Expected - give it time |

The first three are **actionable now**. The last three are **contextual** - they need judgement, but the tool can surface the signal.

### Test-only code belongs with tests

Code that's only imported by test modules but lives in `src/` is a code organization smell. If `Expr.Sugar` is only consumed by `Test.Expr.ExprSpec` and friends, it should either:

1. **Move to `test/`** - it's test infrastructure, not library API
2. **Get a real consumer** - if it's genuinely part of the public API, some showcase or application should use it

The reachability tool could flag this directly: "these 7 modules in `src/` are only imported by test code." That's a concrete, actionable finding that doesn't require deep architectural knowledge to act on.

Note the tension: tests can also **mask staleness**. If we'd had tests importing the old Selection API modules, they'd have shown as "consumed" and we might not have questioned them. A module that's only test-consumed is in a grey zone - the tests might be validating live API surface, or they might be propping up dead code.

### Git history as a decay signal

Combining reachability with `git log --follow` data could surface temporal patterns:

- **Dead and forgotten**: unreachable + last modified > 6 months ago. Strong delete signal.
- **Dead but recent**: unreachable + modified within the last month. Probably new code not yet wired up. Don't flag.
- **Alive but stale**: reachable + last modified > 1 year ago. Stable infrastructure (good) or bitrotting foundation (investigate).
- **Churn in dead code**: unreachable + high recent commit frequency. Someone's working on code that nothing uses - either it's about to be integrated, or effort is being wasted.

The commit data is already in the DB (`commits`, `module_commits` tables). The loader extracts git history. This is a matter of joining it with reachability in the frontend.

### The tool should do what we just did manually

The investigation that found the ~711 LOC of genuinely dead Transition code was:

1. Compute reachability (BFS from external consumers)
2. For unreachable modules, check internal import relationships
3. Identify clusters of mutually-unreachable modules (a subsystem went dark)
4. Cross-reference with git history (when were they last touched?)
5. Check if any are test-only consumers

**The tool has all this data.** Steps 1-5 could run automatically and produce a structured report, or better, visual classification in the treemap. Instead of binary reachable/unreachable coloring, use a palette that encodes the taxonomy above.

## Re-exports

hylograph-selection doesn't happen to use PureScript's `module X (module Y)` re-export syntax, so this wasn't the cause of the current issue. But re-exports are fundamental to PureScript/Haskell:

- **`Prelude`** is the canonical example: it re-exports from `Data.Show`, `Data.Eq`, `Data.Ord`, `Control.Bind`, etc. Any reachability analysis that only follows direct imports will miss the fact that importing `Prelude` transitively depends on dozens of modules.
- The data pipeline already handles this: the Rust loader extracts `reExports` from `docs.json`, stores them in `module_reexports`, and the server has a working endpoint. **The frontend just never fetches or uses it.**
- **Fix**: Augment the import graph with re-export edges before running BFS. If module A re-exports declarations from module B, treat A→B as an import edge for reachability purposes.

## Clustering as a Complementary View

Reachability answers "is this code live?" but doesn't answer "how does this code relate structurally?" These are orthogonal questions, and the most interesting insights come from crossing them.

### What clustering reveals

- **Cohesion clusters**: Groups of modules that import each other heavily (the Expr interpreter family, the Transition subsystem, the Interaction modules). These clusters reveal architectural boundaries that namespace hierarchy only partially captures.
- **Coupling bridges**: Modules that connect otherwise-separate clusters. These are architectural chokepoints - changing them has outsized ripple effects.
- **Dead clusters vs dead leaves**: A single unreachable module is a leaf to prune. A cluster of 4 unreachable modules that import each other (like Transition.Manager + FFI + Types + Scene) is a **subsystem** that was superseded wholesale. That's a qualitatively different signal from scattered dead leaves - and it's exactly what we found manually.

### Cross-cutting reachability with clustering

The current Reach coloring overlays reachability on namespace-organized treemap cells. A force-directed or adjacency view organized by *actual coupling* would tell a different story:

- The namespace hierarchy says "these modules are in `Internal.Transition`" but the coupling graph says "these modules form a tight subsystem that nothing else touches."
- A cluster view colored by reachability would immediately show: "this whole island is gray" (dead subsystem) vs "this cluster is mostly alive but has one gray outlier" (dead leaf within a live system).
- Combined with git history: a gray cluster whose last commit was 8 months ago is a strong "delete this" signal. A gray cluster with commits last week is "work in progress."

### Prototype direction

The existing adjacency and chord diagram views in Minard already show module coupling. The prototype could:

1. Take the module import graph for a package
2. Run community detection (Louvain, label propagation, or even just connected components on the undirected import graph)
3. Layout clusters as grouped nodes
4. Color by the richer taxonomy (dead subsystem, test-only, available, etc.)
5. Overlay git recency as node size or border treatment

This would be a genuinely new view that no existing code cartography tool offers: **architectural clustering cross-cut with liveness and temporal signals**.

## Transition.Manager et al: Not FFI-Mediated After All

Initial hypothesis was that `InterpreterTick` called `Transition.Manager` through JavaScript FFI, making the dependency invisible to PureScript imports. **This turned out to be wrong.**

`InterpreterTick` imports `HATS.Transitions`, which imports `Hylograph.Transition.Tick` from the **`hylograph-transitions` package** (a separate published library). The old `Internal.Transition.Manager/FFI/Scene` modules are genuinely superseded infrastructure from before the transition system was extracted. They're dead code, not FFI-hidden dependencies.

This is good news: PureScript's import discipline means the module graph is trustworthy. Unlike JavaScript where `require()` can be dynamic, PureScript imports are static and complete. FFI `.js` companion files can introduce hidden dependencies, but in practice this is rare in well-structured code - the FFI files typically implement primitive operations, not call back into other PureScript modules.

## Action Items

### Immediate (fix what's broken)

1. **Fix ghost module bug** in `minard-loader` - clean up orphaned package_versions on reload
2. **Scope `getAllImports` query** to active snapshots as defense in depth
3. **Delete Transition.Manager/FFI/Scene + Data.DependencyGraph** (~711 LOC, Phase 9)

### Reach feature enhancements

4. **Replace binary reachable/unreachable with richer taxonomy** - dead subsystem, dead leaf, test-only, available, stale available, recently written
5. **Flag test-only modules** - code in `src/` only imported by `test/`
6. **Integrate git history** with reachability - surface temporal decay signals
7. **Incorporate re-exports into reachability BFS** (data already in DB, just need frontend integration)

### New views

8. **Prototype clustering view** - community detection on import graph, colored by liveness taxonomy, with git recency overlay
