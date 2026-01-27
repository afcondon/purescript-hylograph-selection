# PSD3 Presentation Planning

Interview and planning session for presenting PSD3 to senior engineers (mostly FP folks).

## Interview Transcript Summary

### Origin Story
- Tufte influence from university days, longtime interest in data viz
- D3.js admiration, but JavaScript shortcomings
- PureScript solves JS problems without Haskell's deployment friction
- Multiple rewrites over years, converging on current architecture

### Evolution of the Architecture
1. **Thin wrapper** (pelotom's purescript-d3) - monadic chaining, didn't scale
2. **Overly ideological** - complex type signatures tracking effects, no real benefit
3. **Pragmatic type safety** - focus on viz correctness, not effect tracking
4. **Finally tagless breakthrough** - overcome expression problem, idiomatic PureScript
5. **Ian Ross's typeclass trick** - polymorphic attributes in static types
6. **2022 proof of concept** - rich, interactive, performant viz in principled FP
7. **2025 LLM-accelerated rewrite** - vast expansion using Claude Code

### Key Technical Insights
- "Even complex visualizations have slight true structure when formalized"
- Row polymorphism: "viz works with any record that has these fields"
- Multiple interpreters: D3, Mermaid, English, structure visualization, sound
- "You can't glance at audio" - sonification requires repetition

### The Three Audiences
1. **Practical PureScript users**: "Can I make charts?" → Yes, here's code
2. **Data viz practitioners**: Componentization, row polymorphism, dynamic possibilities
3. **UI builders broadly**: Viz as interface, not just charts - stop using dropdowns for trees

### Library Ecosystem
- **psd3-selection**: Core, declarative viz
- **psd3-simulation**: Physics/force layouts, declarative control from PureScript
- **psd3-tree**: Tree-specific utilities
- **psd3-graph**: Graph algorithms (pathfinding, centrality, etc.)
- **psd3-layouts**: Pure PureScript layout algorithms
- **psd3-sheetless**: Functional spreadsheet reimagined from FP-first principles
- **psd3-tidal**: Port of TidalCycles for algorave/live coding

### Tidal and Sheetless as Proof Points
- **Tidal roundtripping**: text → AST → visual → edited → AST → text (bidirectional)
- **Sheetless**: What if spreadsheets used strong FP from day one? Foldables not ranges, SQL first-class, rich viz built-in

### The Challenge with FP Audiences
- Unusually small overlap between "Haskellers" and "visualization"
- Not skepticism, just absence of intuition
- "Puzzle lovers" don't notice puzzling UI
- Strategy: find the converts, not mass conversion
- Show don't tell - demos over documentation

### Target Communities
- PureScript community (friendly, some will get it)
- DataHaskell Discord (already shown interest)
- ObservableHQ / Mike Bostock (maybe)
- Tidal / algorave community
- Scientific viz: Our World in Data, Works in Progress, etc.
- Math explainers: Grant Sanderson et al

### Current Maturity
- LLM usage: exploration, fearless refactoring, algorithm development, code review
- Showcases more complex than prior commercial work
- Lots of AI-generated code outside 2022 core
- Type system keeps "man and machine on tight rails"
- Would audit before commercial use

---

## Narrative Structure for Presentation

### Talking Points (speaker notes, not slides)

**1. Origin Story Arc**
- Tufte → D3 admiration → PureScript conviction → multiple rewrites → finally-tagless breakthrough → LLM-accelerated expansion

**2. Technical Architecture Story**
- Thin wrapper → overly ideological → pragmatic type safety → declarative AST with multiple interpreters
- Key insight: "even complex visualizations have slight true structure when formalized"

**3. The Three Audiences**
- Practical: "Can I make charts in PureScript?" → Yes, here's code
- Practitioners: Componentization, row polymorphism, dynamic possibilities
- UI builders: Viz as interface, not just charts

**4. Proof Points**
- Tidal roundtripping (bidirectional editing works)
- Sheetless (spreadsheets from FP-first principles)
- Sonification (accessibility, "can't glance at audio")
- Chimera visualizations (hybrid rendering)

**5. The Honest Reality**
- FP folks often don't "get" visualization (not skepticism, just absence)
- Show don't tell - demos over documentation
- Already worthwhile; community adoption is the dream

### Presentation Flow (website as slides)

1. **Opening** (talking): Tufte→D3→PureScript origin
2. **Quick win** (demo): Simple chart, reveal code
3. **Architecture reveal** (demo): Same viz as Mermaid/English/structure
4. **The real pitch** (demo): Code Explorer - viz as UI
5. **Proof of depth** (demo): Tidal roundtripping or Sheetless
6. **Sonification aside** (demo): "Can't glance at audio"
7. **Close** (talking): Where to find it, communities

---

## Website Redesign Ideas

### Example Taxonomy
```
example (component)     → "here's a bar chart"
poster (composition)    → "here's components working together" (Simpson's)
chimera (hybrid)        → "here's new grammar possibilities"
showcase (application)  → "here's viz AS the UI" (Code Explorer, Algorave, Sheetless)
```

### Combined Tour/Example View
Each example shows together:
- Explanation text
- Small visualization (with magnifier for detail)
- AST code (PureScript)
- Grammar diagram

### Scrollytelling Single Page
- Long scrolling page instead of click-through nav
- Sticky section headers that displace as you scroll
- Reduces click friction, maintains context
- People more likely to scroll than click "next"

### Treemap Navigation Idea
- Full-screen responsive treemap showing whole site
- Click to zoom into sections
- Breadcrumbs for instant backward nav
- Thumbnails (not live viz) for performance

### Triple View Switcher (Daring Idea)
Front page offers three views of same content:
1. **Docs** - traditional AsciiDoc
2. **Graph** - force layout of concept relationships
3. **Treemap** - content hierarchy

Switchable at any time. The site demonstrates PSD3 by being built with it.

### Traditional Landing Page
Hero boxes with benefits, thumbnails linking to demos.
Responsive 1-4 column layout.

---

## Benefits List (for landing page / marketing)

### Core Value Props
1. Easy type-safe charts with PureScript
2. Re-usable components
3. Maximum flexibility for custom work
4. Integrate data viz with UI for complex domains
5. Generate documentation or other viz output (Mermaid, dot/graphviz, English)
6. Explore sonic output to augment or replace visual
7. Visualize more than arrays - maps, sets, trees, graphs all first class. Extensible!

### Additional Benefits
- Declarative - describe what you want, not how to draw it
- Framework agnostic - Halogen, plain JS, whatever
- Data-source agnostic - local, API, SQL results
- Inspect and debug - see the structure of your viz
- Bidirectional editing - graphical changes round-trip to code (Tidal proof)
- Battle-tested D3 under the hood - not reinventing rendering
- Compile-time guarantees your data fits your viz
- Same viz definition, swap the dataset

### Possible Groupings
- "For getting started quickly"
- "For production use"
- "For pushing boundaries"

---

## Next Steps

### Content Creation
- [ ] Finalize benefits list
- [ ] Map existing demos to taxonomy (example/poster/chimera/showcase)
- [ ] Identify gaps in demo coverage
- [ ] Write missing content

### Technical Validation
- [ ] Create psd3-react repo to test framework agnosticism
- [ ] Add dot/graphviz output interpreter
- [ ] Test purescript-react integration

### Website Implementation
- [ ] Choose between landing page approaches
- [ ] Build shock-and-awe front page after content exists
- [ ] Implement view switcher if pursuing that direction

---

## Content Graph (for future force layout)

See conversation for ASCII diagram of:
- Quadrant structure (Getting Started, How-To, Understanding, Reference)
- Showcases (Code Explorer, Algorave, Sheetless)
- Concepts (Finally Tagless, AST, Interpreters, Row Poly, Comonads, etc.)
- Libraries (psd3-selection, simulation, tree, graph, layouts)
- Visualization types (Charts, Hierarchical, Relational, Novel)
- Key edges/relationships between all of the above
