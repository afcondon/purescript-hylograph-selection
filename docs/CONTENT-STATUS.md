# PSD3 Documentation Content Status

## Summary

| Section | Complete | Placeholder | Total |
|---------|----------|-------------|-------|
| Getting Started | 2 | 3 | 5 |
| How-To | 8 | 2 | 10 |
| Understanding | 8 | 0 | 8 |
| Reference | 1 | 0 | 1 |
| **Total** | **19** | **5** | **24** |

---

## Getting Started

| Page | Status | Notes |
|------|--------|-------|
| Quickstart | ✅ Complete | TL;DR of full Hylograph API surface - distilled from skill doc |
| Your First Selection | ✅ Complete | Full tutorial with working code |
| Building a Bar Chart | 📝 Placeholder | Needs: data binding, scales, axes |
| From D3.js to PSD3 | 📝 Placeholder | Needs: migration guide, pattern mapping |
| Web App Architecture | ✅ Complete | Halogen + PSD3 integration patterns |

### Content to Write

**bar-chart.adoc**:
- Loading data (JSON, CSV)
- Using `joinData` for data binding
- Creating scales with `PSD3.Scale`
- Adding axes
- Complete working example

**from-d3.adoc**:
- Side-by-side D3 vs PSD3 code
- Conceptual differences (imperative vs declarative)
- Common D3 patterns translated
- Escape hatches when needed

---

## How-To Guides

| Page | Status | Notes |
|------|--------|-------|
| PSD3 in a Simple Web Page | ✅ Complete | No-framework usage |
| Halogen Events from PSD3 | ✅ Complete | Callback bridge pattern |
| Simple CSS Hover Effects | ✅ Complete | CSS-only approach |
| Coordinated Highlighting | ✅ Complete | Migrated from existing |
| Scrolling to Elements | ✅ Complete | Click-to-scroll pattern |
| Tooltips | ✅ Complete | Migrated from existing |
| Linked Brushing | ✅ Complete | Cross-view selection |
| Zoomable Visualizations | ✅ Complete | Pan and zoom |
| Data Joins Deep Dive | 📝 Placeholder | Needs: GUP, nested joins |
| Animated Transitions | 📝 Placeholder | Needs: transition config, easing |

### Content to Write

**data-joins.adoc**:
- `joinData` vs `updateJoin` vs `nestedJoin`
- General Update Pattern (enter/update/exit)
- Key functions for identity matching
- Nested data structures
- Working examples of each pattern

**transitions.adoc**:
- TransitionConfig type
- Duration, delay, easing options
- Enter/update/exit transitions
- Sequencing multiple transitions
- Performance considerations

---

## Understanding

| Page | Status | Notes |
|------|--------|-------|
| Finally Tagless Architecture | ✅ Complete | Core architectural pattern |
| The Selection AST | ✅ Complete | Tree structure explained |
| Interpreters | ✅ Complete | Multiple backends |
| Functional Spreadsheets | ✅ Complete | Migrated - comonads, recursion schemes |
| Unified Data DSL | ✅ Complete | Migrated - computation/viz bridge |
| Design Decisions | ✅ Complete | Migrated - architectural choices |
| Graph Algorithms | ✅ Complete | Migrated - psd3-graph design |
| Future Directions | ✅ Complete | Migrated - advanced concepts |

All Understanding pages are complete.

---

## Reference

| Page | Status | Notes |
|------|--------|-------|
| Module Overview | ✅ Complete | Module listing with descriptions |

Note: Full API reference will be on Pursuit once packages are published.

---

## Additional Content Suggestions

### Getting Started (consider adding)
- [ ] Interactive Tutorial - step-by-step with live examples
- [ ] PSD3 for React/Vue developers - integration patterns
- [ ] Debugging PSD3 visualizations

### How-To (consider adding)
- [ ] Responsive visualizations - resize handling
- [ ] Color scales and palettes
- [ ] Axes and legends
- [ ] Performance optimization for large datasets
- [ ] Testing visualizations
- [ ] Accessibility in visualizations
- [ ] Custom interpreters
- [ ] Formula analysis (from ANALYSIS-MODULE-PLAN)
- [ ] Excel import (from EXCEL-COMPILER-IMPLEMENTATION-PLAN)

### Understanding (consider adding)
- [ ] Effect and Effect.Ref in PSD3 - why mutable refs
- [ ] FFI strategy - D3.js binding approach
- [ ] Row polymorphism in attributes
- [ ] Migration from purescript-d3 (old library)

---

## Priority Order for Writing

### Must Have (for initial release)
1. bar-chart.adoc - critical getting started content
2. data-joins.adoc - fundamental concept
3. transitions.adoc - common use case

### Should Have
4. from-d3.adoc - helps D3 users transition
5. Testing visualizations how-to
6. Responsive visualizations how-to

### Nice to Have
7. Custom interpreters
8. Performance optimization
9. Formula analysis / Excel import (more niche)

---

## Website Generation Notes

The following pages should be generated for the demo website from these docs:

| AsciiDoc Page | Website Page |
|---------------|--------------|
| getting-started/your-first-selection.adoc | getting-started/first-selection.html |
| getting-started/bar-chart.adoc | getting-started/bar-chart.html |
| how-to/tooltips.adoc | how-to/tooltips.html |
| how-to/coordinated-highlighting.adoc | how-to/highlighting.html |
| understanding/finally-tagless.adoc | understanding/finally-tagless.html |
| ... | ... |

The website versions should:
- Strip AsciiDoc navigation/xref
- Embed live visualizations where applicable
- Use the website's CSS/styling
- Add "View source" links to the AsciiDoc

---

## Maintenance

When writing new content:
1. Create the .adoc file in the appropriate section
2. Add it to nav.adoc
3. Update this tracking document
4. Run `npx antora antora-playbook.yml` to rebuild
5. Check the rendered output
