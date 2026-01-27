# Claude Project Instructions

## Documentation Contribution

**After non-WIP commits**, consider whether the work done could contribute to documentation:

1. **Check if relevant docs exist** in `docs/modules/ROOT/pages/`
2. **Suggest updates** for:
   - New features → Getting Started or How-To content
   - Bug fixes revealing common issues → How-To troubleshooting
   - Architectural changes → Understanding section
   - New API → Reference updates

3. **Check `docs/CONTENT-STATUS.md`** for:
   - Placeholder pages that could be filled
   - Missing content that matches what was just implemented

4. **When suggesting docs**, offer to:
   - Write a complete draft
   - Add to existing page
   - Create a new stub with outline

The goal is to keep documentation in sync with implementation while context is fresh.

## Documentation Structure

```
docs/
├── antora.yml                 # Component config
├── CONTENT-STATUS.md          # Tracks what's written/missing
└── modules/ROOT/
    ├── nav.adoc               # Navigation
    └── pages/
        ├── getting-started/   # Tutorials
        ├── how-to/            # Task guides
        ├── understanding/     # Conceptual
        └── reference/         # API docs
```

Build docs: `npx antora antora-playbook.yml`
Output: `build/site/`

## Project Context

- PSD3 is a PureScript D3 visualization library
- Uses finally-tagless encoding for expressions
- Tree AST with multiple interpreters (D3, String, Mermaid)
- Part of ecosystem: psd3-selection, psd3-graph, psd3-tree, psd3-sheetless
