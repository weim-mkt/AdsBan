# Explorations — AdsBan

Sandbox for experimental work. Anything that might not graduate to `code/R/` belongs here first.

## Structure

```
explorations/
├── README.md             # (this file)
├── ACTIVE_PROJECTS.md    # (create when first exploration is started)
├── [project-name]/
│   ├── README.md         # Goal, status, findings (use templates/exploration-readme.md)
│   ├── R/                # Code (use _v1, _v2 for iterations)
│   ├── scripts/          # Test scripts
│   ├── output/           # Results
│   └── SESSION_LOG.md    # Progress notes
└── ARCHIVE/
    ├── completed_[project]/
    └── abandoned_[project]/
```

## Rules

- Use fast-track workflow: 60/100 quality threshold, no plan needed, research-value check only.
- Graduation to `code/R/` requires ≥ 80/100 quality score, tests pass, code clear without deep context.
- Abandoned work moves to `ARCHIVE/` with a short explanation (use `templates/archive-readme.md`).

See `.claude/rules/exploration-folder-protocol.md` and `.claude/rules/exploration-fast-track.md`.
