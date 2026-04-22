# CLAUDE.MD -- AdsBan: Junk Food Ads Ban & Household Consumption

<!-- Project-specific. If you update generic patterns (rules, agents, skills)
     consider whether they should also propagate upstream to
     claude-code-my-workflow/. See harness memory: template_version.md.
     Keep this file under ~150 lines — Claude loads it every session. -->

**Project:** The Effect of Junk Food Advertising Bans on Household Consumption
**Institution:** University College London (UCL)
**Researcher:** Wei Miao
**Branch:** main
**Primary language:** R (occasional Python, Stata)
**Method:** Difference-in-differences
**Natural experiments:** London Underground junk-food ad ban (primary); 2025 UK TV ads ban (future)
**Target journals:** JMR, Marketing Science

---

## Core Principles

- **Plan first** -- enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** -- run/render and confirm output at the end of every task
- **Replication-first** -- replicate prior results to the dot before extending (see `.claude/rules/replication-protocol.md`)
- **Quality gates** -- nothing ships below 80/100 (advisory; `/commit` halts)
- **[LEARN] tags** -- when corrected, save `[LEARN:category]` to [MEMORY.md](MEMORY.md)

Cross-session context: [MEMORY.md](MEMORY.md) (committed) + harness memory at `/home/weimiao/.claude/projects/-media-psf-data-AdsBan/memory/` (local). Past plans, specs, session logs, and merge reports are in [quality_reports/](quality_reports/).

---

## Folder Structure

```
AdsBan/
├── CLAUDE.md                     # This file
├── MEMORY.md                     # Learning log (committed)
├── .claude/                      # Rules, skills, agents, hooks, settings
├── code/
│   └── R/                        # R pipeline (00-setup.R, 01-clean_data.R, main.R, utils/)
├── data/                         # Raw + cleaned data (gitignored)
│   ├── raw data/                 # AiMark + Open Food Facts (macOS Dropbox via Parallels symlinks)
│   ├── cleaned data/             # Intermediate outputs
│   └── data_description.md       # Variable documentation
├── notes/                        # Research notes
├── renv/  renv.lock              # R package isolation
├── explorations/                 # Experimental work (fast-track, 60/100 threshold)
├── quality_reports/              # Plans, specs, session logs, merge reports, decisions
├── templates/                    # Session log, quality report, spec templates
├── scripts/                      # Utility scripts (quality_score.py, validate-setup.sh, ...)
└── claude-code-my-workflow/      # symlink → upstream template (for diffing only, gitignored)
```

Slide/paper artifacts (`Slides/`, `Quarto/`, `Preambles/`, `Figures/`, `Bibliography_base.bib`, `master_supporting_docs/`) will be materialized on demand when slide/paper work begins.

---

## Commands

```bash
# R pipeline (entry point)
Rscript code/R/main.R

# Validate workflow setup
./scripts/validate-setup.sh

# Quality score
python3 scripts/quality_score.py <file.R or file.qmd>

# Upstream template diff (when checking for updates)
cd claude-code-my-workflow && git fetch upstream && git log HEAD..upstream/main
```

Slide-specific commands (LaTeX 3-pass, `sync_to_docs.sh`, palette sync) activate when `Slides/` and `Quarto/` exist. See `claude-code-my-workflow/CLAUDE.md` for the full command set.

---

## Quality Thresholds (advisory)

| Score | Checkpoint | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for deployment |
| 95 | Excellence | Aspirational |

Enforced by `/commit` (halts + asks for override); not enforced by a git pre-commit hook. R scripts follow the simplified research orchestrator (`.claude/rules/orchestrator-research.md`) — no multi-round reviews.

---

## Skills Quick Reference (active for this project)

| Command | What It Does |
|---------|-------------|
| `/data-analysis [dataset]` | End-to-end R analysis |
| `/review-r [file]` | R code quality review |
| `/audit-reproducibility [paper]` | Enforce replication tolerance on paper ↔ code |
| `/lit-review [topic]` | Literature search + synthesis |
| `/research-ideation [topic]` | Research questions + strategies |
| `/interview-me [topic]` | Interactive research interview |
| `/review-paper [file]` | Manuscript review (`--adversarial` / `--peer <journal>`) |
| `/respond-to-referees [report] [manuscript]` | R&R cross-ref + response draft |
| `/seven-pass-review` | Seven-pass adversarial manuscript review |
| `/verify-claims [file]` | Chain-of-Verification fact-check |
| `/devils-advocate` | Challenge design decisions |
| `/commit [msg]` | Stage, commit, PR, merge (respects quality gates) |
| `/validate-bib` | Cross-reference citations |
| `/context-status` | Show session health + context usage |
| `/deep-audit` | Repository-wide consistency audit |
| `/permission-check` | Diagnose permission layers |
| `/learn [skill-name]` | Extract discovery into persistent skill |

**Inactive until `Slides/` + `Quarto/` materialize:** `/compile-latex`, `/deploy`, `/extract-tikz`, `/new-diagram`, `/proofread`, `/visual-audit`, `/pedagogy-review`, `/qa-quarto`, `/slide-excellence`, `/translate-to-quarto`, `/create-lecture`.

---

## Non-Negotiables (this project)

- **Paths:** `here::here()` for all R paths (cross-platform: macOS, Fedora-via-Parallels, Linux)
- **Seed:** `set.seed(888)` before **every** randomness step (not once at top)
- **Pipe:** base `|>` only, never magrittr `%>%`
- **Data wrangling:** `data.table` with `_[...]` placeholder pipe syntax
- **Caching:** `fst::write_fst()` for tabular; `qs2::qs_save()` for model objects
- **Figures:** `ggthemes::theme_stata()`, `bg = "transparent"`, explicit `width`/`height`, save PDF + PNG
- **Tolerance thresholds:** TBD per DiD output type — first replication target sets the baseline
- **Commits:** user explicitly asks — I do not auto-commit, even after plan approval

---

## Current Project State

| Stage | Script | Status | Notes |
|---|---|---|---|
| Setup | `code/R/00-setup.R` | Done | Loads 30+ R packages; consider migrating to `renv + pak` per conventions |
| Raw data load | `code/R/01-clean_data.R` + `utils/01-clean_data.R` | In progress | AiMark (barcode + purchase) loaders + Open Food Facts MongoDB/Parquet/JSONL helpers |
| Cleaning | TBD | Not started | |
| DiD analysis | TBD | Not started | London Underground ad ban as treatment |
| Robustness | TBD | Not started | |
| Slides / Paper | TBD | Not started | Activates slide-related skills and rules |
