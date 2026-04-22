# Workflow Quick Reference — AdsBan

**Model:** Contractor (you direct, Claude orchestrates after plan approval)

---

## The Loop

```
Your instruction
    ↓
[PLAN] (if multi-file, non-trivial, or ambiguous) → Save to quality_reports/plans/ → Your approval
    ↓
[EXECUTE] Implement → verify → (for R: simple orchestrator; no multi-round review loop)
    ↓
[REPORT] Summary + what's ready + what's next
    ↓
Repeat
```

---

## I Ask You When

- **Design forks:** "Option A (fast, biased) vs. Option B (slow, unbiased). Which?"
- **Estimand ambiguity:** "Spec says ATT — aggregated across cohorts, or by cohort?"
- **Replication edge case:** "Paper reports -1.632; we got -1.641. Investigate or accept within tolerance?"
- **Scope question:** "Also refactor X while here, or focus on this task?"
- **Data contract:** "Column name mismatch between AiMark 2015-2020 and 2021-2023. Unify one way or keep both?"

---

## I Just Execute When

- Code fix is obvious (bug, pattern application, convention enforcement)
- Verification (tolerance checks, tests, code runs cleanly)
- Documentation (session logs, plan updates, MEMORY.md entries)
- Figure production per established standards (transparent bg, theme_stata, explicit dimensions)
- Commits are **NEVER** in the "just execute" bucket — always wait for an explicit user request.

---

## Quality Gates

| Score | Action |
|-------|--------|
| >= 80 | Ready to commit (advisory — `/commit` halts below this) |
| < 80  | Fix blocking issues first |

For experimental work in `explorations/`: 60/100 threshold (see `exploration-fast-track.md`).

---

## Non-Negotiables

- **Paths:** `here::here()` for all R paths — cross-platform (macOS, Fedora-via-Parallels Linux)
- **Seed:** `set.seed(888)` before every randomness step (not once at top — per-step for independent re-runs)
- **Pipe:** base `|>`, never magrittr `%>%`
- **Data wrangling:** `data.table` with `|> _[...]` placeholder pipe syntax (one op per line)
- **Caching:** `fst::write_fst()` / `fst::read_fst()` for tabular; `qs2::qs_save()` / `qs2::qs_read()` for models
- **Figures:** `ggthemes::theme_stata()`, `bg = "transparent"`, explicit `width`/`height`, save PDF + PNG
- **File I/O:** explicit `encoding = "UTF-8"` on every `fread`/`fwrite`
- **Numerical discipline:** no float `==`; CDF clamping with `eps = 1e-12`; integer literals (`1L`) for counts; pre-allocate vectors; explicit `na.rm` every time
- **Line length:** ≤ 100 chars except documented math formulas (see `r-code-conventions.md` §9)
- **Commits:** never without an explicit user request — even after plan approval

---

## Preferences

- **Visual:** Publication-ready from first draft — transparent bg, Stata-style theme, sentence-case labels with units, readable at projection size (base_size ≥ 14)
- **Reporting:** Concise bullets + small tables; details on request. End-of-turn summary in one or two sentences.
- **Session logs:** Always (post-plan, incremental, end-of-session) — see `.claude/rules/session-logging.md`
- **Replication:** Strict — match integers exactly; point estimates within 0.01; SEs within 0.05. Flag near-misses rather than swallowing.
- **Check-in cadence:** For the first few sessions, pause at phase boundaries for a one-paragraph status + any ambiguity flags; ramp down once the user signals comfort.

---

## Exploration Mode

For experimental work, use the **Fast-Track** workflow:
- Work in `explorations/[project]/` folder
- 60/100 quality threshold (vs. 80/100 for production `code/R/`)
- No plan needed — just a 2-minute research-value check
- See `.claude/rules/exploration-fast-track.md`

Graduation to `code/R/` requires score ≥ 80, clear code, tests pass.

---

## Next Step

You provide task → I plan (if needed) → Your approval → Execute → Brief report → Repeat.
