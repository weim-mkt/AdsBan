---
paths:
  - "Slides/**/*.tex"
  - "Quarto/**/*.qmd"
  - "code/**/*.R"
  - "scripts/R/**/*.R"
---

# AdsBan Knowledge Base

<!-- Seeded 2026-04-22. Grows organically as analyses run, replications land,
     and the paper is drafted. Claude reads this before creating/modifying any
     analysis, slide, or manuscript content. -->

## Research Question

**How do advertising bans on junk food affect household consumption?** Identification via DiD using:

1. **Primary:** London Underground junk-food ad ban (2019) — spatial + temporal variation in ad exposure for UK households with comparable socioeconomic profiles outside London.
2. **Future:** 2025 UK-wide TV junk-food ad ban — nationwide treatment; comparison across product categories with/without HFSS classification.

## Notation Registry

| Symbol | Meaning | Notes |
|--------|---------|-------|
| `i` | Household (panel unit) | AiMark household panellist ID |
| `t` | Time period (week / month / quarter — TBD) | Pick one and stick with it across specs |
| `g` | Treatment cohort (first-treated period) | For staggered designs |
| `Y_it` | Outcome (consumption of targeted category) | e.g., junk-food spend, HFSS calories, purchase count |
| `D_it` | Treatment indicator (post-ban × in-treated-area) | 0/1 |
| `ATT` | Average treatment effect on the treated | Aggregated across cohorts |
| `ATT(g,t)` | Cohort-time-specific ATT | Callaway–Sant'Anna style |
| `ES(e)` | Event-study coefficient at lead/lag `e` | e ∈ {−L, …, −1, 0, 1, …, K} |

## Empirical Applications

| Application | Paper | Dataset | Status | Purpose |
|-------------|-------|---------|--------|---------|
| London Underground junk-food ad ban | (TBD — fill during lit review) | AiMark UK 2015–2023 + OFF HFSS labels | Planning | Primary DiD identification |
| UK 2025 TV ads ban | (prospective) | AiMark post-2024 + industry reports | Future | Secondary replication / extension |

## Data Sources

| Source | Path | Access | Key Fields |
|--------|------|--------|------------|
| AiMark UK (2005–2023) | `data/raw data/AiMark Data/UK YYYY-YYYY/` | Parallels-mounted Dropbox (macOS volume `/Volumes/dataHP/...`) | Purchase: `product_code`, `Date_of_purchase` (YYYYMMDD int), household_id (var name TBD); Barcode: `product`, `barcode`, category hierarchy (see `data/data_description.md`) |
| Open Food Facts | `data/raw data/openfoodfacts/` | MongoDB dump (requires `mongod` on port 27018) + Parquet + JSONL | `code` (barcode), `categories_tags`, `nutriscore_grade`, `nova_group`, NOVA/HFSS markers |
| Cleaned outputs | `data/cleaned data/` | Local, gitignored | e.g., `countries_in_jsonl.csv` |

## Tolerance Thresholds (for replication / robustness)

<!-- Fill in when the first replication target lands. Until then: defaults
     from .claude/rules/replication-protocol.md Phase 3. -->

| Quantity | Tolerance | Rationale |
|----------|-----------|-----------|
| Integer counts (N) | Exact match | No reason to differ |
| Point estimates | < 0.01 | Paper-display rounding |
| Standard errors | < 0.05 | Bootstrap/clustering variation |
| Percentages | < 0.1pp | Display rounding |

## Design Principles

| Principle | Rationale | Where enforced |
|-----------|-----------|----------------|
| Replication-first | Match prior result before extending | `.claude/rules/replication-protocol.md` |
| Pre-declare estimand | ATT vs ATT(g,t) vs ES; pick before estimating | `r-code-conventions.md` §4 |
| Staggered-safe | Never plain TWFE on heterogeneous timing | `r-code-conventions.md` §4 |
| Transparent-bg figures | Beamer + Quarto compatible from draft #1 | `r-code-conventions.md` §5 + INV-11 |

## Anti-Patterns (Don't Do This)

| Anti-Pattern | Why It Bites | Correction |
|--------------|--------------|-----------|
| TWFE on staggered rollout without robustness | Negative weights → wrong sign | CS / dCdH / Sun-Abraham / Borusyak; report Goodman-Bacon diagnostic |
| Dropping zero-purchase households silently | Selection on outcome → biased ATT | Explicit balance step + restriction-counts table |
| Event-study plots without joint pre-trend test | "Looks parallel" is not a test | Joint F-test on leads; report p-value |
| Mixing time aggregation across specs | Confounds effect-size interpretation | One time unit per paper; document choice |

## R Code Pitfalls

| Bug | Impact | Fix |
|-----|--------|-----|
| `fixest` default SE | Over-rejection | Always explicit `cluster = ~var` |
| `as.Date()` on YYYYMMDD int | Silent mis-coercion | `substr(as.character(x), 1, 4)` for year; `as.IDate(sprintf("%08d", x), "%Y%m%d")` for full date |
| `rbindlist(fill = TRUE)` across AiMark vintages | Silent column drift | Diff schemas first: `sapply(files, \(f) names(fread(f, nrows = 0)))` |

## Estimand Registry (fill in as analyses run)

| Analysis | Estimand | Estimator | Sample | Status |
|----------|----------|-----------|--------|--------|
| (TBD) | | | | |
