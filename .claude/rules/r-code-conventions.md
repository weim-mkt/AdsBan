---
paths:
  - "**/*.R"
  - "code/**/*.R"
---

# R Code Standards

**Standard:** Senior Principal Data Engineer + PhD researcher quality

---

## 1. Reproducibility

- `set.seed(888)` called before every step involving randomness (default seed: 888)
- `renv` for project isolation with `pak` as install backend; `library()` for loading
- All paths via `here::here()` for cross-platform compatibility (macOS, Windows, Linux)
- `dir.create(..., recursive = TRUE)` for output directories
- All file I/O uses UTF-8 encoding explicitly (`fread(..., encoding = "UTF-8")`)
- Script files saved as UTF-8 (no BOM)

### renv + pak Setup
```r
# Project setup (once)
renv::init()
options(renv.config.pak.enabled = TRUE)

# Install packages
renv::install(c("data.table", "ggplot2", "here", "ggthemes"))

# Snapshot for reproducibility
renv::snapshot()
```

## 2. Function Design & Code Style

- `snake_case` naming, verb-noun pattern
- Roxygen-style documentation
- Default parameters, no magic numbers
- Named return values (lists or data.tables)
- Section headings use `# Content ----` format (four trailing dashes)
- Use base pipe `|>` everywhere (not magrittr `%>%`)
- One pipe per line, pipe into the next line

## 3. Data Manipulation

- Prefer `data.table` over `dplyr` for data wrangling
- Pipe-style data.table with `|> _[...]` placeholder syntax
- One operation per line for readability

```r
dt |>
  _[x > 1] |>
  _[, `:=`(y = x + 1)] |>
  _[dt2, `:=`(a = i.a), on = c("key" = "key")]
```

## 4. Domain Correctness (Difference-in-Differences)

- [ ] **Estimand stated** — ATT, ATT(g,t), ES(e), or ITT — declared in the script header or a comment before estimation runs, not inferred after
- [ ] **Clustering justified** — `fixest::feols(..., cluster = ~id_var)`; the clustering level matches the treatment-assignment mechanism (geographic area, household, etc.) and is documented in-line
- [ ] **Staggered DiD diagnosed** — if treatment timing varies across units, do NOT use plain TWFE without (a) a Goodman-Bacon (2021) decomposition, or (b) a heterogeneity-robust estimator: Callaway & Sant'Anna (2021), de Chaisemartin & D'Haultfœuille (2020), Sun & Abraham (2021), or Borusyak et al. (2024)
- [ ] **Control group declared** — never-treated vs. not-yet-treated vs. eventually-treated; a one-line comment in the script names the choice and why
- [ ] **Parallel-trends check** — event-study leads plotted; pre-trend joint test reported; any lead coefficient > 1 pre-period SE flagged
- [ ] **Sample restrictions logged** — at each filter (pre-only, post-only, zero-purchase, missing covariates), log N dropped and reason to a restriction-counts table
- [ ] **SE method matches spec** — clustered / HC / bootstrap — pick one, justify, don't silently switch between models

## 5. Visual Identity

- Use `ggthemes::theme_stata()` as the default ggplot theme

### Figure Dimensions for Beamer
```r
ggsave(filepath, width = 12, height = 5, bg = "transparent")
```

## 6. Data Caching

**Heavy computations cached to disk; downstream scripts load pre-computed data.**

- `fst::write_fst()` / `fst::read_fst()` for data.table / data.frame objects
- `qs2::qs_save()` / `qs2::qs_read()` for model objects and other R objects

```r
fst::write_fst(dt, here::here("data", "cleaned", "descriptive_name.fst"))
qs2::qs_save(model_fit, here::here("output", "descriptive_name.qs2"))
```

## 7. Project Structure

```
./code/
  main.R          # Entry point, sources 00-*.R files
  00-setup.R      # Packages + global env vars
  01-load_data.R  # Load raw data
  02-*.R, 03-*.R  # Subsequent pipeline steps
./data/
  raw/            # Raw data (read-only)
  cleaned/        # Cleaned/processed data
./output/         # Analysis outputs, tables, model results
```

- `main.R` orchestrates by sourcing numbered scripts in order
- Numbered prefix (00-, 01-, 02-) ensures execution order is clear

## 8. Common Pitfalls

| Pitfall                                            | Impact                                           | Prevention                                                                                 |
| ---------------------------------------------------| -------------------------------------------------| -------------------------------------------------------------------------------------------|
| Missing `bg = "transparent"`                       | White boxes on slides                            | Always include in `ggsave()`                                                               |
| Hardcoded paths                                    | Breaks on other machines                         | Use `here::here()` for all paths                                                           |
| Platform path separators                           | `\` vs `/` breaks                                | `here::here()` handles this                                                                |
| Non-UTF-8 data files                               | Encoding errors                                  | `fread(..., encoding = "UTF-8")`                                                           |
| TWFE with staggered adoption                       | Negative-weighted ATTs; wrong-sign coefficients  | Use Callaway-Sant'Anna, de Chaisemartin-D'Haultfœuille, Sun-Abraham, or Borusyak et al.; add Goodman-Bacon decomposition as diagnostic |
| `fixest` default SE (not clustered)                | Over-rejection of H₀                             | Always pass `cluster = ~var` or `vcov = "cluster"` — never rely on defaults                |
| Silent dropping of zero-purchase households        | Biased ATT (selection on outcome)                | Balance the panel explicitly; log N dropped at each filter step                            |
| AiMark `Date_of_purchase` as integer YYYYMMDD      | `as.Date()` silently coerces wrong               | Parse via `substr(..., 1, 4)` + explicit year extraction (see `utils/01-clean_data.R`)     |
| MongoDB extract without running `mongod`           | `extract_openfoodfacts_data()` returns NULL      | Check `mongod` running on port 27018 before calling `extract_openfoodfacts_data()`         |
| Barcode / product_code mismatch across AiMark years| Wrong joins between barcode map and purchase data| Run `check_barcode()` + `check_product_coverage()` before any merge                        |
| Macros Dropbox via Parallels mount unavailable     | `data/raw data/*` symlinks dead on Linux-only host| Guard loaders with `if (!dir.exists(...)) stop(informative)`; document mount in data_description.md |

## 9. Line Length & Mathematical Exceptions

**Standard:** Keep lines <= 100 characters.

**Exception: Mathematical Formulas** -- lines may exceed 100 chars **if and only if:**

1. Breaking the line would harm readability of the math (influence functions, matrix ops, finite-difference approximations, formula implementations matching paper equations)
2. An inline comment explains the mathematical operation:
   ```r
   # Sieve projection: inner product of residuals onto basis functions P_k
   alpha_k <- sum(r_i * basis[, k]) / sum(basis[, k]^2)
   ```
3. The line is in a numerically intensive section (simulation loops, estimation routines, inference calculations)

**Quality Gate Impact:**
- Long lines in non-mathematical code: minor penalty (-1 to -2 per line)
- Long lines in documented mathematical sections: no penalty

## 10. Numerical Discipline

See [`r-reviewer.md`](../agents/r-reviewer.md) Category 11 ("Numerical Discipline") for the full checklist. Headline rules:

- **No float equality.** Never use `==` on doubles. Use `all.equal()` or `abs(a - b) < tol`.
- **CDF clamping** to an OPEN interval. Exact 0 or 1 passed to `qnorm()` / `pbinom()` etc. produces `±Inf`. Project-wide epsilon:

  ```r
  eps <- 1e-12
  p <- pmin(1 - eps, pmax(eps, p))   # now safe for qnorm(p)
  ```

- **Integer literals for counts.** `nrow <- 1000L` (not `1000`), `for (i in 1L:nL)` — avoids silent promotion.
- **Pre-allocate vectors** before loops (`numeric(n)`, `vector("list", n)`), never grow with `c()`.
- **Deterministic bootstrap seeding.** Set seed before the bootstrap, and if the bootstrap is nested, set per-replicate seeds as `seed_base + b`.
- **Explicit `na.rm = TRUE/FALSE`.** Never rely on defaults for `mean()`, `sd()`, `sum()` on data with potential NAs.
- **No `T` / `F`.** They're variables, not constants — write `TRUE` / `FALSE`.

## 11. Code Quality Checklist

```
[ ] renv initialized with pak backend
[ ] Packages loaded at top via library()
[ ] set.seed(888) before each randomness step
[ ] Paths via here::here()
[ ] Section headings use # Content ---- format
[ ] Base pipe |> used (not %>%)
[ ] Functions documented (Roxygen)
[ ] Figures: transparent bg, explicit dimensions, theme_stata()
[ ] Tabular data cached with fst::write_fst()
[ ] Model objects cached with qs2::qs_save()
[ ] Files read/written with UTF-8 encoding
[ ] Comments explain WHY not WHAT
[ ] Numerical discipline: no float ==, CDF clamping with eps, pre-allocated vectors
```
