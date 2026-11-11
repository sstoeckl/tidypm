# tidypm

Companion package to **Tidy Asset Allocation in R**.

`PortfolioAnalytics` covers portfolio optimisation thoroughly. `tidypm` covers
what it does not, and smooths the parts that fight you:

- a tibble interface to the `xts`-only API (`as_pm()`, `as_xts_pm()`)
- a reproducible data layer built from public sources on your machine, with a
  manifest so any divergence from the book's build is visible
  (`pm_init()`, `pm_data()`, `pm_check()`, `pm_sources()`, `pm_cache_dir()`)
- optimisation universes and exclusion screens (`pm_universe()`, `pm_screen()`)
- per-period solver diagnostics for rolling backtests (`diagnose_rebalancing()`)
- parametric portfolio policies, absent from the package entirely
- tracking-error constraints and an information-ratio objective
- band-based rebalancing, where the package offers a calendar only
- yield-curve decomposition and duration overlays
- sustainability screens built from public data

Nothing lands here until the chapter that needs it is being written, so every
function ships with a worked example behind it.

## Install

```r
# pak::pak("sstoeckl/tidypm")
devtools::load_all()
```

## Status

Early, but no longer empty.

**Working and tested.** The `xts` boundary (`as_pm()`, `as_xts_pm()`) and the
whole data layer: `pm_init()` builds the Tier 1 asset-class panel from Kenneth
French, FRED and one fund proxy and caches it in `pm_cache_dir()`; `pm_data()`
reads it without touching the network; `pm_check()` compares your build against a
reference manifest; `pm_sources()` is the sleeve registry, including the sleeves
that are declared but not yet built; `pm_universe()` and `pm_screen()` supply the
larger cross-sections and the exclusion sets. `bond_total_return()` and
`credit_total_return()` construct the two government sleeves and the credit
sleeve from yields.

**Stub.** `diagnose_rebalancing()` is documented in full and aborts on every
call. That is deliberate — see its help page — and chapter 22 implements it.

**Roadmap.** Everything else. See `R/tidypm-package.R` for the chapter-by-chapter
plan; nothing lands until the chapter that needs it is being written.
