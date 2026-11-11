#' Portfolio universes for optimisation
#'
#' The Tier 1 panel from [pm_data()] is the allocation spine, and it is small:
#' eight sleeves at the time of writing, of the twelve declared in [pm_sources()].
#' That is a comfortable number of assets, and comfortable numbers hide problems.
#' Several chapters need a larger cross-section: to make a covariance matrix
#' singular, to see a solver slow down, to give a group constraint something to
#' group.
#'
#' These universes come from Kenneth French, so they arrive through the same
#' loader as everything else, reach back to 1926, and need no credentials.
#'
#' @param which Which universe, one of `"industries10"`, `"industries49"` or
#'   `"size_bm100"`, matched by [base::match.arg()]. See Details for what each is for.
#'   Anything else is an error listing the three.
#' @param weighting `"value"` (the default) or `"equal"`. Selects Kenneth
#'   French's value-weighted or equal-weighted sub-table. The two are cached
#'   separately, so switching costs one more download and no confusion.
#' @param from,to Optional inclusive date bounds, as `Date` or anything
#'   [base::as.Date()] accepts. Applied after the cache is read, so narrowing the
#'   window never triggers a second download. `NULL` (the default) means no bound
#'   on that side.
#'
#' @return A tibble with a `date` column (`Date`, month-end, ascending) and one
#'   column per portfolio — 10, 49 or 100 of them — each a `numeric` vector of
#'   monthly simple returns in **decimal** form. French publishes percent; the
#'   conversion happens here, once, before caching.
#'
#'   All three start in 1926-07 and run to French's latest month. They are
#'   **not** rectangular: French's missing-value sentinels (`-99`, `-999`,
#'   `-99.99`) become `NA`, and a portfolio with no constituents in an early
#'   month is `NA` rather than dropped. Nine of the forty-nine industries begin
#'   after 1926 — the last, in 1969 — and thirty of the hundred size and
#'   book-to-market portfolios do. Filtering with `from` is your business, not
#'   this function's.
#'
#' @section Caching and the network:
#' The first call for a given `which` and `weighting` downloads through
#' \pkg{FFdownload} and writes
#' `universe_<which>_<weighting>.rds` into [pm_cache_dir()]. Every later call
#' reads that file and needs no network. Delete the file to force a refresh;
#' there is deliberately no `force` argument, because these portfolios are
#' revised far less often than the Tier 1 sleeves.
#'
#' Unlike [pm_data()], this function *will* reach the network on a cold cache.
#' That is the one place in the data layer where it happens implicitly, and it is
#' why a chapter should call it once at the top rather than inside a loop.
#'
#' @details
#' The point of offering a ladder rather than one universe is that **N relative
#' to T is the variable that matters**, and it is easiest to teach by moving it:
#'
#' \describe{
#'   \item{`"industries10"`}{Ten industries. Small enough that the sample
#'     covariance is always well behaved — the baseline where everything works.}
#'   \item{`"industries49"`}{Forty-nine industries. With a sixty-month rolling
#'     window this is N close to T, which is where shrinkage stops being
#'     optional. Columns are named `Agric`, `Food`, `Beer`, `Smoke`, `Guns`,
#'     `Coal`, `Oil` and so on, so an exclusion screen can be written from the
#'     column names alone.}
#'   \item{`"size_bm100"`}{One hundred size and book-to-market portfolios. With a
#'     sixty-month window N exceeds T, the sample covariance is singular, and
#'     `solve()` fails outright. That failure is the point.}
#' }
#'
#' @section Why not individual stocks:
#' Individual equities are the obvious choice for a large universe and the wrong
#' one for the book's spine. Yahoo Finance is undocumented, rate-limited and
#' revises without notice, so a book built on it stops reproducing the moment a
#' ticker changes. These portfolios have a real covariance structure, a
#' century of history, and no survivorship problem in the demonstration.
#'
#' Named stocks still earn their place where recognisability is worth more than
#' reproducibility — showing what a trade costs, or what a sleeve actually holds.
#' They are illustration, not foundation, and the book says which is which.
#'
#' @examples
#' \dontrun{
#' # Downloads on the first call for each (which, weighting) pair, then caches.
#' ind <- pm_universe("industries10", from = "1990-07-01")
#' dim(ind)
#'
#' # N close to T on a sixty-month window: where shrinkage stops being optional.
#' ind49 <- pm_universe("industries49", from = "1990-07-01")
#'
#' # The regime where the sample covariance gives up. Its smallest eigenvalue is
#' # negative, so the estimate is not even positive semi-definite.
#' big <- pm_universe("size_bm100", from = "2020-01-01")
#' dim(big)
#' min(eigen(stats::cov(big[, -1]), only.values = TRUE)$values)
#'
#' # Equal weighting is a separate download and a separate cache file.
#' pm_universe("industries10", weighting = "equal", from = "2000-01-01")
#' }
#'
#' # Argument checking happens before anything is fetched.
#' try(pm_universe("industries20"))
#'
#' @family tidypm data layer
#' @seealso [pm_screen()] for exclusion sets expressed in these column names,
#'   [pm_data()] for the Tier 1 allocation spine.
#' @export
pm_universe <- function(which = c("industries10", "industries49", "size_bm100"),
                        weighting = c("value", "equal"),
                        from = NULL, to = NULL) {

  which <- match.arg(which)
  weighting <- match.arg(weighting)

  dataset <- switch(
    which,
    industries10 = "10_Industry_Portfolios",
    industries49 = "49_Industry_Portfolios",
    size_bm100   = "100_Portfolios_10x10"
  )

  subtable <- switch(
    weighting,
    value = "average_value_weighted_returns",
    equal = "average_equal_weighted_returns"
  )

  path <- file.path(pm_cache_dir(), paste0("universe_", which, "_", weighting, ".rds"))

  if (file.exists(path)) {
    out <- readRDS(path)
  } else {
    rlang::check_installed("FFdownload", "to download Kenneth French's data.")
    cli::cli_alert("Downloading {.val {dataset}} ({weighting}-weighted). Cached afterwards.")

    res <- FFdownload::FFget(
      dataset,
      frequency = "monthly",
      subtable  = subtable,
      na_values = c(-99, -999, -99.99),
      format    = "tbl"
    )
    if (is.list(res) && !is.data.frame(res)) res <- res[[1]]

    out <- tibble::as_tibble(res)
    names(out)[1] <- "date"
    out$date <- zoo::as.Date(zoo::as.yearmon(out$date), frac = 1)

    # French reports percent throughout; the package is decimal throughout.
    num <- setdiff(names(out), "date")
    out[num] <- lapply(out[num], function(x) x / 100)

    saveRDS(out, path)
  }

  # The @return says "ascending", so make it so rather than inherit it from the
  # parser. Applied after the cache read as well as after a download, so a file
  # written by an older version is normalised on the way out.
  out <- out[order(out$date), , drop = FALSE]

  if (!is.null(from)) out <- out[out$date >= as.Date(from), ]
  if (!is.null(to))   out <- out[out$date <= as.Date(to), ]
  out
}


#' Industries that a typical exclusion screen removes
#'
#' The forty-nine-industry universe names its columns, so a sustainability screen
#' can be expressed as a set of column names rather than a mapping from SIC
#' codes. This returns the usual suspects for three common screen types.
#'
#' @param screen Character vector, any combination of `"sin"`, `"fossil"` and
#'   `"weapons"`, matched by [base::match.arg()] with `several.ok = TRUE`. The default
#'   is all three, so a bare `pm_screen()` returns the union — the widest screen,
#'   not an empty one. Anything else is an error naming the three.
#'
#' @return An unnamed `character` vector of column names, every one of which is
#'   present in `pm_universe("industries49")` — verified, so a screen never
#'   silently removes nothing. Lengths are 4, 3, 3 and 9 for `"sin"`,
#'   `"fossil"`, `"weapons"` and the union.
#'
#'   Names appear in the order the screens were requested, de-duplicated by first
#'   appearance: `"Guns"` is in both `"sin"` and `"weapons"` and is returned once.
#'   The order therefore *does* depend on the order of `screen`, so compare two
#'   screens with [base::setequal()] rather than [base::identical()].
#'
#'   The result is a set of column names, so it composes directly:
#'   `universe[, setdiff(names(universe), pm_screen("fossil"))]`.
#'
#' @details
#' The three sets are
#'
#' \describe{
#'   \item{`"sin"`}{`Smoke`, `Beer`, `Guns`, `Fun` — tobacco, alcohol, firearms
#'     and gambling, the classical sin set.}
#'   \item{`"fossil"`}{`Coal`, `Oil`, `Mines`. `Mines` is arguable and included:
#'     an extraction screen that stops at hydrocarbons is a choice, not an
#'     oversight.}
#'   \item{`"weapons"`}{`Guns`, `Aero`, `Ships` — defence, which in this
#'     classification cannot be separated from civil aerospace and shipbuilding.
#'     That imprecision is the honest cost of an industry-level screen.}
#' }
#'
#' These are conventions, not standards. Every index provider draws the lines
#' differently, and the point of chapter 10 is to measure what the line costs
#' rather than to argue about where it belongs.
#'
#' @examples
#' pm_screen("sin")
#' pm_screen(c("fossil", "weapons"))
#'
#' # No argument is the widest screen, not the narrowest.
#' pm_screen()
#'
#' # Guns is in both "sin" and "weapons" and is returned once. The set is the
#' # same either way round; the order is not.
#' setequal(pm_screen(c("weapons", "sin")), pm_screen(c("sin", "weapons")))
#' identical(pm_screen(c("weapons", "sin")), pm_screen(c("sin", "weapons")))
#'
#' try(pm_screen("carbon"))
#'
#' \dontrun{
#' ind <- pm_universe("industries49", from = "1990-07-01")
#' screened <- ind[, setdiff(names(ind), pm_screen("fossil"))]
#' ncol(ind) - ncol(screened)
#' }
#'
#' @family tidypm data layer
#' @seealso [pm_universe()], whose `"industries49"` column names these refer to.
#' @export
pm_screen <- function(screen = c("sin", "fossil", "weapons")) {
  screen <- match.arg(screen, several.ok = TRUE)

  sets <- list(
    sin     = c("Smoke", "Beer", "Guns", "Fun"),
    fossil  = c("Coal", "Oil", "Mines"),
    weapons = c("Guns", "Aero", "Ships")
  )

  unique(unlist(sets[screen], use.names = FALSE))
}
