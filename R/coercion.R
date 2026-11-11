#' Move between tidy returns and the wide `xts` matrix `PortfolioAnalytics` wants
#'
#' `PortfolioAnalytics` is `xts`-only end to end: every optimisation function
#' expects a wide matrix of returns indexed by date, one column per asset. Tidy
#' data is long: one row per asset per date. These two functions are the boundary,
#' and the book crosses it explicitly rather than by accident.
#'
#' @param data A data frame or tibble of returns in long format: one row per
#'   asset per date. Each `(date, asset)` pair must appear at most once — see
#'   Details for what happens when it does not.
#' @param date,asset,value <[`data-masking`][rlang::args_data_masking]> Columns
#'   of `data` holding, respectively, the date, the asset identifier, and the
#'   return. Bare column names, defaulting to `date`, `asset` and `value`. Only
#'   these three columns are carried across; anything else in `data` is dropped.
#' @param x An `xts` object, typically the output of `as_xts_pm()`, the return
#'   matrix a chapter was built from, or the weights returned by an optimisation.
#'
#' @return
#' `as_xts_pm()` returns an `xts` object: a numeric matrix with `nrow` equal to
#' the number of distinct dates in `data`, one column per distinct value of
#' `asset`, and an index of the same class as the `date` column (`Date` in and
#' `Date` out). Rows are sorted by date. Columns are **not** sorted; see Details.
#' Cells with no matching row in `data` are `NA`, with a warning.
#'
#' `as_pm()` returns a tibble with `nrow(x) * ncol(x)` rows and exactly three
#' columns: `date` (the index of `x`, class preserved), `asset` (character, the
#' column names of `x`) and `value` (the cell). Rows run date-major: all assets
#' for the first date, then all assets for the second.
#'
#' @details
#' Three failure modes are worth knowing about, because the first two produce an
#' object that looks correct and optimises to nonsense:
#'
#' * **Ragged panels.** If an asset is missing for some dates, `as_xts_pm()` pads
#'   with `NA` and warns. `PortfolioAnalytics` will not warn; the covariance
#'   estimate silently changes its effective sample. Handle the gaps before
#'   crossing the boundary, not after.
#' * **Column order.** Weight vectors returned by the optimiser are ordered by the
#'   columns of the input matrix, and those columns come out in **order of first
#'   appearance in `data`**, not alphabetically. Sorting `data` differently
#'   therefore produces a different column order and a different weight vector
#'   ordering, from the same numbers. Fix the order once, before the boundary, and
#'   never reorder between fitting and applying weights.
#' * **Duplicate keys.** If a `(date, asset)` pair appears twice, the pivot
#'   produces a list-column and the call fails in `xts()` with
#'   `"cannot convert lists to xts objects"`. The message names the symptom, not
#'   the cause; check for duplicates first.
#'
#' @examples
#' # Month-end dates, written out rather than generated: `seq(by = "month")`
#' # from a 31st rolls into the following month.
#' returns <- data.frame(
#'   date  = rep(as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30")),
#'               each = 2),
#'   asset = rep(c("equity", "bonds"), times = 4),
#'   ret   = c(0.02, 0.01, -0.01, 0.00, 0.03, 0.01, 0.01, -0.02)
#' )
#'
#' r <- as_xts_pm(returns, date, asset, ret)
#' r
#'
#' as_pm(r)
#'
#' # Column order follows first appearance in the input, not the alphabet.
#' colnames(as_xts_pm(returns, date, asset, ret))
#' colnames(as_xts_pm(returns[order(returns$asset), ], date, asset, ret))
#'
#' # A gap is padded and announced, never filled.
#' gappy <- as_xts_pm(returns[-3, ], date, asset, ret)
#' gappy
#'
#' @seealso [pm_data()] for the panel these are usually applied to.
#' @name pm-coercion
NULL

#' @rdname pm-coercion
#' @export
as_xts_pm <- function(data, date = date, asset = asset, value = value) {
  date_q  <- rlang::enquo(date)
  asset_q <- rlang::enquo(asset)
  value_q <- rlang::enquo(value)

  wide <- data |>
    dplyr::select(!!date_q, !!asset_q, !!value_q) |>
    tidyr::pivot_wider(
      names_from  = !!asset_q,
      values_from = !!value_q
    ) |>
    dplyr::arrange(!!date_q)

  idx <- wide[[rlang::as_name(date_q)]]
  mat <- as.matrix(wide[, setdiff(names(wide), rlang::as_name(date_q)), drop = FALSE])

  n_missing <- sum(is.na(mat))
  if (n_missing > 0) {
    cli::cli_warn(c(
      "Padded {n_missing} missing return{?s} with {.val NA}.",
      "i" = "{.pkg PortfolioAnalytics} will not warn about these, but they change
             the effective sample of every moment estimate.",
      ">" = "Handle the gaps before crossing the boundary."
    ))
  }

  xts::xts(mat, order.by = idx)
}

#' @rdname pm-coercion
#' @export
as_pm <- function(x) {
  stopifnot(xts::is.xts(x))

  tibble::as_tibble(zoo::coredata(x)) |>
    dplyr::mutate(date = zoo::index(x), .before = 1) |>
    tidyr::pivot_longer(
      cols      = -"date",
      names_to  = "asset",
      values_to = "value"
    )
}
