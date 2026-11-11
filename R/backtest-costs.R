#' Default one-way proportional trading costs, by asset class
#'
#' A flat cost per unit traded is the wrong model for a multi-asset book. The
#' spread on a ten-year Treasury and the spread on a high-yield bond differ by
#' more than an order of magnitude, and a rebalancing rule that looks cheap under
#' a flat 10 basis points can be uneconomic once the credit leg is priced
#' properly. `pm_costs_default()` returns the book's default schedule so that a
#' chapter can vary one number and see what moves, rather than inheriting a
#' constant.
#'
#' The numbers are **one-way** proportional costs in decimal: `0.0025` is 25
#' basis points paid on each unit bought and each unit sold, so a full round trip
#' costs twice the figure below. They are deliberately round. Realised costs vary
#' by decade, size and venue by more than the difference between any two adjacent
#' rows here; the schedule exists to get the *ordering* and the *order of
#' magnitude* right, not to be precise.
#'
#' @param ... Named numeric overrides, e.g. `credit = 0.004`. Names are matched
#'   against the defaults; an unrecognised name is added rather than rejected, so
#'   that a chapter working at instrument level can add a per-instrument entry.
#'
#' @return A named numeric vector of one-way proportional costs in decimal, with
#'   the elements `government`, `money_market`, `commodity`, `equity`, `credit`
#'   and `.default`, plus any names passed through `...`. The `.default` element
#'   is what [pm_backtest()] charges an asset that matches nothing else; set it
#'   to `NA` to make an unmatched asset an error instead.
#'
#' @section How a cost is matched to an asset:
#' [pm_backtest()] resolves the schedule against the column names of the return
#' matrix, in this order:
#'
#' 1. an exact match on the column name, so `US_equity = 0.0004` prices one
#'    series on its own;
#' 2. a match on a trailing, underscore-delimited token, case-insensitively, so
#'    `equity` prices `US_equity`, `EU_equity` and the rest, and `money_market`
#'    prices `US_money_market`;
#' 3. the `.default` element.
#'
#' Where several names match, the longest wins, so `money_market` beats `market`.
#' The class-level universe of the simulated market is named
#' `<REGION>_<asset_class>` and resolves entirely at step 2. The instrument-level
#' universe is named `US_EQU_01` and does **not**: build a per-asset vector from
#' [sim_assets] first, as shown in the examples.
#'
#' @section The ordering, and where it comes from:
#' Government bonds are the cheapest thing in the book, money market cheaper
#' still; commodity futures are cheap; cash equities sit in the middle; corporate
#' credit is the dearest by a wide margin because it trades over the counter in
#' size, not on an exchange.
#'
#' @source Rounded from the following, and intended to be varied rather than
#'   believed:
#'
#'   * **Government, 2 bp; money market, 1 bp.** Fleming, M. J. (2003),
#'     "Measuring Treasury Market Liquidity", *FRBNY Economic Policy Review* 9(3),
#'     83–108, which reports on-the-run coupon bid-ask spreads of roughly
#'     0.4 to 1.6 32nds, i.e. under 2 bp of price; bills tighter again.
#'   * **Commodity, 8 bp.** Frazzini, A., R. Israel and T. J. Moskowitz (2018),
#'     "Trading Costs", SSRN 3229719, which reports market-impact-inclusive costs
#'     for liquid futures below 10 bp.
#'   * **Equity, 10 bp.** Frazzini, Israel and Moskowitz (2018) again: realised
#'     one-way costs on developed-market equities of roughly 5 to 20 bp for a
#'     large manager, well above quoted half-spreads and well below the
#'     effective-spread estimates of Hasbrouck (2009).
#'   * **Credit, 25 bp.** Bessembinder, H., S. Jacobsen, W. Maxwell and
#'     K. Venkataraman (2018), "Capital Commitment and Illiquidity in Corporate
#'     Bonds", *Journal of Finance* 73(4), 1615–1661, which reports one-way
#'     institutional costs falling from about 50 bp pre-crisis to about 25 bp
#'     after; and Chen, L., D. A. Lesmond and J. Wei (2007), "Corporate Yield
#'     Spreads and Bond Liquidity", *Journal of Finance* 62(1), 119–149, for the
#'     investment-grade / high-yield gap.
#'
#' @examples
#' pm_costs_default()
#'
#' # Vary one number: credit at 40 bp rather than 25.
#' pm_costs_default(credit = 0.004)
#'
#' # Price one series on its own; the exact name wins over the class token.
#' pm_costs_default(US_government = 0.0001)
#'
#' # Instrument level: build a per-asset vector from the asset table, because
#' # `US_EQU_01` does not contain the token "equity".
#' cls <- pm_costs_default()
#' per_asset <- stats::setNames(
#'   unname(cls[sim_assets$asset_class]), sim_assets$asset_id
#' )
#' utils::head(per_asset)
#'
#' @seealso [pm_backtest()], which takes this as its `costs` argument.
#' @export
pm_costs_default <- function(...) {
  base <- c(
    money_market = 0.0001,
    government   = 0.0002,
    commodity    = 0.0008,
    equity       = 0.0010,
    credit       = 0.0025,
    .default     = 0.0010
  )

  extra <- list(...)
  if (length(extra) == 0L) return(base)

  nms <- names(extra)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(c(
      "Every override passed to {.fn pm_costs_default} must be named.",
      "x" = "At least one argument had no name.",
      ">" = "Write {.code pm_costs_default(credit = 0.004)}, not
             {.code pm_costs_default(0.004)}."
    ))
  }

  bad <- names(extra)[!vapply(extra, function(z) {
    is.numeric(z) && length(z) == 1L && (is.na(z) || z >= 0)
  }, logical(1))]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Trading costs must be single non-negative numbers.",
      "x" = "{.arg {bad}} {?is/are} not.",
      ">" = "Costs are decimals, not basis points: 25 bp is {.val {0.0025}}."
    ))
  }

  base[names(extra)] <- unlist(extra, use.names = FALSE)
  base
}


# Resolve a class-level cost schedule against the columns of a return matrix.
# Returns a named numeric vector, one element per asset, in the order of
# `assets`. Errors rather than guessing when an asset matches nothing and there
# is no usable `.default`.
pm_resolve_costs <- function(assets, costs) {
  if (!is.numeric(costs) || is.null(names(costs)) || any(!nzchar(names(costs)))) {
    cli::cli_abort(c(
      "{.arg costs} must be a fully named numeric vector.",
      "x" = "You supplied {.obj_type_friendly {costs}}.",
      ">" = "Start from {.fn pm_costs_default} and override what you need."
    ))
  }
  if (any(!is.na(costs) & costs < 0)) {
    cli::cli_abort(c(
      "Trading costs cannot be negative.",
      "x" = "{.field {names(costs)[!is.na(costs) & costs < 0]}} {?is/are} below zero.",
      ">" = "A rebate belongs in the return series, not in the cost schedule."
    ))
  }

  keys    <- setdiff(names(costs), ".default")
  fallback <- if (".default" %in% names(costs)) unname(costs[[".default"]]) else NA_real_

  out <- vapply(assets, function(a) {
    if (a %in% keys) return(unname(costs[[a]]))
    hit <- keys[vapply(keys, function(k) {
      grepl(paste0("(^|_)", k, "$"), a, ignore.case = TRUE)
    }, logical(1))]
    if (length(hit) == 0L) return(NA_real_)
    unname(costs[[hit[which.max(nchar(hit))]]])
  }, numeric(1), USE.NAMES = FALSE)

  unmatched <- is.na(out)
  if (any(unmatched)) {
    if (is.na(fallback)) {
      cli::cli_abort(c(
        "No trading cost for {sum(unmatched)} asset{?s}.",
        "x" = "{.val {utils::head(assets[unmatched], 5L)}} match{?es/} no name in
               {.arg costs} and {.code costs[['.default']]} is {.val NA}.",
        "i" = "Names in {.arg costs}: {.val {keys}}.",
        ">" = "Add an entry for the asset or its class, or set
               {.code costs['.default']} to a number."
      ))
    }
    out[unmatched] <- fallback
  }

  stats::setNames(out, assets)
}
