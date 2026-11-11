#' The sleeve registry
#'
#' One entry per Tier 1 sleeve: where it comes from, and the function that builds
#' it. Everything about the panel's composition is here rather than scattered
#' through [pm_init()], so a reader can see the whole universe at a glance and
#' change one line to swap a source.
#'
#' @return A named list, one element per sleeve, the name being the column the
#'   sleeve occupies in [pm_data()]. Each element is itself a list with:
#'
#'   \describe{
#'     \item{`description`}{Character. One line naming the source, as printed by
#'       [pm_init()] while it builds.}
#'     \item{`implemented`}{Logical. `TRUE` if a fetch function exists. Sleeves
#'       with `FALSE` are declared so the gap is visible, are skipped by
#'       [pm_init()], and do not become columns of the panel.}
#'     \item{`fetch`}{A function of no arguments returning a two-column tibble,
#'       `date` and the sleeve, monthly simple returns in decimal form. Present
#'       only when `implemented` is `TRUE`. **Calling it downloads.**}
#'   }
#'
#'   The registry is data, not state: calling `pm_sources()` fetches nothing and
#'   costs nothing.
#'
#' @section Adding a sleeve:
#' Write a fetch function returning `date` plus one decimal-return column named
#' after the sleeve, add an entry here, and set `implemented = TRUE`. That is the
#' whole change; [pm_init()] picks it up and [pm_data()] gains a column. A sleeve
#' the book has decided not to build stays in the list with `implemented = FALSE`
#' and a comment saying where the data would come from and what it would cost, so
#' that the gap is a visible entry rather than an absence a reader has to notice.
#'
#' @examples
#' src <- pm_sources()
#'
#' # What the panel is made of, and what is still missing.
#' vapply(src, function(s) isTRUE(s$implemented), logical(1))
#'
#' # The registry is inspectable without touching the network.
#' vapply(src, function(s) s$description, character(1))[["gov_long"]]
#'
#' @family tidypm data layer
#' @seealso [pm_init()], which walks this list, and [bond_total_return()], which
#'   builds the two constructed sleeves.
#' @export
pm_sources <- function() {
  list(
    us_equity = list(
      description = "US equity, Kenneth French market return",
      implemented = TRUE,
      fetch = function() ff_market()
    ),
    dev_ex_us_equity = list(
      description = "Developed ex-US equity, Kenneth French",
      implemented = TRUE,
      fetch = function() ff_region("Developed_ex_US_3_Factors", "dev_ex_us_equity")
    ),
    em_equity = list(
      description = "Emerging equity, Kenneth French",
      implemented = TRUE,
      fetch = function() ff_region("Emerging_5_Factors", "em_equity")
    ),
    cash = list(
      description = "Cash, one-month T-bill (French RF)",
      implemented = TRUE,
      fetch = function() ff_cash()
    ),
    gov_short = list(
      description = "Government short, constructed from FRED DGS1",
      implemented = TRUE,
      fetch = function() fred_bond("DGS1", maturity = 1, name = "gov_short")
    ),
    gov_long = list(
      description = "Government long, constructed from FRED DGS10",
      implemented = TRUE,
      fetch = function() fred_bond("DGS10", maturity = 10, name = "gov_long")
    ),
    ig_credit = list(
      description = "Investment-grade credit, constructed from Moody's BAA",
      implemented = TRUE,
      fetch = function() fred_credit("BAA", name = "ig_credit")
    ),

    high_yield = list(
      description = "High yield, Vanguard VWEHX fund proxy (spliced from 1985)",
      implemented = TRUE,
      fetch = function() yahoo_fund("VWEHX", "high_yield")
    ),

    # Not built, and not blocking. Declared so the gap is visible rather than
    # implied. Adding one is a fetch function and a TRUE -- see the notes on
    # each for where the data would come from.
    #
    # commodities: no free source reaches 1990. AQR's long-run index does but
    #   costs an xlsx parser, an excess-return adjustment and a dependency on a
    #   corporate file we do not control. The practical alternative is DBC via
    #   yahoo_fund() from 2006, which needs no new machinery.
    commodities       = list(description = "Commodities, no free source back to 1990",
                             implemented = FALSE),
    real_estate       = list(description = "Listed real estate, FTSE Nareit",  implemented = FALSE),
    dev_ex_us_hedged  = list(description = "Developed ex-US, currency hedged",  implemented = FALSE),
    em_hedged         = list(description = "Emerging, currency hedged",         implemented = FALSE)
  )
}


# --- Kenneth French, via FFdownload -------------------------------------------
# One call per dataset. FFget() downloads and parses in one step, so there is no
# intermediate RData file to manage; our own cache is the caching layer.

# Several sleeves come out of the same French file -- the US market return and
# the cash sleeve both live in F-F_Research_Data_Factors. Without this, one
# pm_init() downloads and parses that file twice for no reason.
.ff_cache <- new.env(parent = emptyenv())

ff_get_monthly <- function(dataset) {
  rlang::check_installed("FFdownload", "to download Kenneth French's data.")

  if (!is.null(.ff_cache[[dataset]])) return(.ff_cache[[dataset]])

  res <- FFdownload::FFget(
    dataset,
    frequency = "monthly",
    subtable  = "Temp2",
    na_values = c(-99, -999, -99.99),
    format    = "tbl"
  )
  if (is.list(res) && !is.data.frame(res)) res <- res[[1]]

  out <- tibble::as_tibble(res)
  names(out)[1] <- "date"
  # French dates are month labels; anchor them to the month end.
  out$date <- zoo::as.Date(zoo::as.yearmon(out$date), frac = 1)

  # Ascending, explicitly. Everything downstream -- the full joins in pm_init(),
  # the leading-NA convention in bond_total_return(), a reader taking diff() of a
  # column -- reads this as a time series, and none of them check. The parser
  # currently returns the file in order; that is an observation about FFdownload,
  # not a contract.
  out <- out[order(out$date), , drop = FALSE]

  .ff_cache[[dataset]] <- out
  out
}

ff_market <- function() {
  d <- ff_get_monthly("F-F_Research_Data_Factors")
  tibble::tibble(
    date = d$date,
    # French reports percent; the panel is decimal throughout.
    us_equity = (d[["Mkt.RF"]] + d[["RF"]]) / 100
  )
}

ff_cash <- function() {
  d <- ff_get_monthly("F-F_Research_Data_Factors")
  tibble::tibble(date = d$date, cash = d[["RF"]] / 100)
}

ff_region <- function(dataset, name) {
  d <- ff_get_monthly(dataset)
  out <- tibble::tibble(date = d$date, x = (d[["Mkt.RF"]] + d[["RF"]]) / 100)
  names(out)[2] <- name
  out
}


# --- FRED ---------------------------------------------------------------------
# The public CSV endpoint needs no API key, which keeps the book credential-free.

fred_series <- function(id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id,
                "&cosd=1950-01-01")
  d <- utils::read.csv(url, stringsAsFactors = FALSE)
  names(d) <- c("date", "value")
  d$date <- as.Date(d$date)
  d$value <- suppressWarnings(as.numeric(d$value))
  tibble::as_tibble(d[!is.na(d$value), ])
}

# Month-end, not monthly average. The GS-prefixed series average daily yields
# within the month, which smooths away intra-month moves and understated both
# the 2022 drawdown and the 2023 recovery. See data/README.md in the book.
fred_month_end <- function(id) {
  d <- fred_series(id)
  # Sort before deduplicating, and do not rely on the endpoint for it.
  # `!duplicated(ym, fromLast = TRUE)` keeps the LAST ROW of each month, which is
  # the last OBSERVATION of that month only if the rows already arrive in date
  # order. FRED currently serves them sorted, so this line changes no number
  # today; the day it does not, the sleeve would silently be built from
  # mid-month yields and nothing downstream would notice. A shuffled March in
  # the test file moves a ten-year bond month from +3.6% to -0.5%.
  d <- d[order(d$date), , drop = FALSE]
  d$ym <- format(d$date, "%Y-%m")
  d <- d[!duplicated(d$ym, fromLast = TRUE), ]
  d$date <- zoo::as.Date(zoo::as.yearmon(d$date), frac = 1)
  d[, c("date", "value")]
}

fred_bond <- function(id, maturity, name) {
  d <- fred_month_end(id)
  out <- tibble::tibble(
    date = d$date,
    x    = bond_total_return(d$value / 100, maturity = maturity)
  )
  names(out)[2] <- name
  out[!is.na(out[[2]]), ]
}

# --- Yahoo, used once and reluctantly ------------------------------------------
# High yield is the one sleeve with no free index history. Every ICE BofA series
# on FRED -- total return and spread alike -- is now served on a rolling
# three-year window, which is stated on the series page itself, not merely a
# quirk of the CSV endpoint.
#
# A long-running mutual fund is the remaining free option. Two honest caveats,
# both repeated in chapter 2 rather than buried here:
#
#  1. VWEHX is a managed fund holding higher-quality high yield, so it
#     UNDERSTATES drawdowns -- 2008 gives -21.2% against roughly -26% for the
#     index. That is the wrong direction for the one sleeve whose reason to exist
#     is its left tail. The size of the bias is measured in the chapter.
#  2. Yahoo is undocumented and could disappear. Our cache protects anyone who
#     has already run pm_init(); it does not protect a reader in ten years.
#
# The data is deliberately NOT redistributed to hedge against (2). That would
# solve the fragility by creating a licensing question, which is the trade the
# other books made and this one does not.
yahoo_fund <- function(ticker, name, from = "1980-01-01") {
  rlang::check_installed("tidyquant", "to download fund prices.")

  d <- tidyquant::tq_get(ticker, from = from)
  d <- d[!is.na(d$adjusted), c("date", "adjusted")]

  # Sort first. Both steps below assume date order: the deduplication keeps the
  # last row of each month rather than the last observation, and the return is a
  # ratio of adjacent rows. Yahoo is undocumented, so its row order is not a
  # guarantee we are entitled to rely on.
  d <- d[order(d$date), , drop = FALSE]

  # Month-end observations, then simple returns. `adjusted` includes
  # distributions, which for a bond fund is most of the return.
  d$ym <- format(d$date, "%Y-%m")
  d <- d[!duplicated(d$ym, fromLast = TRUE), ]

  out <- tibble::tibble(
    date = zoo::as.Date(zoo::as.yearmon(d$date), frac = 1),
    x    = c(NA_real_, d$adjusted[-1] / d$adjusted[-nrow(d)] - 1)
  )
  names(out)[2] <- name
  out[!is.na(out[[2]]), ]
}

fred_credit <- function(id, name, maturity = 20, annual_loss = 0.004) {
  d <- fred_month_end(id)
  out <- tibble::tibble(
    date = d$date,
    x    = credit_total_return(d$value / 100, maturity = maturity,
                               annual_loss = annual_loss)
  )
  names(out)[2] <- name
  out[!is.na(out[[2]]), ]
}
