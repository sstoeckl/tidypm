#' The model calendar of the modelled market
#'
#' The market shipped with this package is modelled, not recorded, and it runs
#' on its own calendar: thirty-one **model years**, written as the calendar
#' years 2101 to 2131 so that every date is an ordinary `Date` that `xts`,
#' `PerformanceAnalytics` and `PortfolioAnalytics` handle without special
#' cases, and so that no model month can be mistaken for a historical one.
#' `model_year()` turns a date into its model year, and `in_episode()` selects
#' the dates that fall inside one of the named episodes of [sim_episodes].
#'
#' @param date A `Date` vector, or anything [base::as.Date()] accepts, on the model
#'   calendar. For `in_episode()` typically `zoo::index()` of a return matrix.
#' @param episode Character scalar, one of the keys in `sim_episodes$episode`:
#'   `"equity_bear"`, `"credit_crisis"`, `"liquidity_shock"` or `"rates_shock"`.
#'
#' @return `model_year()` returns an integer vector the length of `date`: 1 for
#'   the first year of the sample, 31 for the last. Dates before the sample
#'   (the capitalisations reach back further) give zero or negative years.
#'   `in_episode()` returns a logical vector the length of `date`, `TRUE` where
#'   the date lies in the closed interval of the episode.
#'
#' @section Why a model calendar:
#' The factor models behind the market were estimated on market data and then
#' replayed with fresh idiosyncratic components, so the series are a simulation
#' that has the statistical properties of a history without being one. Dating
#' them with historical dates would invite exactly the reading the data cannot
#' support: that a given month's return is what some market did in that month.
#' On the model calendar an episode is identified by what happens in it, which
#' is also how a portfolio meets one.
#'
#' @examples
#' d <- sort(unique(sim_class_returns_monthly$date))
#' range(model_year(d))
#'
#' # The months of the credit crisis, and what US equity did in them.
#' us <- subset(sim_class_returns_monthly, class == "US_equity")
#' crisis <- in_episode(us$date, "credit_crisis")
#' sum(crisis)
#' prod(1 + us$return[crisis]) - 1
#'
#' sim_episodes[, c("episode", "label", "start", "end")]
#'
#' @name model-calendar
NULL

# Model year = calendar year minus this constant. Kept in one place.
.model_year0 <- 2100L

#' @rdname model-calendar
#' @export
model_year <- function(date) {
  d <- tryCatch(as.Date(date), error = function(e) {
    cli::cli_abort(c(
      "{.arg date} must be a date on the model calendar.",
      "i" = "Pass a {.cls Date}, for example {.code zoo::index(R)}.",
      "x" = conditionMessage(e)
    ))
  })
  as.integer(format(d, "%Y")) - .model_year0
}

#' @rdname model-calendar
#' @export
in_episode <- function(date, episode) {
  ep <- get("sim_episodes", envir = asNamespace("tidypm"))
  if (!is.character(episode) || length(episode) != 1L || !episode %in% ep$episode) {
    cli::cli_abort(c(
      "{.arg episode} must be one episode key.",
      "i" = "Available: {.val {ep$episode}}.",
      "x" = "Got {.val {episode}}."
    ))
  }
  d <- as.Date(date)
  i <- match(episode, ep$episode)
  d >= ep$start[i] & d <= ep$end[i]
}
