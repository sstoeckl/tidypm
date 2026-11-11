# The modelled market ships on a model calendar: thirty-one model years,
# written 2101 to 2131. These tests guard the calendar itself, the episode
# table that every chapter takes its windows from, and the two helpers.

test_that("the sample runs over model years 1 to 31 and nowhere else", {
  d <- sort(unique(sim_class_returns_monthly$date))
  expect_s3_class(d, "Date")
  expect_identical(range(model_year(d)), c(1L, 31L))
  expect_equal(as.integer(table(model_year(d))), rep(12L, 31L))
  # No return table carries a date outside the model calendar.
  for (x in list(sim_class_returns_weekly, sim_money_market_monthly,
                 sim_asset_returns_monthly, sim_asset_returns_weekly,
                 sim_fx_monthly, sim_riskfree_monthly)) {
    expect_true(all(model_year(x$date) %in% 1:31))
  }
})

test_that("monthly dates are month ends and weekly dates stay in their month", {
  d <- sort(unique(sim_class_returns_monthly$date))
  expect_true(all(format(d + 1, "%d") == "01"))
  expect_false(is.unsorted(d, strictly = TRUE))

  # Compounding the weekly class returns inside each month reproduces the
  # monthly table. This is the property a chapter relies on when it aggregates
  # a weekly series itself.
  wk <- sim_class_returns_weekly
  wk$ym <- format(wk$date, "%Y-%m")
  agg <- stats::aggregate(return ~ class + ym, data = wk,
                          FUN = function(r) prod(1 + r) - 1)
  mo <- sim_class_returns_monthly
  mo$ym <- format(mo$date, "%Y-%m")
  both <- merge(agg, mo, by = c("class", "ym"), suffixes = c("_weekly", "_monthly"))
  expect_equal(nrow(both), nrow(mo))
  expect_equal(both$return_weekly, both$return_monthly, tolerance = 1e-10)
})

test_that("weekly dates are a week apart", {
  d <- sort(unique(sim_class_returns_weekly$date))
  expect_false(is.unsorted(d, strictly = TRUE))
  # Seven days, except where a leap day falls on one calendar and not the other.
  expect_true(all(as.integer(diff(d)) %in% 6:8))
  expect_gt(mean(as.integer(diff(d)) == 7L), 0.98)
})

test_that("the episode table is complete and inside the sample", {
  expect_s3_class(sim_episodes, "tbl_df")
  expect_named(sim_episodes, c("episode", "label", "start", "end", "description"))
  expect_setequal(sim_episodes$episode,
                  c("equity_bear", "credit_crisis", "liquidity_shock", "rates_shock"))
  expect_equal(sum(duplicated(sim_episodes$episode)), 0L)
  expect_true(all(sim_episodes$start < sim_episodes$end))
  expect_true(all(model_year(sim_episodes$start) %in% 1:31))
  expect_true(all(model_year(sim_episodes$end) %in% 1:31))
  expect_false(is.unsorted(sim_episodes$start, strictly = TRUE))
  # Episodes are described by what happens, never by a historical name or year.
  txt <- paste(sim_episodes$label, sim_episodes$description)
  expect_false(any(grepl("19[0-9]{2}|20[0-9]{2}", txt)))
})

test_that("the episodes are what their descriptions say they are", {
  cum <- function(series, episode) {
    x <- sim_class_returns_monthly[sim_class_returns_monthly$class == series, ]
    prod(1 + x$return[in_episode(x$date, episode)]) - 1
  }
  # Credit crisis: flight to quality.
  expect_lt(cum("US_equity", "credit_crisis"), -0.20)
  expect_lt(cum("US_credit", "credit_crisis"), 0)
  expect_gt(cum("US_government", "credit_crisis"), 0)
  # Rates shock: bonds and equities fall together, commodities gain.
  expect_lt(cum("US_government", "rates_shock"), 0)
  expect_lt(cum("US_equity", "rates_shock"), 0)
  expect_gt(cum("GLOBAL_commodity", "rates_shock"), 0)
  # Equity bear market: equities down, government bonds up.
  expect_lt(cum("US_equity", "equity_bear"), -0.20)
  expect_gt(cum("US_government", "equity_bear"), 0)
})

test_that("model_year() and in_episode() validate their input", {
  expect_identical(model_year(as.Date("2117-09-30")), 17L)
  expect_identical(model_year("2101-01-31"), 1L)
  expect_error(model_year("not a date"), "model calendar")

  d <- sort(unique(sim_class_returns_monthly$date))
  hit <- in_episode(d, "credit_crisis")
  expect_type(hit, "logical")
  expect_length(hit, length(d))
  expect_equal(sum(hit), 7L)
  expect_error(in_episode(d, "great_depression"), "episode")
  expect_error(in_episode(d, c("credit_crisis", "rates_shock")), "episode")
})

test_that("the ESG changes sit on the model calendar and on equities only", {
  expect_named(sim_esg_changes_quarterly, c("date", "asset", "esg_change"))
  expect_s3_class(sim_esg_changes_quarterly$date, "Date")
  expect_type(sim_esg_changes_quarterly$esg_change, "integer")
  expect_true(all(model_year(sim_esg_changes_quarterly$date) %in% 1:31))
  expect_true(all(format(sim_esg_changes_quarterly$date + 1, "%d") == "01"))
  eq <- sim_assets$asset_id[sim_assets$asset_class == "equity"]
  expect_true(all(sim_esg_changes_quarterly$asset %in% eq))
  expect_false(anyNA(sim_esg_changes_quarterly$esg_change))
})
