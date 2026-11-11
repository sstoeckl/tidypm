# The simulated market ships as package data, so these tests guard the shipped
# objects rather than a loader. Each one corresponds to a property the
# documentation asserts, or to a defect some version of the export has had.

test_that("the metadata has the documented shape and vocabulary", {
  expect_s3_class(sim_assets, "tbl_df")
  expect_equal(nrow(sim_assets), 181L)
  expect_named(sim_assets, c("asset_id", "region", "currency", "currency_basis",
                             "asset_class", "sector", "credit_grade"))
  expect_equal(sum(duplicated(sim_assets$asset_id)), 0L)
  expect_setequal(unique(sim_assets$region), c("US", "EU", "UK", "CH", "JP", "GLOBAL"))
  expect_equal(
    c(table(sim_assets$asset_class)[c("equity", "credit", "government",
                                      "commodity", "money_market")]),
    c(equity = 123L, credit = 30L, government = 15L,
      commodity = 8L, money_market = 5L)
  )
  expect_equal(sum(!is.na(sim_assets$sector)), 123L)
  expect_setequal(stats::na.omit(sim_assets$credit_grade), c("IG", "HY"))
})

test_that("the currency repair reached every instrument", {
  # The export labels the 50 bonds "local" although their returns are in
  # dollars; a screen on this column would otherwise drop every bond.
  expect_true(all(sim_assets$currency_basis == "USD"))
  expect_true(all(sim_assets$currency == "US$"))
})

test_that("the return tables have the documented layout", {
  for (d in list(sim_asset_returns_monthly, sim_asset_returns_weekly)) {
    expect_named(d, c("date", "asset", "return"))
    expect_s3_class(d$date, "Date")
    expect_type(d$return, "double")
    expect_false(anyNA(d$return))
  }
  for (d in list(sim_class_returns_monthly, sim_class_returns_weekly,
                 sim_money_market_monthly, sim_money_market_weekly)) {
    expect_named(d, c("date", "class", "return", "index"))
    expect_s3_class(d$date, "Date")
    expect_false(anyNA(d$return))
  }
})

test_that("the class universe is the sixteen risky series, money markets beside it", {
  # The book's universe, agreed 2026-09-12 and taken at import: five currencies
  # x (government, credit, equity) plus one commodity index. No chapter should
  # have to drop the money markets in its first code block.
  cls <- unique(sim_class_returns_monthly$class)
  expect_length(cls, 16L)
  expect_false(any(grepl("_money_market$", cls)))
  expect_setequal(cls, c(paste0(rep(c("US", "UK", "EU", "CH", "JP"), each = 3),
                                c("_equity", "_government", "_credit")),
                         "GLOBAL_commodity"))
  mm <- unique(sim_money_market_monthly$class)
  expect_setequal(mm, paste0(c("US", "UK", "EU", "CH", "JP"), "_money_market"))
  expect_setequal(unique(sim_class_returns_weekly$class), cls)
  expect_setequal(unique(sim_money_market_weekly$class), mm)
})

test_that("every panel is rectangular", {
  # Version 4.2.0 was ragged by one cell -- GLOBAL_CMD_08 had no observation in
  # the first week -- which 4.2.1 closed. The check guards both directions:
  # raggedness must never arrive, or disappear, unnoticed.
  gap <- function(d, id) {
    length(unique(d$date)) * length(unique(d[[id]])) - nrow(d)
  }
  expect_equal(gap(sim_asset_returns_monthly, "asset"), 0L)
  expect_equal(gap(sim_asset_returns_weekly, "asset"), 0L)
  expect_equal(gap(sim_class_returns_monthly, "class"), 0L)
  expect_equal(gap(sim_class_returns_weekly, "class"), 0L)
  expect_equal(gap(sim_money_market_monthly, "class"), 0L)
  expect_equal(gap(sim_money_market_weekly, "class"), 0L)
})

test_that("coverage and identifiers line up across the datasets", {
  expect_equal(length(unique(sim_asset_returns_monthly$asset)), 181L)
  expect_equal(length(unique(sim_class_returns_monthly$date)), 372L)
  expect_equal(length(unique(sim_money_market_monthly$date)), 372L)
  expect_setequal(unique(sim_asset_returns_monthly$asset), sim_assets$asset_id)
  expect_identical(sim_riskfree_monthly$date, sort(unique(sim_class_returns_monthly$date)))
  expect_identical(sort(unique(sim_money_market_monthly$date)),
                   sort(unique(sim_class_returns_monthly$date)))
  expect_setequal(unique(sim_fx_monthly$region), c("EU", "UK", "CH", "JP"))
  expect_equal(length(unique(sim_curve_weekly$series)), 25L)
  expect_setequal(unique(sim_marketcap_weekly$asset),
                  sim_assets$asset_id[sim_assets$asset_class == "equity"])
  expect_true(all(sim_marketcap_weekly$market_cap > 0))
})

test_that("the bill is a risk-free rate and the money market is a duration asset", {
  # Both ship precisely because they differ; the documentation turns on it.
  # Since 5.0.0 the US money market earns its rolldown and tracks the bill
  # closely, so the difference is in the sign pattern rather than in a
  # drawdown: marked to market, it has more negative months than the bill.
  mm <- sim_money_market_monthly$return[
    sim_money_market_monthly$class == "US_money_market"]
  expect_lt(mean(sim_riskfree_monthly$return < 0), 0.02)
  expect_gt(mean(mm < 0), 0.05)
  expect_lt(cor(mm, sim_riskfree_monthly$return), 0.99)
  rf_ann <- prod(1 + sim_riskfree_monthly$return)^(12 / nrow(sim_riskfree_monthly)) - 1
  expect_equal(rf_ann, 0.0226, tolerance = 1e-3)
})

test_that("the returns really are in dollars, unhedged", {
  # A foreign money market has almost no duration, so unhedged it is nearly a
  # pure currency position. If the bonds were still in local currency, as the
  # export's metadata claimed, the correlation would be about zero.
  for (rg in c("EU", "UK", "CH", "JP")) {
    mm <- subset(sim_money_market_monthly, class == paste0(rg, "_money_market"))
    fx <- subset(sim_fx_monthly, region == rg)
    expect_gt(cor(mm$return, fx$fx_return[match(mm$date, fx$date)]), 0.95)
  }
})

test_that("hedging by subtracting fx_return removes the currency", {
  hedge <- function(d, series, rg) {
    x  <- d[d$class == series, ]
    fx <- sim_fx_monthly[sim_fx_monthly$region == rg, ]
    x$return - fx$fx_return[match(x$date, fx$date)]
  }
  jp_u <- subset(sim_money_market_monthly, class == "JP_money_market")$return
  expect_lt(sd(hedge(sim_money_market_monthly, "JP_money_market", "JP")), 0.1 * sd(jp_u))

  us_gov <- subset(sim_class_returns_monthly, class == "US_government")$return
  expect_lt(sd(hedge(sim_class_returns_monthly, "EU_government", "EU")), sd(us_gov) * 2)
})

test_that("the euro-area equity overlay of 5.1.0 is present", {
  # A stated capital-market assumption, +3 points a year on every euro-area
  # equity. A regenerated export without it would put EU_equity back among the
  # lowest-return equity markets and fail here.
  ann <- function(x) prod(1 + x)^(12 / length(x)) - 1
  eq <- subset(sim_class_returns_monthly, grepl("_equity$", class))
  a  <- tapply(eq$return, eq$class, ann)
  expect_gt(a[["EU_equity"]], a[["JP_equity"]] + 0.03)
})

test_that("the commodity block is eight independent series", {
  # Version 4.1.0 shipped eight copies of one factor: rank 1 of 8, which made
  # the full instrument covariance singular.
  ids <- sim_assets$asset_id[sim_assets$asset_class == "commodity"]
  x <- as_xts_pm(subset(sim_asset_returns_monthly, asset %in% ids),
                 date, asset, return)
  expect_equal(qr(cov(x))$rank, length(ids))
  expect_lt(min(cor(x)), 0.5)
  expect_gt(diff(range(apply(x, 2, sd))), 0.01)   # gold and crypto differ
})

test_that("the datasets cross into xts without surprises", {
  r <- as_xts_pm(sim_class_returns_monthly, date, class, return)
  expect_s3_class(r, "xts")
  expect_equal(dim(r), c(372L, 16L))
  expect_false(anyNA(r))
  expect_false(is.unsorted(zoo::index(r)))
  m <- as_xts_pm(sim_money_market_monthly, date, class, return)
  expect_equal(dim(m), c(372L, 5L))
})

test_that("the weekly instrument panel crosses into xts without padding", {
  r <- as_xts_pm(sim_asset_returns_weekly, date, asset, return)
  expect_s3_class(r, "xts")
  expect_equal(dim(r), c(1618L, 181L))
  expect_false(anyNA(r))
})
