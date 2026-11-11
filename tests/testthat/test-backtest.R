# The backtest engine is tested on properties that must hold rather than on
# numbers that happen to come out: the drift accounting must reproduce a
# hand-computed equal-weighted return, the timing must contain no look-ahead,
# net must equal gross minus the cost actually charged, and the reported
# out-of-sample window must be exactly what the arguments imply.

# ---------------------------------------------------------------------------
# fixtures
# ---------------------------------------------------------------------------

toy_returns <- function(n = 96, k = 4, seed = 1602) {
  set.seed(seed)
  # Month ends built by stepping first-of-month and subtracting a day.
  # seq(by = "month") from a 31st rolls into the following month and would give
  # a calendar with two observations in some months and none in others, which
  # xts::endpoints() then honours.
  idx <- seq(as.Date("2010-02-01"), by = "month", length.out = n) - 1L
  xts::xts(
    matrix(stats::rnorm(n * k, 0.006, 0.04), n, k,
           dimnames = list(NULL, paste0(c("US", "EU", "UK", "JP")[seq_len(k)],
                                        "_equity"))),
    order.by = idx
  )
}

ew_only <- function() pm_book_strategies()["equal_weight"]

risky_classes <- function() {
  subset(sim_class_returns_monthly, !grepl("_money_market$", class))
}


# ---------------------------------------------------------------------------
# shape and timing
# ---------------------------------------------------------------------------

test_that("the object has the documented shape", {
  R  <- toy_returns()
  bt <- pm_backtest(R, ew_only(), estimation_window = 24L, verbose = FALSE)

  expect_s3_class(bt, "pm_backtest")
  expect_named(bt, c("metrics", "returns", "weights", "costs_paid",
                     "diagnosis", "fits", "cost_schedule", "meta"))

  expect_named(bt$metrics,
               c("strategy", "label", "basis", "ann_return", "ann_vol",
                 "sharpe", "skewness", "es_95", "max_drawdown", "ann_turnover",
                 "n_periods", "n_failed", "n_suspect", "objective_metric"))
  expect_equal(nrow(bt$metrics), 2L)          # one strategy, two bases
  expect_setequal(bt$metrics$basis, c("gross", "net"))

  expect_named(bt$returns, c("strategy", "date", "basis", "return"))
  expect_named(bt$weights, c("strategy", "date", "asset", "weight"))
  expect_named(bt$costs_paid, c("strategy", "date", "turnover", "cost"))
  expect_named(bt$diagnosis, c("strategy", "date", "solver", "status",
                               "objective", "n_assets", "weights_sum",
                               "message"))
})

test_that("the out-of-sample window is exactly what the arguments imply", {
  R  <- toy_returns(n = 96)
  bt <- pm_backtest(R, ew_only(), estimation_window = 24L, verbose = FALSE)

  # 96 months, 24-month window: rebalance at months 24..96 (73 dates), first
  # out-of-sample return in month 25, so 72 of them.
  expect_equal(bt$meta$n_rebalances, 73L)
  expect_equal(bt$meta$n_oos, 72L)
  expect_equal(bt$meta$oos_start, zoo::index(R)[25])
  expect_equal(bt$meta$oos_end, zoo::index(R)[96])
  expect_equal(nrow(bt$diagnosis), 73L)
  expect_equal(nrow(bt$costs_paid), 72L)
  expect_equal(nrow(bt$returns), 2L * 72L)
  expect_equal(nrow(bt$weights), 72L * ncol(R))
})

test_that("the estimation window really holds estimation_window observations", {
  # PortfolioAnalytics uses R[(ep - rolling_window):ep, ], which is one more
  # than asked for. pm_backtest() compensates; this pins that it does.
  R  <- toy_returns(n = 60)
  bt <- pm_backtest(R, pm_book_strategies()["inverse_vol"],
                    estimation_window = 24L, verbose = FALSE)

  fit  <- bt$fits$inverse_vol
  used <- vapply(fit$opt_rebalancing, function(x) nrow(x$R), integer(1))
  expect_true(all(used == 24L))
})

test_that("there is no look-ahead: weights depend only on earlier returns", {
  # Corrupt the last return and check that no weight in the backtest moves.
  # If any window reached forward, at least one would.
  R <- toy_returns(n = 60)
  bt1 <- pm_backtest(R, pm_book_strategies()["inverse_vol"],
                     estimation_window = 24L, verbose = FALSE)

  R2 <- R
  R2[60, ] <- c(0.5, -0.5, 0.5, -0.5)
  bt2 <- pm_backtest(R2, pm_book_strategies()["inverse_vol"],
                     estimation_window = 24L, verbose = FALSE)

  # The last rebalancing date uses the last return, and its weights are never
  # traded, so compare everything before it.
  last_date <- max(bt1$diagnosis$date)
  w1 <- subset(bt1$weights, date < last_date)
  w2 <- subset(bt2$weights, date < last_date)
  expect_equal(w1$weight, w2$weight)
})


# ---------------------------------------------------------------------------
# the accounting
# ---------------------------------------------------------------------------

test_that("equal weight reproduces a hand-computed portfolio return", {
  # With monthly rebalancing to 1/N there is no drift to carry, so the gross
  # return of each period is exactly the cross-sectional mean.
  R  <- toy_returns(n = 48)
  bt <- pm_backtest(R, ew_only(), estimation_window = 12L, verbose = FALSE)

  got  <- subset(bt$returns, basis == "gross")$return
  want <- rowMeans(zoo::coredata(R)[13:48, ])
  expect_equal(got, unname(want), tolerance = 1e-12)
})

test_that("weights drift between rebalancing dates and are not reset", {
  # Rebalance yearly on monthly data: within a year the weights must move with
  # the returns, and at the turn of the year they must snap back to 1/N.
  R  <- toy_returns(n = 48)
  bt <- pm_backtest(R, ew_only(), estimation_window = 12L,
                    rebalance_on = "years", verbose = FALSE)

  w   <- bt$weights
  idx <- zoo::index(R)

  first  <- subset(w, date == idx[13])$weight
  second <- subset(w, date == idx[14])$weight
  expect_equal(first, rep(0.25, 4L))          # freshly rebalanced
  expect_false(isTRUE(all.equal(second, rep(0.25, 4L))))  # drifted

  # Drift is multiplicative in one period's return, normalised.
  drift <- 0.25 * (1 + zoo::coredata(R)[13, ])
  expect_equal(second, unname(drift / sum(drift)), tolerance = 1e-12)

  # And the next January is a fresh 1/N.
  expect_equal(subset(w, date == idx[25])$weight, rep(0.25, 4L))
})

test_that("net is gross minus the cost charged, and costs are never blended in", {
  R  <- toy_returns(n = 48)
  bt <- pm_backtest(R, ew_only(), estimation_window = 12L, verbose = FALSE)

  gross <- subset(bt$returns, basis == "gross")$return
  net   <- subset(bt$returns, basis == "net")$return
  expect_equal(net, gross - bt$costs_paid$cost, tolerance = 1e-14)

  # A zero-cost schedule leaves the two bases identical: costs are added on,
  # never baked into the headline number.
  free <- pm_backtest(R, ew_only(), estimation_window = 12L,
                      costs = c(.default = 0), verbose = FALSE)
  expect_equal(subset(free$returns, basis == "net")$return,
               subset(free$returns, basis == "gross")$return)
  expect_equal(free$metrics$ann_return[1], free$metrics$ann_return[2])
})

test_that("the cost model is per class, not flat", {
  # Two schedules that differ only in the credit rate must give different net
  # returns for a portfolio holding credit, and identical ones for a portfolio
  # holding none.
  R  <- as_xts_pm(risky_classes(), date, class, return)[1:60, ]

  cheap <- pm_backtest(R, ew_only(), estimation_window = 24L,
                       costs = pm_costs_default(credit = 0), verbose = FALSE)
  dear  <- pm_backtest(R, ew_only(), estimation_window = 24L,
                       costs = pm_costs_default(credit = 0.01), verbose = FALSE)

  expect_equal(subset(cheap$returns, basis == "gross")$return,
               subset(dear$returns, basis == "gross")$return)
  expect_true(all(dear$costs_paid$cost >= cheap$costs_paid$cost))
  expect_gt(sum(dear$costs_paid$cost), sum(cheap$costs_paid$cost))

  # The resolved schedule prices the classes differently.
  cs <- cheap$cost_schedule
  expect_equal(unname(cs["US_credit"]), 0)
  expect_gt(unname(cs["US_equity"]), unname(cs["US_government"]))
})

test_that("turnover is the one-way fraction of the book and includes the first trade", {
  R  <- toy_returns(n = 48)
  bt <- pm_backtest(R, ew_only(), estimation_window = 12L,
                    rebalance_on = "years", verbose = FALSE)

  tv <- bt$costs_paid$turnover
  # Buying the initial 1/N portfolio out of cash turns over 50% by the one-way
  # convention (100% traded, halved).
  expect_equal(tv[1], 0.5)
  # Nothing is traded between annual rebalancing dates.
  expect_equal(tv[2:12], rep(0, 11L))
  expect_gt(tv[13], 0)

  expect_equal(bt$metrics$ann_turnover[1], mean(tv) * 12)
})

test_that("annualised metrics are internally consistent", {
  R  <- toy_returns(n = 96)
  bt <- pm_backtest(R, ew_only(), riskfree = 0.002,
                    estimation_window = 24L, verbose = FALSE)

  r <- subset(bt$returns, basis == "gross")$return
  m <- subset(bt$metrics, basis == "gross")
  n <- length(r)

  expect_equal(m$ann_return, prod(1 + r)^(12 / n) - 1)
  expect_equal(m$ann_vol, stats::sd(r) * sqrt(12))
  # Sharpe is (annualised return - annualised bill) / annualised volatility, so
  # the three numbers in the table reconcile with each other.
  expect_equal(m$sharpe,
               (m$ann_return - (1.002^12 - 1)) / m$ann_vol,
               tolerance = 1e-10)
  expect_gt(m$es_95, 0)          # a positive loss magnitude
  expect_gte(m$max_drawdown, 0)
})

test_that("periods_per_year is an argument and changes the annualisation", {
  R <- toy_returns(n = 96)
  a <- pm_backtest(R, ew_only(), estimation_window = 24L,
                   periods_per_year = 12L, verbose = FALSE)
  b <- pm_backtest(R, ew_only(), estimation_window = 24L,
                   periods_per_year = 52L, verbose = FALSE)

  expect_equal(subset(a$returns, basis == "gross")$return,
               subset(b$returns, basis == "gross")$return)
  expect_equal(b$metrics$ann_vol[1] / a$metrics$ann_vol[1], sqrt(52 / 12))
})


# ---------------------------------------------------------------------------
# input validation
# ---------------------------------------------------------------------------

test_that("a gap in the returns is an error, not something to impute", {
  R <- toy_returns(n = 48)
  R[10, 2] <- NA
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L,
                           verbose = FALSE),
               "missing value")
})

test_that("arguments are validated with a fix, not a bare stop", {
  R <- toy_returns(n = 48)
  expect_error(pm_backtest("not returns", ew_only()), "xts")
  expect_error(pm_backtest(R[, 1, drop = FALSE], ew_only()), "at least two")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 1L), "at least 2")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12.5), "whole number")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 100L), "nothing out of sample")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L, es_p = 0.05),
               "0.5, 1")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L, sum_tolerance = -1),
               "0, 0.5")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L, rebalance_on = 1),
               "single string")
  expect_error(pm_backtest(R, "not a register"), "pm_register")
})

test_that("the risk-free series must line up with the returns", {
  R <- toy_returns(n = 48)
  # A bill series that stops half way through is the common mistake, and it
  # must not silently drop the uncovered months from the Sharpe ratio.
  short <- data.frame(date = zoo::index(R)[1:24], return = 0.002)
  expect_error(
    pm_backtest(R, ew_only(), riskfree = short,
                estimation_window = 12L, verbose = FALSE),
    "does not cover"
  )
  # A weekly bill against monthly returns fails the same way.
  expect_error(
    pm_backtest(R, ew_only(), riskfree = sim_riskfree_weekly,
                estimation_window = 12L, verbose = FALSE),
    "does not cover"
  )
  expect_error(
    pm_backtest(R, ew_only(), riskfree = data.frame(date = zoo::index(R)),
                estimation_window = 12L, verbose = FALSE),
    "date.*return|return"
  )
  # NULL and a scalar are both fine, and a positive bill lowers the Sharpe.
  z <- pm_backtest(R, ew_only(), riskfree = NULL, estimation_window = 12L,
                   verbose = FALSE)
  p <- pm_backtest(R, ew_only(), riskfree = 0.003, estimation_window = 12L,
                   verbose = FALSE)
  expect_gt(z$metrics$sharpe[1], p$metrics$sharpe[1])
})

test_that("a specification built for the wrong universe is caught", {
  R    <- toy_returns(n = 48)
  wrong <- pm_strategy("wrong",
                       spec = PortfolioAnalytics::portfolio.spec(letters[1:9]))
  expect_error(pm_backtest(R, wrong, estimation_window = 12L, verbose = FALSE),
               "9 assets")

  not_a_spec <- pm_strategy("bad", spec = function(assets) "oops")
  expect_error(pm_backtest(R, not_a_spec, estimation_window = 12L,
                           verbose = FALSE),
               "did not give")
})

test_that("a long data frame is accepted through the documented boundary", {
  R  <- as_xts_pm(risky_classes(), date, class, return)[1:60, ]
  from_xts <- pm_backtest(R, ew_only(), estimation_window = 24L, verbose = FALSE)
  from_df  <- pm_backtest(
    subset(risky_classes(), date <= zoo::index(R)[60]),
    ew_only(), asset = class, value = return,
    estimation_window = 24L, verbose = FALSE
  )
  expect_equal(from_df$returns$return, from_xts$returns$return)
})


# ---------------------------------------------------------------------------
# solver failure handling
# ---------------------------------------------------------------------------

# An exact `full_investment` constraint is encoded by some builds of
# PortfolioAnalytics as two opposing inequality rows. That encoding is
# degenerate: quadprog reports the problem infeasible and ROI hands back a
# weight vector of NAs without raising. `sum_tolerance` exists to avoid it, and
# setting it to zero provokes it.
#
# Whether any given rebalancing date actually falls over is a property of the
# build AND of the data, not of the build alone. Pinning a row count is
# therefore wrong twice: it broke silently when the simulated market was
# regenerated and the failing windows moved past it, and it would break again
# when the upstream fix is released. The two tests below search for a prefix
# that fails and skip when none does, so they hold for any combination of the
# two. The search is done once and shared.
degenerate_backtest <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    R   <- as_xts_pm(risky_classes(), date, class, return)
    gmv <- pm_book_strategies()["min_variance"]
    for (n in c(200L, 280L, nrow(R))) {
      bt <- suppressWarnings(
        pm_backtest(R[seq_len(n), ], gmv, estimation_window = 60L,
                    sum_tolerance = 0, verbose = FALSE))
      if (bt$metrics$n_failed[1] > 0) {
        cached <<- list(rows = n, bt = bt)
        return(cached)
      }
    }
    cached <<- list(rows = NA_integer_, bt = NULL)
    cached
  }
})

test_that("sum_tolerance = 0 reproduces the quadprog failure, and it is reported", {
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()
  d <- degenerate_backtest()
  skip_if(is.null(d$bt),
          "no window in this panel produces the degenerate failure")

  broken <- d$bt
  R      <- as_xts_pm(risky_classes(), date, class, return)[seq_len(d$rows), ]
  fixed  <- pm_backtest(R, pm_book_strategies()["min_variance"],
                        estimation_window = 60L, verbose = FALSE)

  expect_gt(broken$metrics$n_failed[1], 0)
  expect_equal(fixed$metrics$n_failed[1], 0)

  # The failure is invisible upstream: every period is still an
  # optimize.portfolio object of the right class.
  expect_true(all(vapply(broken$fits$min_variance$opt_rebalancing,
                         inherits, logical(1), what = "optimize.portfolio")))
  expect_true(any(grepl("Non-finite weights",
                        broken$diagnosis$message, fixed = TRUE)))
})

test_that("a failed period warns and carries weights forward rather than trading NA", {
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()
  d <- degenerate_backtest()
  skip_if(is.null(d$bt),
          "no window in this panel produces the degenerate failure")

  R <- as_xts_pm(risky_classes(), date, class, return)[seq_len(d$rows), ]
  expect_warning(
    bt <- pm_backtest(R, pm_book_strategies()["min_variance"],
                      estimation_window = 60L, sum_tolerance = 0,
                      verbose = FALSE),
    "carried forward"
  )
  expect_false(anyNA(bt$weights$weight))
  expect_false(anyNA(bt$returns$return))
})

test_that("a period whose weights are non-finite is diagnosed and carried forward", {
  # The two tests above depend on an upstream defect, so they vanish once it is
  # fixed. The machinery they exercise must not vanish with them: construct the
  # failure directly, so this holds against any version of PortfolioAnalytics.
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()

  R  <- as_xts_pm(risky_classes(), date, class, return)[1:130, ]
  bt <- pm_backtest(R, pm_book_strategies()["min_variance"],
                    estimation_window = 60L, verbose = FALSE)
  expect_equal(bt$metrics$n_failed[1], 0)

  # Corrupt one period exactly as a failed solve leaves it.
  fit <- bt$fits$min_variance
  k   <- 5L
  fit$opt_rebalancing[[k]]$weights[] <- NA_real_

  d <- diagnose_rebalancing(fit)
  expect_equal(nrow(d), length(fit$opt_rebalancing))
  expect_equal(d$status[k], "failed")
  expect_true(grepl("Non-finite weights", d$message[k], fixed = TRUE))
  expect_true(all(d$status[-k] == "converged"))
})


# ---------------------------------------------------------------------------
# parallel execution
# ---------------------------------------------------------------------------

test_that("workers is validated and needs doParallel", {
  R <- toy_returns(n = 48)
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L, workers = 0L),
               "at least 1")
  expect_error(pm_backtest(R, ew_only(), estimation_window = 12L, workers = 2.5),
               "whole number")
})

test_that("running in parallel changes the wall clock, not the answer", {
  skip_if_not_installed("doParallel")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()
  skip_if(parallel::detectCores() < 4L, "needs at least four cores")

  R   <- as_xts_pm(risky_classes(), date, class, return)[1:120, ]
  reg <- pm_book_strategies()["min_variance"]

  seq_bt <- pm_backtest(R, reg, estimation_window = 60L, verbose = FALSE,
                        workers = 1L)
  par_bt <- pm_backtest(R, reg, estimation_window = 60L, verbose = FALSE,
                        workers = 2L)

  # optimize.portfolio.rebalancing() parallelises over periods, so the periods
  # are independent and the result must be identical, not merely close.
  expect_identical(seq_bt$weights$weight, par_bt$weights$weight)
  expect_identical(seq_bt$returns$return, par_bt$returns$return)
  expect_equal(seq_bt$metrics, par_bt$metrics)
  expect_equal(seq_bt$diagnosis$status, par_bt$diagnosis$status)
})

test_that("the foreach backend is restored after the call", {
  skip_if_not_installed("doParallel")
  skip_on_cran()
  skip_if(parallel::detectCores() < 4L, "needs at least four cores")

  R <- toy_returns(n = 48)
  before <- foreach::getDoParName()
  pm_backtest(R, ew_only(), estimation_window = 12L, workers = 2L,
              verbose = FALSE)
  # The cluster is stopped and the backend put back, so a later sequential run
  # is not silently talking to a dead cluster.
  expect_false(identical(foreach::getDoParName(), "doParallelSNOW"))
  expect_equal(foreach::getDoParWorkers(), 1L)
  expect_no_error(pm_backtest(R, ew_only(), estimation_window = 12L,
                              verbose = FALSE))
})


# ---------------------------------------------------------------------------
# pm_backtest_metrics()
# ---------------------------------------------------------------------------

test_that("the default pull is each strategy's own objective", {
  R   <- as_xts_pm(risky_classes(), date, class, return)[1:90, ]
  reg <- pm_book_strategies()[c("equal_weight", "inverse_vol")]
  bt  <- pm_backtest(R, reg, riskfree = sim_riskfree_monthly[1:90, ],
                     estimation_window = 36L, verbose = FALSE)

  out <- pm_backtest_metrics(bt)
  expect_named(out, c("strategy", "label", "basis", "metric", "value"))
  expect_equal(nrow(out), 4L)
  expect_equal(subset(out, strategy == "equal_weight")$metric,
               c("sharpe", "sharpe"))
  expect_equal(subset(out, strategy == "inverse_vol")$metric,
               c("ann_vol", "ann_vol"))

  # The value really is the matching column, not a fixed one.
  expect_equal(subset(out, strategy == "inverse_vol" & basis == "gross")$value,
               subset(bt$metrics, strategy == "inverse_vol" & basis == "gross")$ann_vol)
})

test_that("metric and basis select, and 'all' returns the wide table", {
  R  <- toy_returns(n = 60)
  bt <- pm_backtest(R, ew_only(), estimation_window = 24L, verbose = FALSE)

  expect_equal(nrow(pm_backtest_metrics(bt, basis = "gross")), 1L)
  expect_equal(pm_backtest_metrics(bt, metric = "all"), bt$metrics)
  expect_equal(nrow(pm_backtest_metrics(bt, metric = "all", basis = "net")), 1L)

  long <- pm_backtest_metrics(bt, metric = c("ann_vol", "sharpe"))
  expect_setequal(long$metric, c("ann_vol", "sharpe"))
  expect_equal(nrow(long), 4L)

  expect_error(pm_backtest_metrics(bt, metric = "alpha"), "not")
  expect_error(pm_backtest_metrics("nope"), "pm_backtest")
})

test_that("print returns its input invisibly", {
  skip_if_not_installed("withr")
  R  <- toy_returns(n = 48)
  bt <- pm_backtest(R, ew_only(), estimation_window = 12L, verbose = FALSE)
  res <- suppressMessages(withr::with_output_sink(tempfile(), print(bt)))
  expect_identical(res, bt)
})


# ---------------------------------------------------------------------------
# the book's headline numbers
# ---------------------------------------------------------------------------

test_that("the book's out-of-sample Sharpe ratios reproduce", {
  # 16 risky class series, 60-month rolling window, monthly rebalancing,
  # 312 out-of-sample months, from the first month of model year 6. Computed
  # independently with base R and quadprog. Slow: about four minutes.
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_on_cran()
  skip_on_ci()

  bt <- pm_backtest(
    risky_classes(),
    pm_book_strategies()[c("equal_weight", "min_variance", "max_sharpe")],
    riskfree = sim_riskfree_monthly,
    asset = class, value = return,
    verbose = FALSE
  )

  expect_equal(bt$meta$n_oos, 312L)
  expect_equal(bt$meta$oos_start, as.Date("2106-01-31"))
  expect_equal(bt$meta$oos_end, as.Date("2131-12-31"))
  expect_length(bt$meta$assets, 16L)

  g <- subset(bt$metrics, basis == "gross")
  sharpe <- stats::setNames(g$sharpe, g$strategy)

  expect_equal(unname(sharpe["equal_weight"]), 0.54, tolerance = 0.05)   # 5.1.0: 0.54, 4.3.0: 0.294
  expect_equal(unname(sharpe["min_variance"]), 0.70, tolerance = 0.05)   # 5.1.0: 0.65, 4.3.0: 0.328
  expect_equal(unname(sharpe["max_sharpe"]),   0.53, tolerance = 0.05)   # 5.1.0: 0.43, 4.3.0: 0.131

  # And every period solved.
  expect_true(all(g$n_failed == 0L))
  expect_true(all(bt$diagnosis$status == "converged"))

  # Costs are reported, never blended: net is strictly below gross for every
  # strategy that trades.
  n <- subset(bt$metrics, basis == "net")
  expect_true(all(n$ann_return < g$ann_return))
})
