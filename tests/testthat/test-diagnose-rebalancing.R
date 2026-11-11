# diagnose_rebalancing() exists because optimize.portfolio.rebalancing() does
# not report a solver failure. These tests pin (a) its contract, (b) the four
# criteria, and (c) the upstream PortfolioAnalytics 2.1.2 behaviour the criteria
# are written against, so that if the package ever starts reporting solver
# failure the specification gets revisited rather than silently outgrown.

# ---------------------------------------------------------------------------
# helpers: hand-built objects, so the criteria can be tested one at a time
# without waiting for a solver.
# ---------------------------------------------------------------------------

fake_spec <- function(n = 4L, min_sum = 1, max_sum = 1, lo = 0, hi = 1) {
  assets <- letters[seq_len(n)]
  p <- PortfolioAnalytics::portfolio.spec(assets)
  p <- PortfolioAnalytics::add.constraint(p, "weight_sum",
                                          min_sum = min_sum, max_sum = max_sum)
  PortfolioAnalytics::add.constraint(p, "box", min = lo, max = hi)
}

fake_period <- function(weights, out = 0.01, spec = fake_spec(length(weights)),
                        cls = "optimize.portfolio.ROI") {
  names(weights) <- letters[seq_along(weights)]
  structure(
    list(weights = weights, out = out, portfolio = spec),
    class = c(cls, "optimize.portfolio")
  )
}

fake_rebal <- function(periods, spec = fake_spec()) {
  structure(
    list(portfolio = spec, opt_rebalancing = periods),
    class = "optimize.portfolio.rebalancing"
  )
}


# ---------------------------------------------------------------------------
# contract
# ---------------------------------------------------------------------------

test_that("the signature exposes the tolerance the criteria are written against", {
  expect_named(formals(diagnose_rebalancing), c("opt", "tol"))
  expect_equal(eval(formals(diagnose_rebalancing)$tol), 1e-6)
})

test_that("it refuses anything that is not a rebalancing object", {
  expect_error(diagnose_rebalancing(NULL), "optimize.portfolio.rebalancing")
  expect_error(diagnose_rebalancing(list(a = 1)), "optimize.portfolio.rebalancing")
  expect_error(diagnose_rebalancing(42), "optimize.portfolio.rebalancing")
})

test_that("it refuses a malformed rebalancing object rather than guessing", {
  # A diagnostic that can be run, ignored and mistaken for a pass is worse than
  # none: a chapter would report a clean bill of health it never earned.
  expect_error(
    diagnose_rebalancing(fake_rebal(list())),
    "no periods"
  )
  expect_error(
    diagnose_rebalancing(fake_rebal(unname(list(fake_period(rep(0.25, 4)))))),
    "named list"
  )
  expect_error(
    diagnose_rebalancing(fake_rebal(list(not_a_date = fake_period(rep(0.25, 4))))),
    "parse as dates"
  )
})

test_that("tol is validated", {
  ok <- fake_rebal(list(`2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1))))
  expect_error(diagnose_rebalancing(ok, tol = -1), "non-negative")
  expect_error(diagnose_rebalancing(ok, tol = c(1e-6, 1e-6)), "non-negative")
  expect_error(diagnose_rebalancing(ok, tol = "1e-6"), "non-negative")
  expect_error(diagnose_rebalancing(ok, tol = NA_real_), "non-negative")
})

test_that("it returns the documented tibble, one row per period, in order", {
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1), out = 0.011),
    `2021-12-31` = fake_period(c(0.1, 0.2, 0.3, 0.4), out = 0.012),
    `2022-12-31` = fake_period(c(0.7, 0.1, 0.1, 0.1), out = 0.013)
  ))
  out <- diagnose_rebalancing(opt)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("date", "solver", "status", "objective",
                      "n_assets", "weights_sum", "message"))
  expect_equal(nrow(out), 3L)
  expect_s3_class(out$date, "Date")
  expect_equal(out$date, as.Date(c("2020-12-31", "2021-12-31", "2022-12-31")))
  expect_equal(out$solver, rep("ROI", 3L))
  expect_equal(out$status, rep("converged", 3L))
  expect_equal(out$objective, c(0.011, 0.012, 0.013))
  expect_equal(out$n_assets, rep(4L, 3L))
  expect_equal(out$weights_sum, rep(1, 3L))
  expect_true(all(is.na(out$message)))
})

test_that("n_assets counts holdings, not columns", {
  opt <- fake_rebal(list(`2020-12-31` = fake_period(c(0.5, 0.5, 0, 0))))
  expect_equal(diagnose_rebalancing(opt)$n_assets, 2L)
})


# ---------------------------------------------------------------------------
# the four criteria
# ---------------------------------------------------------------------------

test_that("criterion 1: a condition object in the list is a failure, not a solve", {
  opt <- fake_rebal(list(
    `2020-12-31` = simpleError("No solution found: quadprog said no"),
    `2021-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1))
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, c("failed", "converged"))
  expect_match(out$message[1], "quadprog said no")
  expect_true(is.na(out$solver[1]))
  expect_true(is.na(out$objective[1]))
})

test_that("criterion 1 also catches a non-condition, non-optimisation element", {
  opt <- fake_rebal(list(`2020-12-31` = list(weights = c(1, 0, 0, 0))))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "failed")
  expect_match(out$message, "not an optimize.portfolio")
})

test_that("criterion 2: NA weights are a failure even though the object looks normal", {
  # This is the shape PortfolioAnalytics returns when ROI's quadprog plugin
  # reports a non-zero status: the class is right, the fields are all there,
  # and every number is NA.
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(rep(NA_real_, 4L), out = NA_real_)
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "failed")
  expect_match(out$message, "Non-finite weights")
  expect_equal(out$solver, "ROI")          # the class survives, the numbers do not
})

test_that("criterion 2: a non-finite objective with finite weights is still a failure", {
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1), out = Inf)
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "failed")
  expect_match(out$message, "Objective value")
})

test_that("criterion 3: weights that break the specification are suspect", {
  spec <- fake_spec(min_sum = 1, max_sum = 1, lo = 0, hi = 1)
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(c(0.5, 0.3, 0.2, 0.1), spec = spec),   # sums to 1.1
    `2021-12-31` = fake_period(c(1.3, -0.1, -0.1, -0.1), spec = spec) # negative
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, c("suspect", "suspect"))
  expect_match(out$message[1], "above max_sum")
  expect_match(out$message[2], "below its box minimum")
})

test_that("criterion 3 keys on the constraint class, not on $type", {
  # A full_investment constraint is a weight_sum_constraint whose $type is
  # "full_investment" and which ALSO carries $min and $max of 1. Read as a box
  # it would pin every weight to 1 and flag every honest solve.
  p <- PortfolioAnalytics::portfolio.spec(letters[1:4])
  p <- PortfolioAnalytics::add.constraint(p, "full_investment")
  p <- PortfolioAnalytics::add.constraint(p, "long_only")

  fi <- p$constraints[[1]]
  expect_s3_class(fi, "weight_sum_constraint")
  expect_equal(fi$type, "full_investment")
  expect_equal(as.numeric(fi$min), 1)           # the trap, pinned upstream
  expect_equal(as.numeric(fi$max), 1)

  opt <- fake_rebal(list(`2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1), spec = p)))
  expect_equal(diagnose_rebalancing(opt)$status, "converged")
})

test_that("criterion 3 takes the tightest of several enabled box constraints", {
  p <- PortfolioAnalytics::portfolio.spec(letters[1:4])
  p <- PortfolioAnalytics::add.constraint(p, "full_investment")
  p <- PortfolioAnalytics::add.constraint(p, "long_only")
  p <- PortfolioAnalytics::add.constraint(p, "box", min = 0, max = 0.35)

  opt <- fake_rebal(list(`2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1), spec = p)))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "suspect")
  expect_match(out$message, "above its box maximum 0.35")
})

test_that("criterion 3 respects tol, which is why it is an argument", {
  spec <- fake_spec(min_sum = 1, max_sum = 1)
  # Not near the equal-weight vector, or criterion 4 would fire at the loose
  # tolerance and mask the point being made.
  opt  <- fake_rebal(list(
    `2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1 + 1e-4), spec = spec)
  ))
  expect_equal(diagnose_rebalancing(opt, tol = 1e-6)$status, "suspect")
  expect_equal(diagnose_rebalancing(opt, tol = 1e-3)$status, "converged")
})

test_that("criterion 4: the equal-weight vector from an optimiser is suspect", {
  opt <- fake_rebal(list(`2020-12-31` = fake_period(rep(0.25, 4L))))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "suspect")
  expect_match(out$message, "equal-weight vector")
})

test_that("criterion 4: repeating the weights while the objective moved is suspect", {
  w <- c(0.4, 0.3, 0.2, 0.1)
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(w, out = 0.011),
    `2021-12-31` = fake_period(w, out = 0.012)
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, c("converged", "suspect"))
  expect_match(out$message[2], "identical to the previous period")
})

test_that("criterion 4 does not fire when the problem itself did not change", {
  # A historical CVaR at 95% over 60 months is the mean of the three worst
  # months. Roll the window by one and, unless one of those three moved, the
  # linear programme is literally identical and repeating the answer is right.
  # 243 of 313 min-CVaR periods on the book's universe do exactly this.
  w <- c(0.4, 0.3, 0.2, 0.1)
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(w, out = 0.011),
    `2021-12-31` = fake_period(w, out = 0.011)
  ))
  expect_equal(diagnose_rebalancing(opt)$status, c("converged", "converged"))
})

test_that("criterion 4: the turnover constraint's weight_initial is suspect", {
  p <- PortfolioAnalytics::portfolio.spec(letters[1:4])
  p <- PortfolioAnalytics::add.constraint(p, "full_investment")
  p <- PortfolioAnalytics::add.constraint(p, "long_only")
  p <- PortfolioAnalytics::add.constraint(p, "turnover", turnover_target = 0.2)
  p$constraints[[3]]$weight_initial <- c(0.4, 0.3, 0.2, 0.1)

  opt <- fake_rebal(list(`2020-12-31` = fake_period(c(0.4, 0.3, 0.2, 0.1), spec = p)))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, "suspect")
  expect_match(out$message, "weight_initial")
})

test_that("criterion 4 exempts equal.weight() and inverse.volatility.weight()", {
  # For a rule the seed IS the answer. Without the exemption every period of a
  # 1/N benchmark would report "suspect" and the column would stop meaning
  # anything.
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(rep(0.25, 4L), out = 0,
                               cls = "optimize.portfolio.eqwt"),
    `2021-12-31` = fake_period(rep(0.25, 4L), out = 0,
                               cls = "optimize.portfolio.eqwt")
  ))
  out <- diagnose_rebalancing(opt)
  expect_equal(out$status, c("converged", "converged"))
  expect_equal(out$solver, c("eqwt", "eqwt"))

  # ... but criterion 3 still applies to them.
  bad <- fake_rebal(list(
    `2020-12-31` = fake_period(rep(0.5, 4L), out = 0,
                               cls = "optimize.portfolio.invol")
  ))
  expect_equal(diagnose_rebalancing(bad)$status, "suspect")
})

test_that("the first criterion to fire wins", {
  # NA weights that also violate the box: criterion 2 is definitive and reports
  # "failed", not "suspect".
  opt <- fake_rebal(list(
    `2020-12-31` = fake_period(c(NA_real_, -1, 2, 0.5), out = NA_real_)
  ))
  expect_equal(diagnose_rebalancing(opt)$status, "failed")
})


# ---------------------------------------------------------------------------
# upstream behaviour the criteria are written against
# ---------------------------------------------------------------------------

test_that("the failure it is meant to catch is real in PortfolioAnalytics 2.1.2", {
  # Not a test of tidypm. If PortfolioAnalytics ever starts reporting solver
  # failure, this test fails and the specification gets revisited.
  skip_if_not_installed("PortfolioAnalytics")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()

  set.seed(1602)
  n <- 60
  R <- xts::xts(
    matrix(stats::rnorm(n * 4, 0.006, 0.04), n, 4,
           dimnames = list(NULL, c("a", "b", "c", "d"))),
    order.by = seq(as.Date("2015-01-31"), by = "month", length.out = n)
  )

  # Infeasible by construction: four weights of at least 0.5 each cannot sum
  # to 1.
  spec <- PortfolioAnalytics::portfolio.spec(colnames(R))
  spec <- PortfolioAnalytics::add.constraint(spec, "weight_sum",
                                             min_sum = 1, max_sum = 1)
  spec <- PortfolioAnalytics::add.constraint(spec, "box", min = 0.5, max = 0.6)
  spec <- PortfolioAnalytics::add.objective(spec, type = "risk", name = "StdDev")

  opt <- suppressWarnings(PortfolioAnalytics::optimize.portfolio.rebalancing(
    R, spec, optimize_method = "ROI",
    rebalance_on = "years", training_period = 24, rolling_window = 24
  ))

  # The run completes and returns a normal-looking object. Nothing about it
  # says that not one period solved.
  expect_s3_class(opt, "optimize.portfolio.rebalancing")
  expect_true(length(opt$opt_rebalancing) > 0)

  usable <- vapply(opt$opt_rebalancing, function(x) {
    inherits(x, "optimize.portfolio") &&
      is.numeric(x$weights) && all(is.finite(x$weights))
  }, logical(1))
  expect_false(any(usable))

  # And whichever route it took, no period carries a solver status code, which
  # is precisely why criteria 3 and 4 have to work from symptoms.
  opts_only <- Filter(function(x) inherits(x, "optimize.portfolio"),
                      opt$opt_rebalancing)
  expect_true(all(vapply(opts_only, function(x) is.null(x$status), logical(1))))

  # The diagnostic calls every one of them.
  expect_true(all(diagnose_rebalancing(opt)$status == "failed"))
})

test_that("a feasible rolling optimisation comes back entirely converged", {
  skip_if_not_installed("PortfolioAnalytics")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()

  set.seed(1602)
  n <- 60
  R <- xts::xts(
    matrix(stats::rnorm(n * 4, 0.006, 0.04), n, 4,
           dimnames = list(NULL, c("a", "b", "c", "d"))),
    order.by = seq(as.Date("2015-01-31"), by = "month", length.out = n)
  )

  good <- PortfolioAnalytics::portfolio.spec(colnames(R))
  good <- PortfolioAnalytics::add.constraint(good, "weight_sum",
                                             min_sum = 1 - 1e-6,
                                             max_sum = 1 + 1e-6)
  good <- PortfolioAnalytics::add.constraint(good, "long_only")
  good <- PortfolioAnalytics::add.objective(good, type = "risk", name = "StdDev")

  opt <- PortfolioAnalytics::optimize.portfolio.rebalancing(
    R, good, optimize_method = "ROI",
    rebalance_on = "years", training_period = 24, rolling_window = 23
  )

  out <- diagnose_rebalancing(opt)
  expect_equal(nrow(out), length(opt$opt_rebalancing))
  expect_true(all(out$status == "converged"))
  expect_equal(out$weights_sum, rep(1, nrow(out)), tolerance = 1e-4)
  expect_true(all(is.na(out$message)))
})

test_that("min_sum == max_sum is what breaks quadprog, and it is silent", {
  # The defect pm_backtest()'s sum_tolerance argument exists for. gmv_opt()
  # hands quadprog sum(w) >= 1 and -sum(w) >= -1, two rows active at the same
  # point, and quadprog calls a plainly feasible problem inconsistent. ROI
  # reports the status; PortfolioAnalytics does not read it.
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_on_cran()

  risky <- subset(sim_class_returns_monthly, !grepl("_money_market$", class))
  R  <- as_xts_pm(risky, date, class, return)
  N  <- ncol(R)

  # gmv_opt()'s constraint block, reproduced exactly: a vacuous zero equality
  # row, the weight sum as two opposing inequalities, the box twice over.
  op <- function(S, min_sum, max_sum) {
    ROI::OP(
      objective   = ROI::Q_objective(Q = 2 * S, L = rep(0, N)),
      constraints = ROI::L_constraint(
        L   = rbind(rep(0, N), rep(1, N), rep(-1, N), diag(N), -diag(N)),
        dir = c("==", rep(">=", 2 + 2 * N)),
        rhs = c(0, min_sum, -max_sum, rep(0, N), rep(-1, N))
      ),
      bounds = ROI::V_bound(li = seq_len(N), lb = rep(0, N),
                            ui = seq_len(N), ub = rep(1, N))
    )
  }

  # Which windows fail depends on the covariance, so it depends on the version
  # of the simulated market. Find one rather than pinning a date: an earlier
  # version of this test named 2001-07-31, which stopped failing when the data
  # was regenerated, and the test then failed for a reason that had nothing to
  # do with what it is about.
  first_failing <- NULL
  for (e in seq.int(60L, nrow(R))) {
    S <- stats::var(zoo::coredata(R[(e - 59):e, ]))
    r <- ROI::ROI_solve(op(S, 1, 1), solver = "quadprog")
    if (r$status$code != 0L || anyNA(r$solution)) {
      first_failing <- e
      break
    }
  }
  skip_if(is.null(first_failing),
          "no 60-month window on this data trips the degenerate encoding")

  S <- stats::var(zoo::coredata(R[(first_failing - 59):first_failing, ]))
  tight <- ROI::ROI_solve(op(S, 1, 1), solver = "quadprog")
  loose <- ROI::ROI_solve(op(S, 1 - 1e-6, 1 + 1e-6), solver = "quadprog")

  # ROI knows perfectly well that it failed.
  expect_true(tight$status$code != 0L)
  expect_true(anyNA(tight$solution))
  expect_match(tight$status$msg$message, "inconsistent")

  # Separating the pair by a millionth is all it takes.
  expect_equal(loose$status$code, 0L)
  expect_false(anyNA(loose$solution))
  expect_equal(sum(loose$solution), 1, tolerance = 1e-5)
})
