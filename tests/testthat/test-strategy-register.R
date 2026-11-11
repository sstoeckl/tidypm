spec_fun <- function(assets) {
  p <- PortfolioAnalytics::portfolio.spec(assets)
  p <- PortfolioAnalytics::add.constraint(p, "full_investment")
  p <- PortfolioAnalytics::add.constraint(p, "long_only")
  PortfolioAnalytics::add.objective(p, type = "risk", name = "StdDev")
}


# ---------------------------------------------------------------------------
# pm_strategy()
# ---------------------------------------------------------------------------

test_that("a strategy is a pure value with the documented fields", {
  s <- pm_strategy("gmv", spec = spec_fun, label = "Minimum variance")

  expect_s3_class(s, "pm_strategy")
  expect_named(s, c("name", "label", "spec", "optimize_method", "weight_fun",
                    "metric", "excess_returns", "args"))
  expect_equal(s$name, "gmv")
  expect_equal(s$label, "Minimum variance")
  expect_equal(s$optimize_method, "ROI")
  expect_null(s$weight_fun)
  expect_null(s$metric)              # inferred at run time
  expect_false(s$excess_returns)
  expect_identical(s$args, list())
})

test_that("extra arguments are stored for the optimiser, not evaluated", {
  s <- pm_strategy("msr", spec = spec_fun, maxSR = TRUE, foo = "bar")
  expect_equal(s$args, list(maxSR = TRUE, foo = "bar"))
})

test_that("supplying a weight function clears optimize_method", {
  s <- pm_strategy("ew", spec = spec_fun,
                   weight_fun = PortfolioAnalytics::equal.weight)
  expect_null(s$optimize_method)
  expect_true(is.function(s$weight_fun))
})

test_that("a strategy needs an engine of some kind", {
  expect_error(
    pm_strategy("x", spec = spec_fun, optimize_method = NULL),
    "either an"
  )
})

test_that("bad arguments are refused with an explanation", {
  expect_error(pm_strategy(1, spec_fun), "non-empty string")
  expect_error(pm_strategy(c("a", "b"), spec_fun), "non-empty string")
  expect_error(pm_strategy("", spec_fun), "non-empty string")
  expect_error(pm_strategy("x", spec = "not a spec"), "portfolio")
  expect_error(pm_strategy("x", spec = function() 1), "first argument")
  expect_error(pm_strategy("x", spec_fun, weight_fun = "nope"), "must be a function")
  expect_error(pm_strategy("x", spec_fun, metric = "alpha"), "metric columns")
  expect_error(pm_strategy("x", spec_fun, excess_returns = NA), "TRUE")
  expect_error(pm_strategy("x", spec_fun, excess_returns = 1), "TRUE")
})

test_that("a fixed portfolio object is accepted as well as a function", {
  p <- spec_fun(letters[1:4])
  s <- pm_strategy("fixed", spec = p)
  expect_s3_class(s$spec, "portfolio")
})


# ---------------------------------------------------------------------------
# pm_register()
# ---------------------------------------------------------------------------

test_that("a register is named by strategy name and composes", {
  a <- pm_strategy("a", spec_fun)
  b <- pm_strategy("b", spec_fun)
  reg <- pm_register(a, b)

  expect_s3_class(reg, "pm_register")
  expect_named(reg, c("a", "b"))
  expect_length(reg, 2L)

  # Registers compose: a chapter adds one strategy to the book's five.
  reg2 <- pm_register(reg, pm_strategy("c", spec_fun))
  expect_named(reg2, c("a", "b", "c"))
})

test_that("duplicate names are refused, because the name is the join key", {
  a <- pm_strategy("a", spec_fun)
  expect_error(pm_register(a, a), "unique")
  expect_error(pm_register(pm_register(a), pm_strategy("a", spec_fun)), "unique")
})

test_that("a register only holds strategies", {
  expect_error(pm_register(pm_strategy("a", spec_fun), 42), "pm_strategy")
  expect_error(pm_register(list(1, 2)), "must be a")
})

test_that("subsetting a register keeps it a register and refuses bad names", {
  reg <- pm_register(pm_strategy("a", spec_fun), pm_strategy("b", spec_fun))

  sub <- reg["a"]
  expect_s3_class(sub, "pm_register")
  expect_named(sub, "a")

  expect_error(reg["zzz"], "does not hold")
  expect_error(reg[character(0)], "no strategies")
})

test_that("print methods return their input invisibly", {
  skip_if_not_installed("withr")
  quiet <- function(expr) {
    suppressMessages(withr::with_output_sink(tempfile(), expr))
  }
  s <- pm_strategy("a", spec_fun)
  expect_identical(quiet(print(s)), s)
  reg <- pm_register(s)
  expect_identical(quiet(print(reg)), reg)
})


# ---------------------------------------------------------------------------
# pm_book_strategies()
# ---------------------------------------------------------------------------

test_that("the book register holds the documented five", {
  reg <- pm_book_strategies()
  expect_s3_class(reg, "pm_register")
  expect_named(reg, c("equal_weight", "inverse_vol", "min_variance",
                      "max_sharpe", "min_cvar"))

  # The two rules use PortfolioAnalytics' own exported weight functions rather
  # than hand-written ones.
  expect_identical(reg$equal_weight$weight_fun, PortfolioAnalytics::equal.weight)
  expect_identical(reg$inverse_vol$weight_fun,
                   PortfolioAnalytics::inverse.volatility.weight)

  # The metric matches the objective, which is the point of the register.
  expect_equal(reg$min_variance$metric, "ann_vol")
  expect_equal(reg$max_sharpe$metric, "sharpe")
  expect_equal(reg$min_cvar$metric, "es_95")

  # Max Sharpe needs both PortfolioAnalytics' switch and excess returns.
  expect_true(reg$max_sharpe$args$maxSR)
  expect_true(reg$max_sharpe$excess_returns)
})

test_that("every book strategy builds a portfolio for any universe", {
  reg <- pm_book_strategies()
  for (nm in names(reg)) {
    p <- reg[[nm]]$spec(c("a", "b", "c"))
    expect_s3_class(p, "portfolio")
    expect_length(p$assets, 3L)
  }
  # And for the real one.
  p <- reg$min_cvar$spec(sim_assets$asset_id)
  expect_length(p$assets, nrow(sim_assets))
})

test_that("box_max and es_p are arguments, and they bite", {
  capped <- pm_book_strategies(box_max = 0.25)
  cons <- capped$min_variance$spec(letters[1:4])$constraints
  box  <- Filter(function(c) inherits(c, "box_constraint"), cons)[[1]]
  expect_equal(as.numeric(box$max), rep(0.25, 4))

  tail99 <- pm_book_strategies(es_p = 0.99)
  obj <- Filter(function(o) o$name == "ES",
                tail99$min_cvar$spec(letters[1:4])$objectives)[[1]]
  expect_equal(obj$arguments$p, 0.99)

  expect_error(pm_book_strategies(box_max = 0), "in \\(0, 1\\]")
  expect_error(pm_book_strategies(box_max = 2), "in \\(0, 1\\]")
  expect_error(pm_book_strategies(es_p = 0.05), "in \\(0.5, 1\\)")
  expect_error(pm_book_strategies(es_p = 1), "in \\(0.5, 1\\)")
})


# ---------------------------------------------------------------------------
# metric inference
# ---------------------------------------------------------------------------

test_that("the reported metric is inferred from the objectives", {
  base <- function() {
    p <- PortfolioAnalytics::portfolio.spec(letters[1:4])
    PortfolioAnalytics::add.constraint(p, "full_investment")
  }
  sd_only   <- PortfolioAnalytics::add.objective(base(), type = "risk", name = "StdDev")
  mean_only <- PortfolioAnalytics::add.objective(base(), type = "return", name = "mean")
  both      <- PortfolioAnalytics::add.objective(mean_only, type = "risk", name = "StdDev")
  es_only   <- PortfolioAnalytics::add.objective(base(), type = "risk", name = "ES",
                                                 arguments = list(p = 0.95))

  expect_equal(pm_infer_metric(sd_only,   list()), "ann_vol")
  expect_equal(pm_infer_metric(mean_only, list()), "ann_return")
  expect_equal(pm_infer_metric(both,      list()), "sharpe")
  expect_equal(pm_infer_metric(es_only,   list()), "es_95")
  expect_equal(pm_infer_metric(base(),    list()), "sharpe")

  # maxSR wins over the objective pair, and an ES objective wins over maxSR:
  # the tail is what a tail optimisation was asked for.
  expect_equal(pm_infer_metric(sd_only, list(maxSR = TRUE)), "sharpe")
  expect_equal(pm_infer_metric(es_only, list(maxSR = TRUE)), "es_95")
})
