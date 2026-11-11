test_that("the default schedule is complete, positive and correctly ordered", {
  cs <- pm_costs_default()

  expect_type(cs, "double")
  expect_true(all(nzchar(names(cs))))
  expect_true(all(c("government", "money_market", "commodity", "equity",
                    "credit", ".default") %in% names(cs)))
  expect_true(all(cs > 0))

  # The ordering is the whole point of not using a flat rate: government cheap,
  # credit dear, equity in between.
  expect_lt(cs[["money_market"]], cs[["government"]] + 1e-12)
  expect_lt(cs[["government"]], cs[["commodity"]])
  expect_lt(cs[["commodity"]], cs[["equity"]])
  expect_lt(cs[["equity"]], cs[["credit"]])

  # Decimals, not basis points. A schedule silently in bp would be 10,000 times
  # too expensive and would still run.
  expect_true(all(cs < 0.05))
})

test_that("overrides replace and extend, and are validated", {
  expect_equal(pm_costs_default(credit = 0.004)[["credit"]], 0.004)
  expect_equal(pm_costs_default(US_government = 1e-5)[["US_government"]], 1e-5)
  # Overriding one leaves the rest alone.
  expect_equal(pm_costs_default(credit = 0.004)[["equity"]],
               pm_costs_default()[["equity"]])

  expect_error(pm_costs_default(0.004), "must be named")
  expect_error(pm_costs_default(credit = -1), "non-negative")
  expect_error(pm_costs_default(credit = c(0.001, 0.002)), "non-negative")
  expect_error(pm_costs_default(credit = "cheap"), "non-negative")
})

test_that("class tokens resolve the class-level universe", {
  assets <- c("US_equity", "EU_credit", "JP_government", "GLOBAL_commodity",
              "US_money_market")
  out <- pm_resolve_costs(assets, pm_costs_default())

  expect_named(out, assets)
  expect_equal(unname(out["US_equity"]), pm_costs_default()[["equity"]])
  expect_equal(unname(out["EU_credit"]), pm_costs_default()[["credit"]])
  # "money_market" must beat any shorter partial match.
  expect_equal(unname(out["US_money_market"]),
               pm_costs_default()[["money_market"]])
})

test_that("an exact asset name beats the class token", {
  cs <- pm_costs_default(US_equity = 1e-5)
  out <- pm_resolve_costs(c("US_equity", "EU_equity"), cs)
  expect_equal(unname(out["US_equity"]), 1e-5)
  expect_equal(unname(out["EU_equity"]), cs[["equity"]])
})

test_that("matching is anchored, so a token cannot match mid-name", {
  # "equity" must not price "equity_neutral_overlay": the token has to end the
  # name, otherwise a cost silently attaches to the wrong sleeve.
  cs  <- c(equity = 0.001, .default = 0.005)
  out <- pm_resolve_costs(c("US_equity", "equity_neutral"), cs)
  expect_equal(unname(out["US_equity"]), 0.001)
  expect_equal(unname(out["equity_neutral"]), 0.005)
})

test_that("an unmatched asset falls back, or errors when there is no fallback", {
  cs <- pm_costs_default()
  expect_equal(unname(pm_resolve_costs("US_EQU_01", cs)), cs[[".default"]])

  no_default <- cs[setdiff(names(cs), ".default")]
  expect_error(pm_resolve_costs("US_EQU_01", no_default), "No trading cost")

  na_default <- cs
  na_default[".default"] <- NA_real_
  expect_error(pm_resolve_costs("US_EQU_01", na_default), "No trading cost")
})

test_that("a malformed schedule is rejected", {
  expect_error(pm_resolve_costs("US_equity", c(0.001, 0.002)), "fully named")
  expect_error(pm_resolve_costs("US_equity", list(equity = 0.001)), "fully named")
  expect_error(pm_resolve_costs("US_equity", c(equity = -0.001)), "cannot be negative")
})

test_that("the recipe in the documentation works at instrument level", {
  cls <- pm_costs_default()
  per_asset <- stats::setNames(unname(cls[sim_assets$asset_class]),
                               sim_assets$asset_id)
  expect_false(anyNA(per_asset))
  expect_length(per_asset, nrow(sim_assets))

  out <- pm_resolve_costs(sim_assets$asset_id[1:5], per_asset)
  expect_false(anyNA(out))
  expect_equal(unname(out), unname(per_asset[1:5]))
})
