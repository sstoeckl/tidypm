# bond_total_return() / credit_total_return(). Two sleeves of the Tier 1 panel
# are constructed by these functions rather than downloaded, so their errors are
# not visible anywhere downstream. The tests below pin the properties a bond desk
# would recognise, not the arithmetic.

# --- properties that must hold -----------------------------------------------

test_that("a flat yield curve returns the coupon", {
  # If the yield never moves, a par bond earns exactly its coupon: one month of
  # carry and no price change.
  y <- rep(0.04, 13)
  r <- bond_total_return(y, maturity = 10)

  expect_true(is.na(r[1]))
  expect_equal(r[-1], rep(0.04 / 12, 12), tolerance = 1e-8)
})

test_that("a flat year sums to the yield and compounds above it", {
  # Worth pinning because the difference trips people up: on a flat curve the
  # bond earns y/12 each month, so the twelve returns SUM to y but COMPOUND to
  # more, the gap being the reinvestment of monthly carry. The function returns
  # simple monthly returns; anything that annualises them has to say which.
  y <- rep(0.05, 13)
  r <- bond_total_return(y, maturity = 5)[-1]

  expect_equal(sum(r), 0.05, tolerance = 1e-8)
  expect_equal(prod(1 + r) - 1, (1 + 0.05 / 12)^12 - 1, tolerance = 1e-8)
  expect_gt(prod(1 + r) - 1, sum(r))
})

test_that("the flat-curve result does not depend on maturity", {
  # Carry is carry. If this ever stops holding, the repricing has picked up a
  # maturity-dependent bias that would show as a spurious term premium.
  flat <- vapply(c(1, 2, 5, 10, 30),
                 function(m) bond_total_return(rep(0.04, 3), maturity = m)[2],
                 numeric(1))
  expect_equal(flat, rep(0.04 / 12, 5), tolerance = 1e-9)
})

test_that("a rate rise hurts the long bond more", {
  up <- c(0.03, 0.04)

  short <- bond_total_return(up, maturity = 1)[2]
  long  <- bond_total_return(up, maturity = 10)[2]

  expect_lt(long, short)
  expect_lt(long, 0)
})

test_that("a rate fall helps the long bond more", {
  down <- c(0.04, 0.03)

  short <- bond_total_return(down, maturity = 1)[2]
  long  <- bond_total_return(down, maturity = 10)[2]

  expect_gt(long, short)
  expect_gt(long, 0)
})

test_that("return is monotone decreasing in the yield change", {
  # Same starting yield, worsening rate moves: returns must fall, strictly.
  moves <- seq(-0.005, 0.005, by = 0.001)
  r <- vapply(moves,
              function(d) bond_total_return(c(0.04, 0.04 + d), maturity = 10)[2],
              numeric(1))

  expect_true(all(diff(r) < 0))
})

test_that("the implied duration is close to the analytical par-bond duration", {
  # A small yield shock should produce a return of roughly -D_mod * dy, where the
  # modified duration of a par bond is [1 - (1 + y/2)^(-2T)] / y.
  y0 <- 0.04
  dy <- 0.0001
  maturity <- 10

  r <- bond_total_return(c(y0, y0 + dy), maturity = maturity)[2]
  carry <- y0 / 12

  d_implied <- -(r - carry) / dy
  d_analytic <- (1 - (1 + y0 / 2)^(-2 * maturity)) / y0

  expect_equal(d_implied, d_analytic, tolerance = 0.05)
})

test_that("implied duration rises with maturity", {
  dy <- 0.0001
  d <- vapply(c(1, 2, 5, 10, 30), function(m) {
    r <- bond_total_return(c(0.04, 0.04 + dy), maturity = m)[2]
    -(r - 0.04 / 12) / dy
  }, numeric(1))

  expect_true(all(diff(d) > 0))
})

test_that("coupon frequency changes the answer, but only slightly", {
  # freq is an assumption, exposed as an argument. It should matter, and it
  # should matter less than the yield move does.
  semi <- bond_total_return(c(0.04, 0.045), maturity = 10, freq = 2)[2]
  ann  <- bond_total_return(c(0.04, 0.045), maturity = 10, freq = 1)[2]

  expect_false(isTRUE(all.equal(semi, ann)))
  expect_lt(abs(semi - ann), 0.005)
})

# --- edge cases --------------------------------------------------------------

test_that("a zero yield does not divide by zero", {
  r <- bond_total_return(c(0, 0, 0), maturity = 5)
  expect_true(all(is.finite(r[-1])))
  expect_equal(r[-1], c(0, 0), tolerance = 1e-8)
})

test_that("negative yields are priced rather than rejected", {
  # They happened, so refusing them would make the euro-area chapters
  # unwritable. A yield rise from -1% to -0.5% must still lose money.
  r <- bond_total_return(c(-0.01, -0.005), maturity = 10)

  expect_true(is.finite(r[2]))
  expect_lt(r[2], 0)
})

test_that("a single observation cannot produce a return", {
  expect_true(is.na(bond_total_return(0.04, maturity = 10)))
  expect_length(bond_total_return(0.04, maturity = 10), 1L)
})

test_that("an empty input returns an empty vector, not an error", {
  expect_equal(bond_total_return(numeric(0), maturity = 10), numeric(0))
})

test_that("NA propagates forwards and backwards by exactly one month", {
  # An NA yield kills the return of its own month (no closing price) and of the
  # next (no opening yield), and nothing further. Anything that silently filled
  # the gap would show up here.
  r <- bond_total_return(c(0.04, NA, 0.04, 0.04), maturity = 10)

  expect_true(is.na(r[1]))   # no previous yield
  expect_true(is.na(r[2]))   # closing yield missing
  expect_true(is.na(r[3]))   # opening yield missing
  expect_false(is.na(r[4]))  # recovered
})

test_that("a fractional maturity is allowed", {
  r <- bond_total_return(c(0.04, 0.041), maturity = 2.5)
  expect_true(is.finite(r[2]))
})

test_that("the output is the same length and order as the input", {
  y <- c(0.04, 0.042, 0.039, 0.041, 0.045)
  expect_length(bond_total_return(y, maturity = 10), length(y))
  # Reversing the input reverses which month is which, so the answers differ.
  expect_false(isTRUE(all.equal(
    bond_total_return(y, maturity = 10)[-1],
    rev(bond_total_return(rev(y), maturity = 10)[-1])
  )))
})

# --- errors ------------------------------------------------------------------

test_that("percent input is rejected rather than silently mis-scaled", {
  expect_error(bond_total_return(c(4.0, 4.1), maturity = 10), "decimal")
  # The message names the offending value and the fix.
  expect_error(bond_total_return(c(4.0, 4.1), maturity = 10), "4\\.1")
})

test_that("non-numeric input is refused", {
  expect_error(bond_total_return("0.04", maturity = 10), "must be numeric")
  expect_error(bond_total_return(as.Date("2020-01-31"), maturity = 10),
               "must be numeric")
})

test_that("maturity must be one value, longer than a month", {
  expect_error(bond_total_return(c(0.04, 0.04), maturity = c(1, 10)),
               "single value")
  expect_error(bond_total_return(c(0.04, 0.04), maturity = 1 / 24),
               "single value")
  expect_error(bond_total_return(c(0.04, 0.04), maturity = 0), "single value")
  expect_error(bond_total_return(c(0.04, 0.04), maturity = -5), "single value")
})

test_that("a missing or non-finite maturity gets the intended message", {
  # Without an is.finite() guard these reach `if (NA <= 1/12)`, which raises R's
  # own "missing value where TRUE/FALSE needed". That is a message about the
  # implementation, not about what the caller did wrong.
  expect_error(bond_total_return(c(0.04, 0.041), maturity = NA), "single value")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = NA_real_),
               "single value")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = NaN), "single value")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = Inf), "single value")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = "10"), "single value")

  # Specifically the package's condition, not R's bare one.
  expect_error(bond_total_return(c(0.04, 0.041), maturity = NA),
               class = "rlang_error")
  msg <- tryCatch(bond_total_return(c(0.04, 0.041), maturity = NA),
                  error = conditionMessage)
  expect_false(grepl("missing value where", msg, fixed = TRUE))
})

test_that("freq must be a single positive number of coupons", {
  expect_error(bond_total_return(c(0.04, 0.041), maturity = 10, freq = 0),
               "freq")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = 10, freq = -2),
               "freq")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = 10, freq = NA),
               "freq")
  expect_error(bond_total_return(c(0.04, 0.041), maturity = 10, freq = c(1, 2)),
               "freq")
})

test_that("a yield at or below -freq is refused rather than returning NaN", {
  # Below that bound 1 + yield/freq is not positive, and a non-positive base
  # raised to the fractional exponent -freq * maturity is NaN. The sleeve would
  # come back empty -- fred_bond() drops NA rows -- with nothing said.
  expect_error(bond_total_return(c(0.04, -2), maturity = 10, freq = 2), "-freq")
  expect_error(bond_total_return(c(0.04, -2.5), maturity = 10, freq = 2), "-freq")

  # The bound moves with freq: an annual-coupon market is stricter.
  expect_error(bond_total_return(c(0.04, -1), maturity = 10, freq = 1), "-freq")
  expect_error(bond_total_return(c(0.04, -1.5), maturity = 10, freq = 1), "-freq")

  # The message names the offending value.
  expect_error(bond_total_return(c(0.04, -2.5), maturity = 10), "2\\.5")
})

test_that("the lower bound does not touch yields anyone has actually seen", {
  # This is the check that would break the euro-area chapters if it were drawn
  # on sign rather than on arithmetic. -1% and even -5% are nowhere near -freq.
  expect_true(is.finite(bond_total_return(c(-0.01, -0.005), maturity = 10)[2]))
  expect_true(is.finite(bond_total_return(c(-0.05, -0.04), maturity = 10)[2]))
  expect_true(is.finite(bond_total_return(c(-0.05, -0.04), maturity = 10,
                                          freq = 1)[2]))
})

# --- credit ------------------------------------------------------------------

test_that("credit_total_return subtracts the loss assumption", {
  y <- rep(0.055, 5)

  gross <- credit_total_return(y, annual_loss = 0)
  net   <- credit_total_return(y, annual_loss = 0.006)

  expect_equal(gross[-1] - net[-1], rep(0.006 / 12, 4), tolerance = 1e-10)
})

test_that("the loss assumption is a level shift and nothing else", {
  # This is the criticism the documentation makes of the function, made
  # executable: a constant loss cannot change the risk of the sleeve, which is
  # exactly why it understates credit in a crisis.
  y <- c(0.055, 0.058, 0.052, 0.061, 0.057)

  gross <- credit_total_return(y, annual_loss = 0)[-1]
  net   <- credit_total_return(y, annual_loss = 0.02)[-1]

  expect_equal(stats::sd(gross), stats::sd(net), tolerance = 1e-12)
  expect_equal(mean(gross) - mean(net), 0.02 / 12, tolerance = 1e-12)
})

test_that("credit defaults to a twenty-year maturity", {
  y <- c(0.055, 0.058)
  expect_equal(credit_total_return(y),
               bond_total_return(y, maturity = 20),
               tolerance = 1e-12)
})

test_that("credit_total_return inherits the yield validation", {
  expect_error(credit_total_return(c(5.5, 5.6)), "decimal")
  expect_error(credit_total_return(c(0.055, 0.058), maturity = 0), "single value")
})

test_that("zero loss is the gross series exactly", {
  y <- c(0.055, 0.058, 0.052)
  expect_identical(credit_total_return(y, annual_loss = 0),
                   bond_total_return(y, maturity = 20))
})
