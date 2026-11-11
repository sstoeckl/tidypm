# pm_sources(): the sleeve registry, and the internal fetchers behind it.
#
# The registry itself is pure data and is tested offline. The fetchers reach the
# network and are gated by skip_if_no_net(), so a failed download shows as a skip
# or a failure, never as a pass.

# --- the registry -------------------------------------------------------------

test_that("pm_sources() is a named list of sleeve definitions", {
  src <- pm_sources()

  expect_type(src, "list")
  expect_true(length(src) > 0)
  expect_false(any(names(src) == ""))
  expect_false(anyDuplicated(names(src)) > 0)
  expect_true(all(vapply(src, is.list, logical(1))))
})

test_that("every sleeve declares a description and an implemented flag", {
  src <- pm_sources()

  expect_true(all(vapply(src, function(s) is.character(s$description), logical(1))))
  expect_true(all(vapply(src, function(s) nzchar(s$description), logical(1))))
  expect_true(all(vapply(src, function(s) is.logical(s$implemented), logical(1))))
  expect_false(any(vapply(src, function(s) is.na(s$implemented), logical(1))))
})

test_that("implemented sleeves have a fetch function and unimplemented ones do not", {
  # The contract pm_init() relies on: it calls src$fetch() for exactly those
  # sleeves flagged TRUE. A TRUE with no fetch would be an error at build time,
  # halfway through a two-minute download.
  src <- pm_sources()

  for (nm in names(src)) {
    s <- src[[nm]]
    if (isTRUE(s$implemented)) {
      expect_true(is.function(s$fetch), info = nm)
      expect_length(formals(s$fetch), 0L)
    } else {
      expect_null(s$fetch, info = nm)
    }
  }
})

test_that("the registry declares the gaps rather than hiding them", {
  src <- pm_sources()
  unimplemented <- names(Filter(function(s) !isTRUE(s$implemented), src))

  # A sleeve the book has decided not to build yet is still listed, so a reader
  # can see what is missing from pm_data() without reading pm_init().
  #
  # `commodities` is here on purpose and has been deleted once already. The
  # decision recorded in data/README.md is "deferred, not blocked": no free
  # source reaches 1990, and the two ways round that both cost more than they
  # are worth today. Deleting the entry turns a documented decision into an
  # invisible one, which is the failure mode the registry exists to prevent.
  expect_true(length(unimplemented) > 0)
  expect_true(all(c("commodities", "real_estate", "dev_ex_us_hedged", "em_hedged") %in%
                    unimplemented))
})

test_that("the eight Tier 1 sleeves the book is written against are implemented", {
  implemented <- names(Filter(function(s) isTRUE(s$implemented), pm_sources()))

  expect_setequal(
    implemented,
    c("us_equity", "dev_ex_us_equity", "em_equity", "cash",
      "gov_short", "gov_long", "ig_credit", "high_yield")
  )
})

test_that("pm_sources() reaches no network and returns the same thing twice", {
  expect_lt(system.time(pm_sources())[["elapsed"]], 1)
  a <- pm_sources()
  b <- pm_sources()
  expect_equal(
    vapply(a, function(s) s$description, character(1)),
    vapply(b, function(s) s$description, character(1))
  )
})

# --- row order, the silent one ------------------------------------------------
#
# All three month-end pickers keep the LAST ROW of a month and call it the month
# end. That is the last OBSERVATION only if the rows arrive in date order, which
# no source promises: FRED's CSV endpoint, Yahoo and the FFdownload parser all
# happen to be sorted today, and none of them is contractually obliged to stay
# that way. The failure would be silent -- a plausible-looking sleeve built from
# mid-month yields -- so it is pinned here rather than trusted.
#
# These tests construct their input and mock the download, so they run offline
# and cannot be skipped away by a network check.

# Six daily yields spanning two months. Chosen so that the month-end pair
# (4.50 -> 4.10) and the mid-month pair (4.20 -> 4.30) imply ten-year returns of
# OPPOSITE SIGN, roughly +3.6% against -0.5%. A row-order bug cannot hide inside
# rounding.
order_test_yields <- function() {
  tibble::tibble(
    date  = as.Date(c("2024-03-01", "2024-03-15", "2024-03-28",
                      "2024-04-02", "2024-04-16", "2024-04-30")),
    value = c(4.00, 4.20, 4.50, 4.60, 4.30, 4.10)
  )
}

test_that("fred_month_end() does not depend on the order FRED sends its rows", {
  raw       <- order_test_yields()
  scrambled <- raw[c(3, 1, 2, 6, 4, 5), ]

  pick <- function(d) {
    local_mocked_bindings(fred_series = function(id) d)
    fred_month_end("DGS10")
  }

  ordered  <- pick(raw)
  shuffled <- pick(scrambled)

  # Invariance, and the right observation rather than merely a stable one.
  expect_equal(shuffled, ordered)
  expect_equal(ordered$value, c(4.50, 4.10))
  expect_equal(ordered$date, as.Date(c("2024-03-31", "2024-04-30")))
})

test_that("a shuffled FRED response cannot move a constructed bond return", {
  # The consequence, end to end. This is the number the sort protects.
  raw       <- order_test_yields()
  scrambled <- raw[c(3, 1, 2, 6, 4, 5), ]

  build <- function(d) {
    local_mocked_bindings(fred_series = function(id) d)
    fred_bond("DGS10", maturity = 10, name = "gov_long")
  }

  ordered  <- build(raw)
  shuffled <- build(scrambled)

  expect_equal(shuffled, ordered)
  expect_equal(nrow(ordered), 1L)
  # 4.50% -> 4.10% on a ten-year sleeve is a gain of roughly 3.6%. Reading the
  # mid-month yields instead (4.20% -> 4.30%) would give about -0.5%.
  expect_gt(ordered$gov_long, 0.03)
  expect_lt(ordered$gov_long, 0.04)
})

test_that("yahoo_fund() does not depend on the order Yahoo sends its rows", {
  skip_if_not_installed("tidyquant")

  raw <- tibble::tibble(
    date     = as.Date(c("2024-03-01", "2024-03-15", "2024-03-28",
                         "2024-04-02", "2024-04-16", "2024-04-30")),
    adjusted = c(100, 104, 102, 103, 108, 105)
  )
  scrambled <- raw[c(5, 2, 6, 1, 4, 3), ]

  build <- function(d) {
    local_mocked_bindings(tq_get = function(x, ...) d, .package = "tidyquant")
    yahoo_fund("VWEHX", "high_yield")
  }

  ordered  <- build(raw)
  shuffled <- build(scrambled)

  expect_equal(shuffled, ordered)
  # March ends at 102 and April at 105; the mid-month prices are higher, so an
  # unsorted read would invent a loss where there was a gain.
  expect_equal(ordered$date, as.Date("2024-04-30"))
  expect_equal(ordered$high_yield, 105 / 102 - 1)
})

test_that("ff_get_monthly() returns its months in ascending order", {
  skip_if_not_installed("FFdownload")

  raw <- tibble::tibble(
    date   = zoo::as.yearmon(as.Date(c("2024-01-15", "2024-02-15", "2024-03-15"))),
    Mkt.RF = c(1.0, -2.0, 3.0),
    RF     = c(0.4, 0.4, 0.4)
  )
  scrambled <- raw[c(3, 1, 2), ]

  # ff_get_monthly() memoises by dataset name, so each call needs its own key and
  # the keys have to be cleared or they leak into the network tests.
  withr::defer(
    suppressWarnings(rm(list = intersect(c("ordered_ds", "shuffled_ds"), ls(.ff_cache)),
                        envir = .ff_cache))
  )

  ordered <- local({
    local_mocked_bindings(FFget = function(...) raw, .package = "FFdownload")
    ff_get_monthly("ordered_ds")
  })
  shuffled <- local({
    local_mocked_bindings(FFget = function(...) scrambled, .package = "FFdownload")
    ff_get_monthly("shuffled_ds")
  })

  expect_equal(shuffled, ordered)
  expect_false(is.unsorted(shuffled$date))
  # ff_market() and ff_cash() read these columns straight through, so a
  # scrambled parse would scramble the equity and cash sleeves month for month.
  expect_equal(shuffled$Mkt.RF, c(1.0, -2.0, 3.0))
})

# --- FRED, the cheap network path --------------------------------------------

test_that("fred_series() returns a clean two-column tibble", {
  skip_if_no_net()

  d <- fred_series("DGS10")

  expect_s3_class(d, "tbl_df")
  expect_named(d, c("date", "value"))
  expect_s3_class(d$date, "Date")
  expect_type(d$value, "double")
  expect_false(any(is.na(d$value)))     # FRED's "." rows are dropped
  expect_false(is.unsorted(d$date))
  # DGS10 is daily from 1962. Anything much shorter means the endpoint changed.
  expect_gt(nrow(d), 10000)
  expect_lt(min(d$date), as.Date("1963-01-01"))
})

test_that("fred_month_end() keeps one observation per month, the last", {
  skip_if_no_net()

  d <- fred_month_end("DGS10")

  expect_named(d, c("date", "value"))
  expect_false(anyDuplicated(format(d$date, "%Y-%m")) > 0)
  # Dates are anchored to the month end, not the last trading day.
  expect_true(all(d$date == zoo::as.Date(zoo::as.yearmon(d$date), frac = 1)))
  expect_gt(nrow(d), 700)
})

test_that("fred_bond() produces decimal monthly returns with no leading NA", {
  skip_if_no_net()

  d <- fred_bond("DGS10", maturity = 10, name = "gov_long")

  expect_named(d, c("date", "gov_long"))
  expect_false(any(is.na(d$gov_long)))
  expect_lt(max(abs(d$gov_long)), 0.5)
  # 2022 was the worst year for a ten-year Treasury sleeve in the sample.
  y2022 <- d$gov_long[format(d$date, "%Y") == "2022"]
  expect_length(y2022, 12L)
  expect_lt(prod(1 + y2022) - 1, -0.10)
})

test_that("fred_credit() nets off the default-loss assumption it documents", {
  skip_if_no_net()

  gross <- fred_credit("BAA", name = "ig_credit", annual_loss = 0)
  net   <- fred_credit("BAA", name = "ig_credit", annual_loss = 0.004)

  expect_equal(nrow(gross), nrow(net))
  expect_equal(mean(gross$ig_credit) - mean(net$ig_credit), 0.004 / 12,
               tolerance = 1e-12)
})

# --- Kenneth French -----------------------------------------------------------

test_that("ff_market() and ff_cash() return decimal monthly series from 1926", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")

  mkt <- ff_market()
  expect_named(mkt, c("date", "us_equity"))
  expect_s3_class(mkt$date, "Date")
  expect_lt(min(mkt$date), as.Date("1927-01-01"))
  # Decimal, not percent. French publishes percent; if this ever slips the whole
  # panel is out by a factor of a hundred and nothing else would notice.
  expect_lt(max(abs(mkt$us_equity), na.rm = TRUE), 1.5)
  expect_gt(stats::sd(mkt$us_equity, na.rm = TRUE), 0.02)

  cash <- ff_cash()
  expect_named(cash, c("date", "cash"))
  expect_true(all(cash$cash > -0.001, na.rm = TRUE))
  expect_lt(max(cash$cash, na.rm = TRUE), 0.02)
})

test_that("the French download is cached in-session, not fetched twice", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")

  ff_market()                     # warms .ff_cache
  second <- system.time(ff_cash())[["elapsed"]]

  # Both sleeves come out of F-F_Research_Data_Factors. The second call must be
  # a lookup, not a download.
  expect_lt(second, 1)
})

test_that("ff_region() names its column after the sleeve", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")

  d <- ff_region("Developed_ex_US_3_Factors", "dev_ex_us_equity")

  expect_named(d, c("date", "dev_ex_us_equity"))
  # This dataset is the reason the panel starts in 1990-07.
  expect_equal(min(d$date), as.Date("1990-07-31"))
  expect_lt(max(abs(d$dev_ex_us_equity), na.rm = TRUE), 1)
})

# --- Yahoo, the one fragile source -------------------------------------------

test_that("yahoo_fund() returns month-end simple returns", {
  skip_if_no_net()
  skip_if_not_installed("tidyquant")

  d <- yahoo_fund("VWEHX", "high_yield", from = "2000-01-01")

  expect_named(d, c("date", "high_yield"))
  expect_false(any(is.na(d$high_yield)))
  expect_false(anyDuplicated(format(d$date, "%Y-%m")) > 0)
  expect_lt(max(abs(d$high_yield)), 0.5)
  # The documented caveat, made executable: this managed fund understates the
  # 2008 drawdown against the index's roughly -26%.
  y2008 <- d$high_yield[format(d$date, "%Y") == "2008"]
  expect_length(y2008, 12L)
  expect_gt(prod(1 + y2008) - 1, -0.26)
  expect_lt(prod(1 + y2008) - 1, -0.15)
})
