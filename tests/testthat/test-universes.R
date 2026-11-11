# pm_universe() and pm_screen().
#
# pm_universe() downloads on a cold cache and reads from disk on a warm one, so
# the two branches are tested separately: the warm branch offline against a
# planted cache file, the cold branch behind skip_if_no_net().

# A stand-in for a French universe file, already in decimal form, as
# pm_universe() writes it.
fake_universe <- function(n_cols = 3) {
  set.seed(1602)
  # Month ends built from the first of each month, because seq() by month from a
  # 31st rolls over: seq(as.Date("2019-12-31"), by = "month")[3] is 2020-03-02,
  # and anchoring that to a month end collides with the following observation.
  dates <- zoo::as.Date(
    zoo::as.yearmon(seq(as.Date("2020-01-01"), by = "month", length.out = 12)),
    frac = 1
  )
  stopifnot(!anyDuplicated(dates))
  out <- tibble::tibble(date = dates)
  for (i in seq_len(n_cols)) {
    out[[paste0("P", i)]] <- round(stats::rnorm(length(dates), 0.008, 0.05), 6)
  }
  out
}

plant_universe <- function(dir, which = "industries10", weighting = "value",
                           data = fake_universe()) {
  saveRDS(data,
          file.path(dir, paste0("universe_", which, "_", weighting, ".rds")))
  invisible(data)
}

# --- argument validation, before anything is fetched -------------------------

test_that("pm_universe() rejects an unknown universe", {
  local_pm_cache()

  # match.arg()'s message is locale-dependent, so match the choices it lists.
  expect_error(pm_universe("industries20"), "industries10")
  expect_error(pm_universe("sp500"), "size_bm100")
})

test_that("pm_universe() rejects an unknown weighting", {
  local_pm_cache()
  expect_error(pm_universe("industries10", weighting = "cap"), "equal")
})

test_that("pm_universe() validates before it touches the cache or the network", {
  # An empty cache and a bad argument: the error must be about the argument.
  local_pm_cache()
  expect_error(pm_universe("nonsense"), "industries49")
})

test_that("the three universes are the documented ladder", {
  expect_equal(eval(formals(pm_universe)$which),
               c("industries10", "industries49", "size_bm100"))
  expect_equal(eval(formals(pm_universe)$weighting), c("value", "equal"))
})

# --- the warm-cache branch, offline ------------------------------------------

test_that("pm_universe() reads the cache without downloading", {
  dir <- local_pm_cache()
  planted <- plant_universe(dir)

  expect_silent(out <- pm_universe("industries10"))
  expect_identical(out, planted)
})

test_that("pm_universe() returns ascending dates even from a scrambled cache", {
  # The @return promises "month-end, ascending", and everything downstream reads
  # the result as a time series without checking. The universes come through the
  # same FFdownload parser as the Tier 1 sleeves, so they get the same explicit
  # sort -- applied after the cache read as well, so a file written by an older
  # version is normalised on the way out rather than trusted.
  dir <- local_pm_cache()
  planted <- fake_universe()
  plant_universe(dir, data = planted[rev(seq_len(nrow(planted))), ])

  out <- pm_universe("industries10")

  expect_false(is.unsorted(out$date))
  expect_equal(out, planted)
})

test_that("value and equal weighting are cached separately", {
  dir <- local_pm_cache()
  plant_universe(dir, weighting = "value", data = fake_universe(2))
  plant_universe(dir, weighting = "equal", data = fake_universe(4))

  expect_equal(ncol(pm_universe("industries10", weighting = "value")), 3L)
  expect_equal(ncol(pm_universe("industries10", weighting = "equal")), 5L)
})

test_that("each universe has its own cache file", {
  dir <- local_pm_cache()
  plant_universe(dir, which = "industries10", data = fake_universe(2))

  # industries49 is not planted, so asking for it must not silently serve the
  # industries10 file.
  expect_equal(ncol(pm_universe("industries10")), 3L)
  expect_false(file.exists(file.path(dir, "universe_industries49_value.rds")))
})

test_that("from and to filter inclusively and never re-download", {
  dir <- local_pm_cache()
  planted <- plant_universe(dir)
  cut <- planted$date[4]

  expect_equal(nrow(pm_universe("industries10", from = cut)), 9L)
  expect_equal(nrow(pm_universe("industries10", to   = cut)), 4L)
  expect_equal(min(pm_universe("industries10", from = cut)$date), cut)
  expect_equal(max(pm_universe("industries10", to   = cut)$date), cut)
})

test_that("bounds that exclude everything give zero rows, not an error", {
  dir <- local_pm_cache()
  plant_universe(dir)

  out <- pm_universe("industries10", from = "2100-01-01")
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(fake_universe()))
})

test_that("filtering does not change the columns", {
  dir <- local_pm_cache()
  planted <- plant_universe(dir)

  expect_named(pm_universe("industries10", from = "2020-06-30"), names(planted))
})

# --- the cold-cache branch, network ------------------------------------------

test_that("pm_universe() downloads, converts to decimal and caches", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")

  dir <- local_pm_cache()
  path <- file.path(dir, "universe_industries10_value.rds")
  expect_false(file.exists(path))

  out <- suppressMessages(pm_universe("industries10"))

  expect_true(file.exists(path))
  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 11L)               # date plus ten industries
  expect_s3_class(out$date, "Date")
  expect_equal(min(out$date), as.Date("1926-07-31"))

  # Decimal, not percent. French publishes percent and the conversion happens
  # once, here; a monthly industry return above 100 per cent is possible but
  # above 1000 per cent is a scaling bug.
  expect_lt(max(abs(unlist(out[, -1])), na.rm = TRUE), 1.5)
  expect_lt(stats::sd(out[[2]], na.rm = TRUE), 0.2)

  # Month-end anchored, one row per month, sorted.
  expect_true(all(out$date == zoo::as.Date(zoo::as.yearmon(out$date), frac = 1)))
  expect_false(anyDuplicated(out$date) > 0)
  expect_false(is.unsorted(out$date))

  # The second call is a disk read.
  expect_silent(again <- pm_universe("industries10"))
  expect_identical(out, again)
})

test_that("industries49 names its columns, and pm_screen() finds them", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")

  dir <- local_pm_cache()
  ind <- suppressMessages(pm_universe("industries49"))

  expect_equal(ncol(ind), 50L)
  # The whole reason chapter 10 can express a screen as column names.
  expect_true(all(pm_screen() %in% names(ind)))
})

# --- pm_screen() --------------------------------------------------------------

test_that("pm_screen() returns the documented sets", {
  expect_equal(pm_screen("sin"),     c("Smoke", "Beer", "Guns", "Fun"))
  expect_equal(pm_screen("fossil"),  c("Coal", "Oil", "Mines"))
  expect_equal(pm_screen("weapons"), c("Guns", "Aero", "Ships"))
})

test_that("no argument is the widest screen, not an empty one", {
  # match.arg(several.ok = TRUE) on the full default vector returns everything.
  # A reader who calls pm_screen() and excludes the result removes nine
  # industries, not zero.
  expect_setequal(pm_screen(), c("Smoke", "Beer", "Guns", "Fun",
                                 "Coal", "Oil", "Mines", "Aero", "Ships"))
  expect_length(pm_screen(), 9L)
})

test_that("overlapping screens de-duplicate", {
  # Guns is in both "sin" and "weapons".
  both <- pm_screen(c("sin", "weapons"))

  expect_length(both, 6L)
  expect_equal(sum(both == "Guns"), 1L)
  expect_false(anyDuplicated(both) > 0)
})

test_that("the set does not depend on request order but the order does", {
  # Pinned because it is a real trap: two screens that select the same
  # industries are not identical(), so a test or a cache key built on identity
  # would spuriously differ.
  a <- pm_screen(c("sin", "weapons"))
  b <- pm_screen(c("weapons", "sin"))

  expect_setequal(a, b)
  expect_false(identical(a, b))
})

test_that("pm_screen() returns an unnamed character vector", {
  out <- pm_screen("fossil")

  expect_type(out, "character")
  expect_null(names(out))
})

test_that("pm_screen() rejects a screen it does not define", {
  expect_error(pm_screen("carbon"), "sin")
  expect_error(pm_screen(""), "weapons")
})

test_that("an unknown name mixed with a known one is silently dropped", {
  # Current behaviour, pinned rather than endorsed. match.arg(several.ok = TRUE)
  # errors only when NOTHING matches, so a typo alongside a valid screen narrows
  # the screen without saying so: pm_screen(c("sin", "fossl")) returns the sin
  # set alone. See REPORT.md -- this should become a cli_abort.
  expect_equal(pm_screen(c("sin", "carbon")), pm_screen("sin"))
  expect_equal(pm_screen(c("fossil", "nuclear")), pm_screen("fossil"))
})

test_that("partial matching works, as match.arg() promises", {
  expect_equal(pm_screen("fo"), pm_screen("fossil"))
})
