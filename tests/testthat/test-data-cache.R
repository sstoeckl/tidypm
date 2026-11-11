# pm_cache_dir(), PM_CUTOFF, pm_init(), pm_data(), pm_check().
#
# Every test here redirects TIDYPM_CACHE into a temporary directory first, so
# the suite neither reads nor overwrites the user's real panel. Tests that need
# the network are gated by skip_if_no_net() and say so; nothing here passes
# quietly because a download failed.

# --- pm_cache_dir() -----------------------------------------------------------

test_that("pm_cache_dir() honours TIDYPM_CACHE", {
  dir <- local_pm_cache()

  expect_equal(normalizePath(pm_cache_dir(), winslash = "/"),
               normalizePath(dir, winslash = "/"))
  expect_length(pm_cache_dir(), 1L)
  expect_type(pm_cache_dir(), "character")
})

test_that("pm_cache_dir() creates the directory it names", {
  parent <- withr::local_tempdir()
  target <- file.path(parent, "deep", "nested", "cache")
  withr::local_envvar(c(TIDYPM_CACHE = target))

  expect_false(dir.exists(target))
  expect_true(dir.exists(pm_cache_dir()))
})

test_that("pm_cache_dir() falls back to R_user_dir when the variable is empty", {
  # Unset and empty-string must behave the same; an empty TIDYPM_CACHE would
  # otherwise resolve to the working directory and scatter rds files into a
  # reader's project.
  withr::local_envvar(c(TIDYPM_CACHE = ""))
  expect_equal(pm_cache_dir(), tools::R_user_dir("tidypm", which = "cache"))
})

test_that("pm_cache_dir() is idempotent", {
  local_pm_cache()
  expect_identical(pm_cache_dir(), pm_cache_dir())
})

# --- PM_CUTOFF ----------------------------------------------------------------

test_that("PM_CUTOFF is a single date at a calendar year end", {
  expect_s3_class(PM_CUTOFF, "Date")
  expect_length(PM_CUTOFF, 1L)
  expect_equal(PM_CUTOFF, as.Date("2025-12-31"))
  # The reason it exists: no chapter should carry a stub year.
  expect_equal(format(PM_CUTOFF, "%m-%d"), "12-31")
})

test_that("PM_CUTOFF is the default cutoff of pm_init()", {
  expect_identical(eval(formals(pm_init)$cutoff), PM_CUTOFF)
})

# --- pm_init(), offline branches ---------------------------------------------

test_that("pm_init() is a no-op when the panel is already cached", {
  dir <- local_pm_cache()
  path <- write_test_cache(dir)
  before <- file.mtime(path)

  expect_message(res <- pm_init(), "already cached")
  expect_equal(normalizePath(res, winslash = "/"),
               normalizePath(path, winslash = "/"))
  expect_equal(file.mtime(path), before)   # nothing was rewritten
})

test_that("pm_init() returns the cache path invisibly", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_invisible(suppressMessages(pm_init()))
  expect_match(suppressMessages(pm_init()), "tier1\\.rds$")
})

test_that("pm_init(quiet = TRUE) says nothing at all", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_silent(pm_init(quiet = TRUE))
})

test_that("pm_init() does not reach the network when a cache exists", {
  # If the early return were ever removed, this would try to download and the
  # test would take a minute instead of a millisecond. Pinning it as a contract
  # rather than a timing accident.
  dir <- local_pm_cache()
  write_test_cache(dir)

  elapsed <- system.time(pm_init(quiet = TRUE))[["elapsed"]]
  expect_lt(elapsed, 2)
})

test_that("pm_init(force = TRUE) with no network fails rather than silently keeping the stale cache", {
  # force = TRUE means rebuild. It must not quietly fall back to the cached
  # panel, because a reader who asked for a rebuild would then believe they had
  # one. Verified by pointing FFdownload at a machine that is not there.
  skip_if_not_installed("FFdownload")
  dir <- local_pm_cache()
  write_test_cache(dir)

  # An unroutable proxy makes every HTTP call fail fast.
  withr::local_envvar(c(http_proxy  = "http://127.0.0.1:1",
                        https_proxy = "http://127.0.0.1:1",
                        no_proxy    = ""))

  expect_error(suppressMessages(pm_init(force = TRUE, quiet = TRUE)))
})

# --- pm_init(), the real build ------------------------------------------------

test_that("pm_init() builds a panel from its public sources", {
  skip_if_no_net()
  skip_if_not_installed("FFdownload")
  skip_if_not_installed("tidyquant")
  # Hits Kenneth French, FRED and Yahoo, takes a minute or two, and depends on
  # one undocumented endpoint. Opt in with TIDYPM_TEST_BUILD=true.
  skip_if_not(identical(tolower(Sys.getenv("TIDYPM_TEST_BUILD")), "true"),
              "set TIDYPM_TEST_BUILD=true to run the full download")

  dir <- local_pm_cache()
  path <- pm_init(quiet = TRUE)

  expect_true(file.exists(path))
  cached <- readRDS(path)

  expect_named(cached, c("panel", "manifest"))
  expect_s3_class(cached$panel, "tbl_df")
  expect_true("date" %in% names(cached$panel))

  implemented <- names(Filter(function(s) isTRUE(s$implemented), pm_sources()))
  expect_setequal(setdiff(names(cached$panel), "date"), implemented)

  # Monthly, decimal, cut at the book's cutoff.
  expect_lte(max(cached$panel$date), PM_CUTOFF)
  numeric_cols <- cached$panel[setdiff(names(cached$panel), "date")]
  expect_lt(max(abs(unlist(numeric_cols)), na.rm = TRUE), 1)

  # The manifest is what pm_check() compares.
  expect_named(cached$manifest, c("built_on", "tidypm", "sleeves"))
  expect_setequal(names(cached$manifest$sleeves), implemented)
  expect_true(all(grepl("^[0-9]+:[0-9a-f]{12}$", cached$manifest$sleeves)))
})

# --- pm_data() ----------------------------------------------------------------

test_that("pm_data() returns the cached panel unchanged", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  out <- pm_data()

  expect_s3_class(out, "tbl_df")
  expect_identical(out, test_panel())
  expect_s3_class(out$date, "Date")
  expect_false(is.unsorted(out$date))
})

test_that("pm_data() does not pad, fill or drop the ragged sleeve", {
  # The rule the data layer is built on: no function may hide a missing history.
  dir <- local_pm_cache()
  write_test_cache(dir)

  out <- pm_data()
  expect_equal(sum(is.na(out$high_yield)), 2L)
  expect_true(all(is.na(out$high_yield[1:2])))
  expect_equal(nrow(out), nrow(test_panel()))
})

test_that("pm_data(sleeves =) selects, and in the order asked for", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  out <- pm_data(sleeves = c("gov_long", "us_equity"))

  expect_named(out, c("date", "gov_long", "us_equity"))
  expect_equal(out$us_equity, test_panel()$us_equity)
})

test_that("pm_data() date bounds are inclusive on both sides", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_equal(nrow(pm_data(from = "2020-02-29")), 4L)
  expect_equal(nrow(pm_data(to   = "2020-02-29")), 2L)
  expect_equal(nrow(pm_data(from = "2020-02-29", to = "2020-04-30")), 3L)
  expect_equal(min(pm_data(from = "2020-03-01")$date), as.Date("2020-03-31"))
})

test_that("pm_data() accepts Date objects as well as strings", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_identical(pm_data(from = as.Date("2020-03-31")),
                   pm_data(from = "2020-03-31"))
})

test_that("an empty window is zero rows, not an error", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  out <- pm_data(from = "2030-01-01")
  expect_equal(nrow(out), 0L)
  expect_named(out, names(test_panel()))
})

test_that("pm_data() errors informatively when no cache exists", {
  local_pm_cache()   # empty temporary directory

  expect_error(pm_data(), "No cached panel found")
  expect_error(pm_data(), "pm_init\\(\\)")
  # It says what pm_init() will do, so the reader knows why it takes a minute.
  expect_error(pm_data(), "French")
})

test_that("pm_data() refuses an unknown sleeve and says what exists", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_error(pm_data(sleeves = "gold"), "Unknown sleeve")
  expect_error(pm_data(sleeves = "gold"), "gold")
  expect_error(pm_data(sleeves = "gold"), "us_equity")
  # One bad name in a good list is still an error; nothing is dropped quietly.
  expect_error(pm_data(sleeves = c("us_equity", "gold")), "Unknown sleeve")
})

# --- pm_check() ---------------------------------------------------------------

test_that("pm_check() errors when there is nothing to check", {
  local_pm_cache()
  expect_error(pm_check(), "No cached panel")
})

test_that("pm_check() reports rather than compares when no reference is shipped", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  # No manifest is bundled with this version, so the honest answer is "I cannot
  # tell you", not a green tick.
  expect_message(res <- pm_check(), "No reference manifest")
  expect_message(pm_check(), "2026-01-15")
  expect_null(res)
})

test_that("pm_check() confirms a manifest that matches", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_message(res <- pm_check(reference = test_manifest()), "matches")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("sleeve", "matches"))
  expect_true(all(res$matches))
  expect_equal(nrow(res), length(test_manifest()$sleeves))
})

test_that("pm_check() names the sleeves that moved", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  moved <- test_manifest()
  moved$sleeves[["us_equity"]] <- "5:ffffffffffff"

  expect_message(res <- pm_check(reference = moved), "us_equity")
  expect_message(pm_check(reference = moved), "differ")
  expect_false(res$matches[res$sleeve == "us_equity"])
  expect_true(all(res$matches[res$sleeve != "us_equity"]))
})

test_that("a sleeve missing from your build counts as a mismatch, not a match", {
  # NA must not be read as agreement. This is the case that would let a reader
  # believe a panel reproduced when a whole sleeve had failed to download.
  dir <- local_pm_cache()
  mine <- test_manifest()
  mine$sleeves <- mine$sleeves[c("us_equity", "cash")]
  write_test_cache(dir, manifest = mine)

  expect_message(res <- pm_check(reference = test_manifest()), "differ")
  expect_equal(sum(res$matches), 2L)
  expect_equal(nrow(res), 4L)
})

test_that("pm_check() returns its comparison invisibly", {
  dir <- local_pm_cache()
  write_test_cache(dir)

  expect_invisible(suppressMessages(pm_check(reference = test_manifest())))
})
