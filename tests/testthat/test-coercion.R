# as_xts_pm() / as_pm(): the boundary between tidy returns and the wide xts
# matrix PortfolioAnalytics insists on. The contract is a round trip that
# preserves values and an index that is always sorted; the interesting tests are
# the ones where the object comes back looking correct and is not.

# --- shape -------------------------------------------------------------------

test_that("as_xts_pm() produces a wide xts with one column per asset", {
  r <- as_xts_pm(test_long(), date, asset, ret)

  expect_s3_class(r, "xts")
  expect_equal(ncol(r), 2L)
  expect_equal(nrow(r), 4L)
  expect_setequal(colnames(r), c("equity", "bonds"))
  expect_true(is.numeric(zoo::coredata(r)))
})

test_that("as_pm() returns exactly three columns in date-major order", {
  out <- as_pm(as_xts_pm(test_long(), date, asset, ret))

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("date", "asset", "value"))
  expect_equal(nrow(out), 8L)
  expect_type(out$asset, "character")
  # date-major: each date appears once per asset, consecutively
  expect_equal(out$date[1:2], rep(min(test_long()$date), 2))
})

test_that("the index class survives both directions", {
  r <- as_xts_pm(test_long(), date, asset, ret)
  expect_s3_class(zoo::index(r), "Date")
  expect_s3_class(as_pm(r)$date, "Date")

  # A POSIXct index is carried through unchanged rather than truncated to a day.
  stamps <- as.POSIXct(c("2020-01-31 16:00", "2020-02-28 16:00"), tz = "UTC")
  x <- xts::xts(matrix(1:4, 2, 2, dimnames = list(NULL, c("a", "b"))),
                order.by = stamps)
  expect_s3_class(as_pm(x)$date, "POSIXct")
  expect_equal(sort(unique(as_pm(x)$date)), stamps)
})

test_that("column names need not be date, asset and value", {
  renamed <- stats::setNames(test_long(), c("month", "sleeve", "r"))
  r <- as_xts_pm(renamed, month, sleeve, r)

  expect_s3_class(r, "xts")
  expect_setequal(colnames(r), c("equity", "bonds"))
})

test_that("extra columns are dropped rather than pivoted", {
  extra <- test_long()
  extra$note <- "ignore me"

  r <- as_xts_pm(extra, date, asset, ret)
  expect_equal(ncol(r), 2L)
})

# --- the property that must hold ---------------------------------------------

test_that("the round trip preserves every value", {
  long <- test_long()
  back <- as_pm(as_xts_pm(long, date, asset, ret))

  joined <- merge(long, back, by = c("date", "asset"))

  expect_equal(nrow(joined), nrow(long))
  expect_equal(joined$ret, joined$value)
})

test_that("the index is sorted whatever order the input arrived in", {
  scrambled <- test_long()[c(5, 2, 8, 1, 6, 3, 7, 4), ]
  r <- as_xts_pm(scrambled, date, asset, ret)

  expect_false(is.unsorted(zoo::index(r)))
  # ignore_attr: an xts index carries tclass and tzone attributes that a bare
  # Date vector does not.
  expect_equal(zoo::index(r), sort(unique(test_long()$date)), ignore_attr = TRUE)
})

test_that("column order survives the round trip", {
  r <- as_xts_pm(test_long(), date, asset, ret)
  expect_equal(colnames(r), unique(as_pm(r)$asset))
})

# --- the edge cases that break it --------------------------------------------

test_that("a ragged panel warns rather than padding silently", {
  ragged <- test_long()[-3, ]

  expect_warning(as_xts_pm(ragged, date, asset, ret), "Padded")

  r <- suppressWarnings(as_xts_pm(ragged, date, asset, ret))
  expect_equal(sum(is.na(r)), 1L)
  # The gap is left as NA. Nothing is filled, carried forward or dropped.
  expect_equal(nrow(r), 4L)
})

test_that("column order depends on the row order of the input", {
  # This is the failure mode the documentation warns about, pinned so that a
  # future change to it is a deliberate one. A weight vector fitted on one
  # ordering and applied to the other misassigns every weight.
  long <- test_long()

  a <- as_xts_pm(long, date, asset, ret)
  b <- as_xts_pm(long[order(long$asset), ], date, asset, ret)

  expect_equal(colnames(a), c("equity", "bonds"))
  expect_equal(colnames(b), c("bonds", "equity"))
  expect_setequal(colnames(a), colnames(b))
})

test_that("duplicate (date, asset) pairs are not silently aggregated", {
  dup <- rbind(test_long(), test_long()[1, ])

  # tidyr warns about the list-column, and the call then fails in xts(). What
  # matters is that no aggregation happens behind the reader's back.
  expect_error(suppressWarnings(as_xts_pm(dup, date, asset, ret)))
})

test_that("a single asset and a single date still produce a matrix", {
  one <- test_long()[test_long()$asset == "equity", ]
  r <- as_xts_pm(one, date, asset, ret)

  expect_equal(dim(r), c(4L, 1L))
  expect_equal(colnames(r), "equity")
  expect_equal(nrow(as_pm(r)), 4L)
})

# --- the error a wrong input must raise --------------------------------------

test_that("as_pm() refuses anything that is not xts", {
  # The message is locale-dependent because it comes from stopifnot(), so match
  # the expression rather than the prose.
  expect_error(as_pm(1:3), "is.xts")
  expect_error(as_pm(test_long()), "is.xts")
  expect_error(as_pm(NULL), "is.xts")
})

test_that("as_xts_pm() refuses a column that is not there", {
  expect_error(as_xts_pm(test_long(), date, asset, missing_column))
})
