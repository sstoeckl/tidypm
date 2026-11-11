# Helpers shared by the test files.
#
# Every test that touches the cache redirects TIDYPM_CACHE into a temporary
# directory first. Without that the suite would read, and in places overwrite,
# the user's real panel -- and would pass or fail depending on whether they had
# run pm_init(). Tests must not depend on the machine they run on.

#' Redirect the tidypm cache into a temporary directory for the caller's scope
#'
#' Returns the directory. Cleaned up when the calling frame exits.
local_pm_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TIDYPM_CACHE = dir), .local_envir = env)
  dir
}

# A minimal but structurally honest panel: month-end dates, decimal returns,
# and one sleeve that starts late, because the real panel is ragged and any
# function that quietly assumes otherwise should fail here.
test_panel <- function() {
  tibble::tibble(
    date       = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31",
                           "2020-04-30", "2020-05-31")),
    us_equity  = c(-0.0011, -0.0813, -0.1338, 0.1365, 0.0558),
    gov_long   = c( 0.0295,  0.0308,  0.0037, 0.0021, 0.0009),
    cash       = c( 0.0013,  0.0012,  0.0008, 0.0000, 0.0001),
    high_yield = c(     NA,      NA, -0.1128, 0.0433, 0.0450)
  )
}

test_manifest <- function() {
  list(
    built_on = "2026-01-15",
    tidypm   = "0.0.0.9000",
    sleeves  = c(us_equity  = "5:1a2b3c4d5e6f",
                 gov_long   = "5:0f9e8d7c6b5a",
                 cash       = "5:aabbccddeeff",
                 high_yield = "3:112233445566")
  )
}

# Plant a cache file of the shape pm_init() writes, without downloading.
write_test_cache <- function(dir, panel = test_panel(), manifest = test_manifest()) {
  saveRDS(list(panel = panel, manifest = manifest), file.path(dir, "tier1.rds"))
  invisible(file.path(dir, "tier1.rds"))
}

# Long-format returns for the coercion tests. Month ends are written out rather
# than generated: seq(as.Date("2020-01-31"), by = "month") returns 2020-03-02.
test_long <- function() {
  data.frame(
    date  = rep(as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30")),
                each = 2),
    asset = rep(c("equity", "bonds"), times = 4),
    ret   = c(0.02, 0.01, -0.01, 0.00, 0.03, 0.01, 0.01, -0.02)
  )
}

# Network tests skip loudly rather than pass quietly.
skip_if_no_net <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("curl")
  testthat::skip_if_offline()
}
