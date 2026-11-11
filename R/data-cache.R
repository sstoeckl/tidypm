#' Where tidypm keeps its data
#'
#' A single directory on the user's machine, outside the package and outside any
#' project, so that one download serves every chapter and every project. Nothing
#' here is shipped with the package or committed to the book's repository.
#'
#' @return A length-one character vector: the absolute path to the cache
#'   directory. The directory is created on first call if it does not exist, so
#'   the return value always names a directory that is there. The path is
#'   `Sys.getenv("TIDYPM_CACHE")` when that is set and non-empty, and
#'   `tools::R_user_dir("tidypm", "cache")` otherwise.
#'
#' @section Pointing it somewhere else:
#' Set the environment variable `TIDYPM_CACHE` before the first call — in
#' `.Renviron`, or with [base::Sys.setenv()] in a script. This is the supported way to
#' give a course, a container or a test run its own isolated cache, and it is how
#' this package's own tests avoid touching yours.
#'
#' @examples
#' # The default location on this machine.
#' tools::R_user_dir("tidypm", which = "cache")
#'
#' # Redirect it, which is what a reproducible script or a test should do.
#' old <- Sys.getenv("TIDYPM_CACHE", unset = NA)
#' Sys.setenv(TIDYPM_CACHE = file.path(tempdir(), "tidypm-example"))
#'
#' pm_cache_dir()
#' dir.exists(pm_cache_dir())
#'
#' if (is.na(old)) Sys.unsetenv("TIDYPM_CACHE") else Sys.setenv(TIDYPM_CACHE = old)
#'
#' @family tidypm data layer
#' @export
pm_cache_dir <- function() {
  d <- Sys.getenv("TIDYPM_CACHE", unset = "")
  if (!nzchar(d)) d <- tools::R_user_dir("tidypm", which = "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}


#' The book's data cutoff
#'
#' A `Date` vector of length one, `2025-12-31`. The default value of the `cutoff`
#' argument of [pm_init()], kept as an exported constant so a chapter can name it
#' rather than repeat the literal, and so a reader can see the assumption instead
#' of inferring it.
#'
#' @rdname pm_init
#' @export
PM_CUTOFF <- as.Date("2025-12-31")


#' Download the book's data once
#'
#' `pm_init()` builds the asset-class panel the book is written against and
#' stores it in [pm_cache_dir()]. Run it once; every chapter then reads from the
#' cache with [pm_data()]. `PM_CUTOFF` is the date it stops at by default.
#'
#' Nothing is shipped with the package and nothing is committed to the book's
#' repository. The data is rebuilt from its public sources on your machine, which
#' keeps the licences clean and means you can see exactly where every number came
#' from.
#'
#' @param cutoff Last date to keep, as a `Date` or anything [base::as.Date()] accepts.
#'   Defaults to `PM_CUTOFF`, chosen so that every annual table is a complete
#'   calendar year and no chapter carries a stub year. Pass `NULL` for everything
#'   the sources have — useful for showing what a ragged tail does to a backtest,
#'   which is what chapter 22 does deliberately.
#' @param force Single logical. Rebuild, and overwrite, even if a cached panel
#'   already exists. `FALSE` (the default) makes the function a no-op when the
#'   cache is present, which is why chapters can call it freely.
#' @param quiet Single logical. Suppress the progress messages. The return value
#'   is unaffected.
#'
#' @return Invisibly, a length-one character vector: the path to
#'   `tier1.rds` inside [pm_cache_dir()]. Called for its side effect, which is
#'   writing that file. The file holds a list of two elements:
#'
#'   \describe{
#'     \item{`panel`}{A tibble with a `date` column and one column per
#'       implemented sleeve, monthly simple returns in decimal form. This is what
#'       [pm_data()] returns.}
#'     \item{`manifest`}{A list with `built_on` (the build date as a string),
#'       `tidypm` (the package version) and `sleeves`, a named character vector
#'       holding `"<non-missing observations>:<12-character hash>"` per sleeve.
#'       This is what [pm_check()] compares.}
#'   }
#'
#'   Sleeves declared in [pm_sources()] with `implemented = FALSE` are not
#'   fetched and do not appear as columns; their names are listed in a warning
#'   unless `quiet = TRUE`.
#'
#' @section Reproducing the book's numbers:
#' The book was built against a specific download date, recorded in its manifest.
#' Kenneth French and FRED both revise history occasionally, so a rebuild today
#' may not be identical to the book's build.
#'
#' `pm_init()` therefore records a manifest — source, download date, row count and
#' a hash of each series — and [pm_check()] compares yours against the book's. If
#' they differ you get told which series moved and by how much, rather than
#' discovering it as an unexplained mismatch three chapters later.
#'
#' That is the honest version of reproducibility for data that is not ours to
#' redistribute: the pipeline is reproducible, and any divergence is visible.
#'
#' @section What it downloads:
#' Kenneth French's data library through \pkg{FFdownload}, FRED through its
#' public CSV endpoint, and one mutual-fund price history through \pkg{tidyquant}.
#' No credentials, no API keys. It takes a minute or two on a normal connection
#' and needs network access; [pm_data()] afterwards does not.
#'
#' @examples
#' # The cutoff is a plain Date, exposed so a reader can move it.
#' PM_CUTOFF
#'
#' \dontrun{
#' # Needs network access. Run once.
#' pm_init()
#'
#' # Everything the sources have, ragged tail included.
#' pm_init(cutoff = NULL, force = TRUE)
#'
#' returns <- pm_data()
#' }
#'
#' @family tidypm data layer
#' @seealso [pm_data()] to read the result, [pm_check()] to compare it against
#'   the book's build, [pm_sources()] for what goes into it.
#' @export
#' @rdname pm_init
pm_init <- function(cutoff = PM_CUTOFF, force = FALSE, quiet = FALSE) {

  path <- file.path(pm_cache_dir(), "tier1.rds")

  if (file.exists(path) && !force) {
    if (!quiet) {
      cli::cli_alert_info("Panel already cached at {.path {path}}.")
      cli::cli_alert_info("Use {.code pm_init(force = TRUE)} to rebuild.")
    }
    return(invisible(path))
  }

  if (!quiet) cli::cli_h1("Building the asset-class panel")

  sleeves <- pm_sources()
  built <- list()
  notes <- character(0)

  for (nm in names(sleeves)) {
    src <- sleeves[[nm]]
    if (!isTRUE(src$implemented)) {
      notes <- c(notes, nm)
      next
    }
    # — is an em dash, written as an escape rather than as the character
    # itself because R CMD check requires R code to be ASCII. The string the
    # parser produces is byte-for-byte the same either way.
    if (!quiet) cli::cli_alert("{nm} \u2014 {src$description}")
    built[[nm]] <- src$fetch()
  }

  panel <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "date"),
    built
  )
  panel <- dplyr::arrange(panel, .data$date)

  # Sources publish at different speeds -- French runs a couple of months behind
  # FRED -- so without a cutoff the last rows of the panel silently carry fewer
  # assets than the rest. Cutting at a calendar year end removes that and keeps
  # every annual table a complete year.
  if (!is.null(cutoff)) panel <- panel[panel$date <= as.Date(cutoff), ]

  manifest <- list(
    built_on = as.character(Sys.Date()),
    tidypm   = tryCatch(as.character(utils::packageVersion("tidypm")),
                        error = function(e) "dev"),
    sleeves  = vapply(names(built), function(nm) {
      v <- panel[[nm]]
      paste0(sum(!is.na(v)), ":", substr(rlang::hash(v[!is.na(v)]), 1, 12))
    }, character(1))
  )

  saveRDS(list(panel = panel, manifest = manifest), path)

  if (!quiet) {
    cli::cli_alert_success(
      "{nrow(panel)} months, {ncol(panel) - 1} sleeve{?s}, {min(panel$date)} to {max(panel$date)}."
    )
    if (length(notes)) {
      cli::cli_alert_warning("Not built yet: {.val {notes}}.")
    }
    cli::cli_alert_info("Cached at {.path {path}}.")
  }

  invisible(path)
}


#' Read the cached panel
#'
#' Reads the file [pm_init()] wrote. No network access, no recomputation: this is
#' a disk read and three subsets, so a chapter can call it as often as it likes.
#'
#' @param sleeves Optional character vector of sleeve names, selecting a subset
#'   of columns. `date` is always returned and need not be named. Columns come
#'   back in the order given, so this is also how you fix column order before
#'   [as_xts_pm()]. An unrecognised name is an error listing what is available —
#'   nothing is silently dropped. `NULL` (the default) returns every sleeve.
#' @param from,to Optional inclusive date bounds, as `Date` or anything
#'   [base::as.Date()] accepts, e.g. `"1990-07-01"`. `NULL` (the default) means no
#'   bound on that side. Rows are filtered, never padded or interpolated.
#'
#' @return A tibble with a `date` column (`Date`, month-end, ascending) and one
#'   column per selected sleeve, each a `numeric` vector of **monthly simple
#'   returns in decimal form** — `0.0123` is 1.23 per cent. One row per month
#'   present in the cache after filtering; zero rows if the bounds exclude
#'   everything, which is not an error.
#'
#'   **The panel is deliberately ragged.** A sleeve whose source does not reach
#'   the start of the panel is `NA` until it begins, and no function here pads,
#'   fills or drops those rows for you. Chapter 5 shows what that does to a
#'   covariance estimate and chapter 22 what it does to a backtest.
#'
#' @section Errors:
#' If no panel has been cached, `pm_data()` aborts with `"No cached panel found."`
#' and tells you to run [pm_init()] once. It does not download on your behalf: a
#' function that silently reaches the network is a function that behaves
#' differently on a plane.
#'
#' @examples
#' # Runnable without a download: plant a two-month panel in a temporary cache.
#' old <- Sys.getenv("TIDYPM_CACHE", unset = NA)
#' Sys.setenv(TIDYPM_CACHE = file.path(tempdir(), "tidypm-example"))
#'
#' panel <- tibble::tibble(
#'   date      = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
#'   us_equity = c(-0.0011, -0.0813, -0.1338),
#'   gov_long  = c( 0.0295,  0.0308,  0.0037),
#'   cash      = c( 0.0013,  0.0012,  0.0008)
#' )
#' saveRDS(list(panel = panel, manifest = list()),
#'         file.path(pm_cache_dir(), "tier1.rds"))
#'
#' pm_data()
#' pm_data(sleeves = c("us_equity", "gov_long"), from = "2020-02-01")
#'
#' # An unknown sleeve is refused, and the message says what exists.
#' try(pm_data(sleeves = "gold"))
#'
#' Sys.unsetenv("TIDYPM_CACHE")
#' if (!is.na(old)) Sys.setenv(TIDYPM_CACHE = old)
#'
#' \dontrun{
#' # The real thing, once pm_init() has run.
#' pm_data()
#' pm_data(sleeves = c("us_equity", "gov_long"), from = "1990-07-01")
#' }
#'
#' @family tidypm data layer
#' @seealso [pm_init()] to build the cache, [as_xts_pm()] to hand the result to
#'   \pkg{PortfolioAnalytics}.
#' @export
pm_data <- function(sleeves = NULL, from = NULL, to = NULL) {

  path <- file.path(pm_cache_dir(), "tier1.rds")

  if (!file.exists(path)) {
    cli::cli_abort(c(
      "No cached panel found.",
      "i" = "Run {.code pm_init()} once to build it.",
      "i" = "It downloads from Kenneth French's library and FRED, and takes a minute or two."
    ))
  }

  out <- readRDS(path)$panel

  if (!is.null(sleeves)) {
    unknown <- setdiff(sleeves, names(out))
    if (length(unknown)) {
      cli::cli_abort(c(
        "Unknown sleeve{?s}: {.val {unknown}}.",
        "i" = "Available: {.val {setdiff(names(out), 'date')}}."
      ))
    }
    out <- out[, c("date", sleeves)]
  }
  if (!is.null(from)) out <- out[out$date >= as.Date(from), ]
  if (!is.null(to))   out <- out[out$date <= as.Date(to), ]

  out
}


#' Compare your build against the book's
#'
#' Kenneth French and FRED both revise history. A panel rebuilt today is
#' therefore not guaranteed to equal the one the book's numbers were computed on.
#' `pm_check()` says so out loud, per sleeve, rather than leaving you to discover
#' it as an unexplained mismatch three chapters later.
#'
#' @param reference Optional reference manifest: a list with a `built_on` string
#'   and a named character vector `sleeves`, in the format [pm_init()] writes
#'   (`"<non-missing observations>:<12-character hash>"` per sleeve). `NULL`, the
#'   default, loads the manifest the book was built with from
#'   `system.file("manifest", "book.rds", package = "tidypm")`.
#'
#' @return Invisibly, a `data.frame` with one row per sleeve in `reference` and
#'   two columns: `sleeve` (character) and `matches` (logical, `FALSE` for a
#'   sleeve that differs *and* for a sleeve absent from your build). Returns
#'   `NULL` invisibly when no reference is available. Called for the message it
#'   prints, not for the value.
#'
#' @section Not yet shipped:
#' No reference manifest is bundled with this version of the package. With
#' `reference = NULL` and nothing to compare against, `pm_check()` reports when
#' your panel was built and says plainly that it has no reference — it does not
#' quietly return success. Pass a manifest explicitly to compare two builds of
#' your own.
#'
#' @section Errors:
#' Aborts with `"No cached panel."` if [pm_init()] has never run.
#'
#' @examples
#' # Runnable without a download: plant a manifest, then compare it with itself
#' # and with a build in which one sleeve has moved.
#' old <- Sys.getenv("TIDYPM_CACHE", unset = NA)
#' Sys.setenv(TIDYPM_CACHE = file.path(tempdir(), "tidypm-example-check"))
#'
#' mine <- list(
#'   built_on = "2026-01-15",
#'   tidypm   = "0.0.0.9000",
#'   sleeves  = c(us_equity = "426:1a2b3c4d5e6f", cash = "426:0f9e8d7c6b5a")
#' )
#' saveRDS(list(panel = NULL, manifest = mine),
#'         file.path(pm_cache_dir(), "tier1.rds"))
#'
#' # No reference is bundled yet, so this reports rather than compares.
#' pm_check()
#'
#' # Against itself: everything matches.
#' pm_check(reference = mine)
#'
#' # Against a build where the equity sleeve was revised.
#' moved <- mine
#' moved$sleeves[["us_equity"]] <- "426:ffffffffffff"
#' pm_check(reference = moved)
#'
#' Sys.unsetenv("TIDYPM_CACHE")
#' if (!is.na(old)) Sys.setenv(TIDYPM_CACHE = old)
#'
#' @family tidypm data layer
#' @seealso [pm_init()], which writes the manifest this reads.
#' @export
pm_check <- function(reference = NULL) {

  path <- file.path(pm_cache_dir(), "tier1.rds")
  if (!file.exists(path)) {
    cli::cli_abort("No cached panel. Run {.code pm_init()} first.")
  }

  mine <- readRDS(path)$manifest

  if (is.null(reference)) {
    ref_path <- system.file("manifest", "book.rds", package = "tidypm")
    if (!nzchar(ref_path)) {
      cli::cli_alert_info("Built on {.val {mine$built_on}} with tidypm {mine$tidypm}.")
      cli::cli_alert_warning("No reference manifest shipped yet, so nothing to compare against.")
      return(invisible(NULL))
    }
    reference <- readRDS(ref_path)
  }

  same <- mine$sleeves[names(reference$sleeves)] == reference$sleeves
  same[is.na(same)] <- FALSE

  if (all(same)) {
    cli::cli_alert_success("Your panel matches the book's build of {.val {reference$built_on}}.")
  } else {
    cli::cli_alert_warning(
      "{sum(!same)} sleeve{?s} differ{?s/} from the book's build of {.val {reference$built_on}}: {.val {names(same)[!same]}}"
    )
    cli::cli_alert_info("Sources revise history. Your numbers will differ slightly from the printed ones.")
  }

  invisible(data.frame(
    sleeve = names(same),
    matches = as.logical(same),
    row.names = NULL
  ))
}
