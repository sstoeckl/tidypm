#' Report which periods of a rolling optimisation actually solved
#'
#' [PortfolioAnalytics::optimize.portfolio.rebalancing()] carries on when a solver
#' fails in a given period. The returned object contains an element for that
#' period regardless, and nothing in it distinguishes a converged solve from a
#' failure. A backtest built on such a run looks entirely normal and is not.
#'
#' This is upstream issue #45. Until it is resolved, every backtest in the book
#' passes through this function before its results are reported.
#'
#' @param opt An object of class `optimize.portfolio.rebalancing`, as returned by
#'   [PortfolioAnalytics::optimize.portfolio.rebalancing()]. Its
#'   `opt_rebalancing` element is a named list, one entry per rebalancing date,
#'   the names being dates in `"YYYY-MM-DD"` form.
#' @param tol Numeric tolerance for the constraint checks in criteria 3 and 4,
#'   in weight units. Default `1e-6`. Loosen it for a metaheuristic backend,
#'   which does not satisfy an equality constraint exactly; tighten it for a
#'   convex one, which should.
#'
#' @return A tibble with one row per element of `opt$opt_rebalancing`, in the
#'   same order, and the columns
#'
#'   \describe{
#'     \item{`date`}{`Date`. The rebalancing date, parsed from the list name.}
#'     \item{`solver`}{Character. The optimisation backend, taken from the
#'       element's class (`optimize.portfolio.ROI` gives `"ROI"`), `NA` when the
#'       period is not an optimisation object at all.}
#'     \item{`status`}{Character, one of `"converged"`, `"suspect"` or
#'       `"failed"`. See the criteria below.}
#'     \item{`objective`}{Numeric. The value of the objective the solver
#'       minimised, from `$out`. `NA` where none survived.}
#'     \item{`n_assets`}{Integer. Assets with a non-zero weight, which for a
#'       long-only portfolio is the count actually held.}
#'     \item{`weights_sum`}{Numeric. `sum(weights)`, to be read against the
#'       specification's `min_sum` and `max_sum`.}
#'     \item{`message`}{Character. The condition message for a period that
#'       errored, the failing criterion for one that is flagged, `NA` for one
#'       that converged.}
#'   }
#'
#' @section Status:
#' Implemented. The four criteria below are executable and describe what the
#' function does; `tol` is the tolerance they are evaluated at.
#'
#' It aborts on malformed input rather than returning an empty tibble, a warning
#' or `NA`s on purpose. A diagnostic whose whole job is to say which periods
#' cannot be trusted is worse than useless if it can be run, ignored and mistaken
#' for a pass — a chapter would then report a backtest carrying a clean bill of
#' health it never earned. Failing loudly is the safe default for a function in
#' this position.
#'
#' @section The four criteria:
#' A period is classified by the first of these that fires. Criteria 1 and 2 are
#' definitive and give `"failed"`; criteria 3 and 4 are circumstantial and give
#' `"suspect"`. Anything untouched is `"converged"`.
#'
#' 1. **The period is not an optimisation.**
#'    `optimize.portfolio.rebalancing()` runs its periods through
#'    `foreach(..., .errorhandling = "pass")`, so a period whose solver throws
#'    leaves a **condition object** in `opt_rebalancing` rather than an
#'    `optimize.portfolio`. Nothing warns, and downstream extractors do not
#'    check. Test: `!inherits(x, "optimize.portfolio")`. Report
#'    `conditionMessage(x)`.
#'
#' 2. **The weights or the objective are not finite.**
#'    On an infeasible or non-positive-definite problem ROI's `quadprog` plugin
#'    returns a solution of `NA`s with a non-zero status code, and
#'    `PortfolioAnalytics:::gmv_opt()` copies `result$solution` into `$weights`
#'    without looking at `result$status`. The period then carries `weights` of
#'    `NA` and `out` of `NA`, silently, and `extractWeights()` returns a matrix of
#'    `NA`. Verified against `PortfolioAnalytics` 2.1.2. Test:
#'    `any(!is.finite(x$weights))` or `!is.finite(x$out)`.
#'
#'    Criteria 1 and 2 are two routes to the same place, and which one an
#'    infeasible problem takes depends on the ROI plugin that resolves it: a
#'    plugin that errors gives criterion 1, one that returns `NA`s gives
#'    criterion 2. Both were observed for the same specification on the same
#'    machine. An implementation must handle both, and the test file pins the
#'    upstream behaviour accordingly.
#'
#' 3. **The weights violate the specification.**
#'    Read `min_sum` and `max_sum` from the `weight_sum` (or `full_investment`)
#'    constraint of `x$portfolio$constraints`, and `min` and `max` from the `box`
#'    (or `long_only`) constraint. Flag when `sum(weights)` falls outside
#'    `[min_sum - tol, max_sum + tol]`, or any weight falls outside its box bound
#'    by more than `tol`. A converged solve satisfies its own constraints; one
#'    that does not was not a solve.
#'
#' 4. **The weights are a degenerate fallback.**
#'    Flag when the weight vector equals, to within `tol`, any of: the
#'    equal-weight vector `rep(1/N, N)`; the `weight_initial` of a turnover
#'    constraint, if there is one; or the previous period's weights, when the
#'    estimation window has moved and the moments therefore have not repeated.
#'    Several backends return their seed when they give up, and a metaheuristic
#'    returns the best of a population that may never have improved on it.
#'
#'    The last of the three carries the qualification in its own wording, and it
#'    is enforced: repeated weights are only flagged when the objective value
#'    **also** moved. If both the weights and the objective are unchanged, the
#'    solver was handed the same problem twice and gave the same answer twice,
#'    which is correct behaviour rather than a fallback. This is not hypothetical.
#'    A historical expected shortfall at 95% over 60 monthly observations is the
#'    mean of the three worst months; roll the window by one month and, unless the
#'    dropped or added month is one of those three, the linear programme is
#'    literally identical. On the book's 16 risky class series, 243 of 313
#'    minimum-CVaR periods repeat exactly, objective included — a fact about
#'    tail estimation on short samples, not about the solver.
#'
#' @section What cannot be checked, and why criteria 3 and 4 exist:
#' There is no solver status code on the returned object. `ROI::ROI_solve()`
#' reports one in `result$status$code`, and `PortfolioAnalytics` 2.1.2 discards it
#' — `gmv_opt()` and its siblings catch only a `try-error` and read
#' `result$solution` regardless of status. `DEoptim` and `random` backends store
#' no convergence flag either. So a non-converged solve cannot be detected
#' directly; it has to be caught by its symptoms, which is what criteria 3 and 4
#' are. This is the substance of issue #45 and the reason this function exists
#' rather than a one-line accessor.
#'
#' @section Deliberate weighting schemes:
#' Criterion 4 asks whether the weights look like a seed the solver never
#' improved on. For [PortfolioAnalytics::equal.weight()] and
#' [PortfolioAnalytics::inverse.volatility.weight()] the seed *is* the answer:
#' they evaluate a rule, they do not search. Periods whose class is
#' `optimize.portfolio.eqwt` or `optimize.portfolio.invol` are therefore exempt
#' from criterion 4 — otherwise every period of a 1/N benchmark would come back
#' `"suspect"` and the column would stop carrying information. Criteria 1 to 3
#' still apply to them: a rule can still throw, still return `NA`, and still
#' violate a box constraint the caller wrote.
#'
#' @section Reading the constraints:
#' Criterion 3 keys on the constraint object's **class**, not on its `$type`.
#' `add.constraint(type = "full_investment")` returns a `weight_sum_constraint`
#' whose `$type` is `"full_investment"` and which *also* carries `$min` and
#' `$max` of 1; read as a box constraint it would appear to pin every weight to
#' 1. `long_only` and `box` both return a `box_constraint`, with `$type` equal to
#' `"long_only"` and `"box"` respectively. Where several box constraints are
#' enabled the tightest bound of each is used, which is what the solver faces.
#'
#' @examples
#' # A malformed input stops the backtest rather than passing it.
#' try(diagnose_rebalancing(NULL))
#'
#' @examplesIf requireNamespace("PortfolioAnalytics", quietly = TRUE) && requireNamespace("ROI.plugin.quadprog", quietly = TRUE)
#' set.seed(1602)
#' R <- xts::xts(
#'   matrix(stats::rnorm(60 * 4, 0.006, 0.04), 60, 4,
#'          dimnames = list(NULL, c("a", "b", "c", "d"))),
#'   order.by = seq(as.Date("2015-01-31"), by = "month", length.out = 60)
#' )
#'
#' spec <- PortfolioAnalytics::portfolio.spec(colnames(R))
#' spec <- PortfolioAnalytics::add.constraint(spec, "full_investment")
#' spec <- PortfolioAnalytics::add.constraint(spec, "long_only")
#' spec <- PortfolioAnalytics::add.objective(spec, type = "risk", name = "StdDev")
#'
#' opt <- PortfolioAnalytics::optimize.portfolio.rebalancing(
#'   R, spec, optimize_method = "ROI",
#'   rebalance_on = "years", training_period = 24, rolling_window = 23
#' )
#'
#' d <- diagnose_rebalancing(opt)
#' d
#' table(d$status)
#'
#' # Never report a backtest without this line.
#' subset(d, status != "converged")
#'
#' @seealso [pm_backtest()], which calls this for every strategy it runs;
#'   [PortfolioAnalytics::optimize.portfolio.rebalancing()],
#'   [PortfolioAnalytics::extractWeights()].
#' @export
diagnose_rebalancing <- function(opt, tol = 1e-6) {
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    cli::cli_abort(c(
      "{.arg tol} must be a single non-negative number.",
      "x" = "You supplied {.obj_type_friendly {tol}} of length {length(tol)}.",
      ">" = "Use {.code tol = 1e-6} for a convex backend, or something looser
             such as {.code tol = 1e-3} for a metaheuristic, which does not
             satisfy an equality constraint exactly."
    ))
  }

  if (!inherits(opt, "optimize.portfolio.rebalancing")) {
    cli::cli_abort(c(
      "{.arg opt} must be an {.cls optimize.portfolio.rebalancing} object.",
      "x" = "You supplied {.obj_type_friendly {opt}}.",
      ">" = "Pass the object returned by
             {.fn PortfolioAnalytics::optimize.portfolio.rebalancing}, or an
             element of the {.field fits} list of a {.fn pm_backtest} result."
    ))
  }

  periods <- opt$opt_rebalancing
  if (!is.list(periods) || length(periods) == 0L) {
    cli::cli_abort(c(
      "{.arg opt} has no periods to diagnose.",
      "x" = "{.code opt$opt_rebalancing} is {.obj_type_friendly {periods}} of
             length {length(periods)}.",
      ">" = "Check that {.arg training_period} is smaller than
             {.code nrow(R)}. When it is not, the rebalancing loop iterates
             zero times and returns an empty list without complaining."
    ))
  }

  nms <- names(periods)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    cli::cli_abort(c(
      "{.code opt$opt_rebalancing} must be a fully named list.",
      "x" = "The names carry the rebalancing dates; without them a period
             cannot be placed in time.",
      ">" = "Do not strip the names from the object returned by
             {.fn PortfolioAnalytics::optimize.portfolio.rebalancing}."
    ))
  }

  # Explicit format: as.Date() on a character vector *errors* on an unparseable
  # string rather than returning NA, which would abort with a message about
  # charToDate() instead of about the object.
  dates <- as.Date(nms, format = "%Y-%m-%d")
  if (anyNA(dates)) {
    bad <- nms[is.na(dates)]
    cli::cli_abort(c(
      "The names of {.code opt$opt_rebalancing} must parse as dates.",
      "x" = "{cli::qty(length(bad))}Name{?s} {.val {utils::head(bad, 3L)}} did not.",
      ">" = "Expected {.val YYYY-MM-DD}, which is what
             {.fn PortfolioAnalytics::optimize.portfolio.rebalancing} writes."
    ))
  }

  n <- length(periods)
  out <- tibble::tibble(
    date        = dates,
    solver      = NA_character_,
    status      = NA_character_,
    objective   = NA_real_,
    n_assets    = NA_integer_,
    weights_sum = NA_real_,
    message     = NA_character_
  )

  prev_weights <- NULL
  prev_obj     <- NA_real_

  for (i in seq_len(n)) {
    x <- periods[[i]]

    ## Criterion 1 -- the period is not an optimisation at all.
    if (!inherits(x, "optimize.portfolio")) {
      out$status[i]  <- "failed"
      out$message[i] <- if (inherits(x, "condition")) {
        paste0("Period errored: ", conditionMessage(x))
      } else {
        paste0("Period is ", class(x)[1L],
               ", not an optimize.portfolio object.")
      }
      prev_weights <- NULL
      prev_obj     <- NA_real_
      next
    }

    out$solver[i] <- pm_solver_label(x)

    w   <- x$weights
    obj <- suppressWarnings(as.numeric(x$out))
    obj <- if (length(obj) == 0L) NA_real_ else obj[1L]
    out$objective[i] <- obj

    if (is.numeric(w) && length(w) > 0L) {
      out$weights_sum[i] <- sum(w)
      out$n_assets[i]    <- sum(abs(w) > tol, na.rm = TRUE)
    }

    ## Criterion 2 -- weights or objective are not finite.
    if (!is.numeric(w) || length(w) == 0L || any(!is.finite(w))) {
      out$status[i]  <- "failed"
      out$message[i] <- paste0(
        "Non-finite weights. ROI returned a solution of NA with a non-zero ",
        "status code and PortfolioAnalytics copied it through without ",
        "reading the status (issue #45)."
      )
      prev_weights <- NULL
      prev_obj     <- NA_real_
      next
    }
    if (!is.finite(obj)) {
      out$status[i]  <- "failed"
      out$message[i] <- "Objective value is not finite."
      prev_weights   <- w
      prev_obj       <- obj
      next
    }

    ## Criterion 3 -- the weights violate the specification.
    spec   <- if (!is.null(x$portfolio)) x$portfolio else opt$portfolio
    bounds <- pm_spec_bounds(spec, length(w))
    viol   <- pm_check_bounds(w, bounds, tol)
    if (!is.null(viol)) {
      out$status[i]  <- "suspect"
      out$message[i] <- viol
      prev_weights   <- w
      prev_obj       <- obj
      next
    }

    ## Criterion 4 -- the weights are a degenerate fallback.
    deliberate <- inherits(x, "optimize.portfolio.eqwt") ||
      inherits(x, "optimize.portfolio.invol")
    if (!deliberate) {
      degen <- pm_check_degenerate(w, prev_weights, obj, prev_obj,
                                   spec, bounds, tol)
      if (!is.null(degen)) {
        out$status[i]  <- "suspect"
        out$message[i] <- degen
        prev_weights   <- w
        prev_obj       <- obj
        next
      }
    }

    out$status[i] <- "converged"
    prev_weights  <- w
    prev_obj      <- obj
  }

  out
}


# The optimisation backend, read off the object's class. PortfolioAnalytics tags
# a result as c("optimize.portfolio.<method>", "optimize.portfolio"); there is no
# field that records it, so the class is the only place it survives.
pm_solver_label <- function(x) {
  cl  <- class(x)
  sub <- grep("^optimize[.]portfolio[.]", cl, value = TRUE)
  if (length(sub) == 0L) return(NA_character_)
  gsub("^optimize[.]portfolio[.]", "", sub[1L])
}


# Weight-sum and box bounds, keyed on the constraint object's class rather than
# its $type. A weight_sum_constraint of type "full_investment" also carries $min
# and $max of 1; read as a box it would appear to pin every weight to 1.
pm_spec_bounds <- function(portfolio, n) {
  out  <- list(min_sum = NA_real_, max_sum = NA_real_, min = NULL, max = NULL)
  cons <- portfolio$constraints
  if (!is.list(cons)) return(out)

  for (cn in cons) {
    if (isFALSE(cn$enabled)) next

    if (inherits(cn, "weight_sum_constraint")) {
      if (!is.null(cn$min_sum)) out$min_sum <- as.numeric(cn$min_sum)[1L]
      if (!is.null(cn$max_sum)) out$max_sum <- as.numeric(cn$max_sum)[1L]
    } else if (inherits(cn, "box_constraint")) {
      if (is.null(cn$min) || is.null(cn$max)) next
      lo <- rep_len(as.numeric(cn$min), n)
      hi <- rep_len(as.numeric(cn$max), n)
      out$min <- if (is.null(out$min)) lo else pmax(out$min, lo)
      out$max <- if (is.null(out$max)) hi else pmin(out$max, hi)
    }
  }
  out
}


pm_check_bounds <- function(w, bounds, tol) {
  s <- sum(w)
  if (!is.na(bounds$min_sum) && s < bounds$min_sum - tol) {
    return(sprintf("sum(weights) = %.6g is below min_sum = %.6g.",
                   s, bounds$min_sum))
  }
  if (!is.na(bounds$max_sum) && s > bounds$max_sum + tol) {
    return(sprintf("sum(weights) = %.6g is above max_sum = %.6g.",
                   s, bounds$max_sum))
  }

  nm <- names(w)
  if (is.null(nm)) nm <- paste0("asset ", seq_along(w))

  if (!is.null(bounds$min)) {
    j <- which(w < bounds$min - tol)
    if (length(j) > 0L) {
      return(sprintf("weight %s = %.6g is below its box minimum %.6g.",
                     nm[j[1L]], w[j[1L]], bounds$min[j[1L]]))
    }
  }
  if (!is.null(bounds$max)) {
    j <- which(w > bounds$max + tol)
    if (length(j) > 0L) {
      return(sprintf("weight %s = %.6g is above its box maximum %.6g.",
                     nm[j[1L]], w[j[1L]], bounds$max[j[1L]]))
    }
  }
  NULL
}


pm_check_degenerate <- function(w, prev_weights, obj, prev_obj,
                                portfolio, bounds, tol) {
  n <- length(w)

  # The equal-weight vector, scaled to max_sum where there is one, because that
  # is the seed PortfolioAnalytics itself builds (see equal.weight()).
  max_sum <- if (is.na(bounds$max_sum)) 1 else bounds$max_sum
  if (max(abs(w - rep(max_sum / n, n))) <= tol) {
    return(paste0("Weights equal the equal-weight vector, which is the seed ",
                  "several backends return when they give up."))
  }

  init <- NULL
  cons <- portfolio$constraints
  if (is.list(cons)) {
    for (cn in cons) {
      if (isFALSE(cn$enabled)) next
      if (identical(cn$type, "turnover") && !is.null(cn$weight_initial)) {
        init <- as.numeric(cn$weight_initial)
        break
      }
    }
  }
  if (!is.null(init) && length(init) == n && max(abs(w - init)) <= tol) {
    return("Weights equal weight_initial of the turnover constraint.")
  }

  # Only suspicious if the problem changed. An unchanged objective means the
  # solver was handed the same problem twice -- routine for a historical CVaR
  # whose 5% tail is three observations -- and repeating the answer is correct.
  same_problem <- is.finite(prev_obj) && is.finite(obj) &&
    abs(obj - prev_obj) <= tol * max(1, abs(obj))
  if (!is.null(prev_weights) && length(prev_weights) == n &&
      max(abs(w - prev_weights)) <= tol && !same_problem) {
    return(paste0("Weights are identical to the previous period even though ",
                  "the objective value moved, so the estimation window did."))
  }

  NULL
}
