#' Run a register of strategies out of sample over one common window
#'
#' The book's backtest engine. Every strategy in `strategies` is fitted on the
#' same rolling estimation windows, over the same universe, on the same
#' rebalancing calendar, and reported on the same set of metrics — gross of
#' trading costs and net of them, side by side, never blended. Each strategy is
#' also passed through [diagnose_rebalancing()], because
#' [PortfolioAnalytics::optimize.portfolio.rebalancing()] does not report a
#' solver failure and a backtest that quietly held a vector of `NA`s for a fifth
#' of its life looks exactly like one that did not.
#'
#' @param returns Either an `xts` matrix of periodic simple returns, one column
#'   per asset, or a long data frame that [as_xts_pm()] can widen. Must have no
#'   missing values; see the section on gaps.
#' @param strategies A [pm_register], a single [pm_strategy], or a list of
#'   `pm_strategy` objects.
#' @param riskfree The per-period risk-free return. Either `NULL` for zero, a
#'   single number held constant, or a data frame with columns `date` and
#'   `return` such as [sim_riskfree_monthly]. It is used twice: for the Sharpe
#'   ratio, and — for strategies declared with `excess_returns = TRUE` — it is
#'   subtracted from the returns the optimiser is fitted on.
#' @param date,asset,value <[`data-masking`][rlang::args_data_masking]> Columns
#'   of `returns` holding the date, the asset identifier and the return, when
#'   `returns` is a data frame. Ignored when it is already an `xts`. Defaults
#'   `date`, `asset`, `value`; the shipped class table wants
#'   `asset = class, value = return`.
#' @param estimation_window Integer. The number of observations the moments are
#'   estimated on, ending at and including the rebalancing date. Default `60`,
#'   the book's convention. See the section on the off-by-one, which is the
#'   reason this is not simply passed through as `rolling_window`.
#' @param rebalance_on Character, passed to [xts::endpoints()]. Default
#'   `"months"`.
#' @param periods_per_year Integer used to annualise. Default `12`. Set `52` for
#'   the weekly tables, `252` for daily.
#' @param costs A named numeric vector of one-way proportional trading costs,
#'   resolved against the asset names. Default [pm_costs_default()]. Costs are
#'   **reported alongside** the gross number and never folded into it.
#' @param es_p Confidence level for the expected-shortfall column. Default
#'   `0.95`. The column is called `es_95` whatever this is set to; the level
#'   actually used is recorded in `$meta$es_p`.
#' @param sum_tolerance Numeric. Every enabled weight-sum constraint whose
#'   `min_sum` equals its `max_sum` is widened by this much on each side before
#'   the specification reaches the solver. Default `1e-6`. Set it to `0` to
#'   reproduce the failure described below; do not set it anywhere near
#'   `PortfolioAnalytics`' own suggested `0.99`/`1.01`, which leaves a percent of
#'   the book uninvested. See that section.
#' @param workers Integer. Number of parallel workers for the rebalancing loop.
#'   Default `1`, which runs sequentially. Anything larger starts a PSOCK
#'   cluster with [parallel::makeCluster()], registers it with
#'   [doParallel::registerDoParallel()], and shuts it down and restores the
#'   previous backend on exit. See the section on what is and is not parallel.
#' @param tol Weight tolerance passed to [diagnose_rebalancing()]. Default
#'   `1e-6`.
#' @param verbose Logical. Print a per-strategy progress line. Default `TRUE`
#'   when the session is interactive.
#'
#' @return An object of class `pm_backtest`, a list with
#'   \describe{
#'     \item{`metrics`}{A tibble, two rows per strategy (`basis` of `"gross"`
#'       and `"net"`), with the columns `strategy`, `label`, `basis`,
#'       `ann_return`, `ann_vol`, `sharpe`, `skewness`, `es_95`,
#'       `max_drawdown`, `ann_turnover`, `n_periods`, `n_failed`, `n_suspect`
#'       and `objective_metric`.}
#'     \item{`returns`}{A tibble `strategy`, `date`, `basis`, `return`, the
#'       out-of-sample portfolio return series. `2 * n_strategies * n_periods`
#'       rows.}
#'     \item{`weights`}{A tibble `strategy`, `date`, `asset`, `weight`, the
#'       weights held at the start of each out-of-sample period, after drift.}
#'     \item{`costs_paid`}{A tibble `strategy`, `date`, `turnover`, `cost`, one
#'       row per strategy per out-of-sample period.}
#'     \item{`diagnosis`}{A tibble, the [diagnose_rebalancing()] output for every
#'       strategy stacked, with a leading `strategy` column.}
#'     \item{`fits`}{A named list of the raw `optimize.portfolio.rebalancing`
#'       objects, one per strategy, for anyone who wants to look inside.}
#'     \item{`cost_schedule`}{The resolved per-asset cost vector.}
#'     \item{`meta`}{A list recording `estimation_window`, `rebalance_on`,
#'       `periods_per_year`, `es_p`, `tol`, the universe, and the first and last
#'       out-of-sample dates.}
#'   }
#'
#' @section The off-by-one in `rolling_window`:
#' `optimize.portfolio.rebalancing()` estimates period `ep` on
#' `R[(ep - rolling_window):ep, ]`, which is `rolling_window + 1` observations,
#' not `rolling_window`. Ask for a 60-month window and you get 61 months of data.
#' `pm_backtest()` therefore passes `rolling_window = estimation_window - 1` and
#' `training_period = estimation_window`, so that `estimation_window = 60` really
#' does mean sixty observations in every window including the first. If you call
#' `optimize.portfolio.rebalancing()` directly, do the subtraction yourself.
#'
#' @section Why `full_investment` has to be widened by a millionth:
#' `PortfolioAnalytics:::gmv_opt()` writes `sum(w) = 1` as **two opposing
#' inequalities**, `sum(w) >= min_sum` and `-sum(w) >= -max_sum`, and hands them
#' to `quadprog` through `ROI`. When `min_sum == max_sum` those two rows are
#' active at the same point, and `quadprog`'s dual active-set method resolves the
#' degeneracy by declaring the problem infeasible: **40 of the 313 minimum-
#' variance windows** over the 16 risky class series come back with
#' `"constraints are inconsistent, no solution!"` for a problem whose entire
#' constraint set is `sum(w) = 1` and `0 <= w <= 1`.
#'
#' It is not a conditioning problem. Those covariance matrices have condition
#' numbers around `1.6e4` and the same windows solve instantly once the pair is
#' separated. It is not a scaling problem either: putting the returns in percent
#' only moves the count from 40 to 25. Dropping either the redundant row or the
#' vacuous zero equality row that `gmv_opt()` also emits when there is no return
#' objective takes it to zero, and so does separating `min_sum` from `max_sum` by
#' any positive amount.
#'
#' `ROI` reports the failure faithfully in `result$status$code`;
#' `PortfolioAnalytics:::gmv_opt()` reads `result$solution` without looking at
#' the status, so the period comes back with weights of `NA` and no complaint.
#' That is issue #45, and [diagnose_rebalancing()] catches it as criterion 2.
#'
#' `sum_tolerance` is the fix: `min_sum` becomes `1 - 1e-6` and `max_sum` becomes
#' `1 + 1e-6`, all 313 windows solve, and the solutions sit at the lower bound
#' with `sum(w) = 0.999999`, a millionth of the book in cash. The
#' `PortfolioAnalytics` documentation suggests `0.99` and `1.01` for the same
#' purpose; that leaves the solver a full percent of slack, which it takes, and
#' one percent of a portfolio is not a rounding error.
#'
#' @section What is parallel, and what only looks as though it is:
#' `optimize.portfolio.rebalancing()` already contains a
#' `foreach(...) %dopar%` over the rebalancing dates, so the periods are
#' embarrassingly parallel and the machinery to run them that way is in the
#' package. What is missing is the other half: `optimize.portfolio()` registers a
#' backend in its own body, and `optimize.portfolio.rebalancing()` does not. With
#' no backend registered, `%dopar%` silently falls back to sequential execution
#' and `foreach` says so once, in a warning about "no parallel backend
#' registered", which is easy to read as noise. A reader can therefore run a
#' 313-period backtest on a 14-core machine, on one core, and never notice.
#'
#' `workers` closes that half. It is not
#' [PortfolioAnalytics::optimize.portfolio.parallel()], which is a different
#' function for a different job: its `nodes` argument runs the *same* single
#' optimisation repeatedly, which is what a stochastic solver or a random
#' portfolio search needs. It does nothing for a rolling backtest.
#'
#' Two consequences of running on workers, both worth knowing before debugging a
#' backtest:
#'
#' * A period whose solver **throws** still comes back, because
#'   `.errorhandling = "pass"` returns the condition object as that period's
#'   result. [diagnose_rebalancing()] catches it as criterion 1 either way.
#' * A period whose solver **warns or messages** loses that output entirely: it
#'   was emitted in the worker process and nothing collects it. Diagnose a
#'   suspicious backtest sequentially, then re-run it in parallel.
#'
#' Strategies declared with a `weight_fun` do not go through
#' `optimize.portfolio.rebalancing()` at all and are evaluated sequentially.
#' They are rules, not searches, and take milliseconds.
#'
#' @section Timing, and why there is no look-ahead:
#' The window for a rebalancing date `d` ends **at** `d`; the weights it produces
#' are applied to the return of the *following* period. Nothing estimated at `d`
#' touches a return earned on or before `d`. With monthly data, a 60-month window
#' and 372 months, the first rebalancing date is month 60 and the first
#' out-of-sample return is month 61.
#'
#' @section What happens when a period does not solve:
#' The weights of a failed period cannot be traded on, so the previous period's
#' weights are carried forward and a warning names how many periods that
#' happened in. If the *first* rebalancing date fails there is nothing to carry
#' and the function aborts. The count is reported in the `n_failed` column
#' whatever happens: a backtest with carried-forward weights is a different
#' strategy from the one that was specified, and the table has to say so.
#'
#' @section Gaps:
#' A missing return is an error, not something to impute. `PortfolioAnalytics`
#' will not warn about one, but it silently changes the effective sample of every
#' moment estimate and, in the drift accounting, makes a portfolio return
#' undefined. Fill or drop before the boundary, not after.
#'
#' @section Costs, turnover and the two bases:
#' Turnover in period `t` is `0.5 * sum(abs(w_bop - w_eop_previous))`, the
#' standard one-way fraction of the book, including the initial purchase in the
#' first period. `ann_turnover` is its mean times `periods_per_year`. The cost
#' charged is `sum(cost_i * abs(w_bop - w_eop_previous))`, per asset and not
#' flat, subtracted from that period's return to give the `"net"` basis. Between
#' rebalancing dates the weights drift and nothing is traded, so nothing is
#' charged.
#'
#' @examples
#' # A short, fast run: three years of estimation, annual rebalancing.
#' \donttest{
#' # The sixteen risky class series are the shipped default; see ?sim-market.
#'
#' bt <- pm_backtest(
#'   sim_class_returns_monthly,
#'   strategies        = pm_book_strategies()[c("equal_weight", "min_variance")],
#'   riskfree          = sim_riskfree_monthly,
#'   asset             = class,
#'   value             = return,
#'   estimation_window = 36,
#'   rebalance_on      = "years",
#'   verbose           = FALSE
#' )
#'
#' bt
#' bt$metrics[, c("strategy", "basis", "ann_return", "ann_vol", "sharpe")]
#'
#' # The line no chapter reports a backtest without.
#' subset(bt$diagnosis, status != "converged")
#'
#' # Each strategy by the metric it was asked to optimise.
#' pm_backtest_metrics(bt)
#' }
#'
#' @seealso [pm_strategy()] and [pm_book_strategies()] for what goes in,
#'   [pm_backtest_metrics()] for what comes out, [diagnose_rebalancing()] for
#'   the per-period check.
#' @export
pm_backtest <- function(returns,
                        strategies        = pm_book_strategies(),
                        riskfree          = NULL,
                        date              = date,
                        asset             = asset,
                        value             = value,
                        estimation_window = 60L,
                        rebalance_on      = "months",
                        periods_per_year  = 12L,
                        costs             = pm_costs_default(),
                        es_p              = 0.95,
                        sum_tolerance     = 1e-6,
                        workers           = 1L,
                        tol               = 1e-6,
                        verbose           = interactive()) {

  R <- if (xts::is.xts(returns)) {
    returns
  } else if (is.data.frame(returns)) {
    as_xts_pm(returns, {{ date }}, {{ asset }}, {{ value }})
  } else {
    cli::cli_abort(c(
      "{.arg returns} must be an {.cls xts} matrix or a long data frame.",
      "x" = "You supplied {.obj_type_friendly {returns}}.",
      ">" = "Cross the boundary with {.fn as_xts_pm} first, or pass the long
             table and name its columns with {.arg asset} and {.arg value}."
    ))
  }

  if (!is.numeric(R) || ncol(R) < 2L) {
    cli::cli_abort(c(
      "A backtest needs at least two assets.",
      "x" = "The return matrix has {ncol(R)} column{?s}.",
      ">" = "Check the {.arg asset} column: a single-asset panel widens to one
             column."
    ))
  }
  if (anyNA(R)) {
    n_na <- sum(is.na(R))
    cli::cli_abort(c(
      "The return matrix has {n_na} missing value{?s}.",
      "x" = "{.pkg PortfolioAnalytics} does not warn about missing returns, but
             they change the effective sample of every moment estimate and make
             the portfolio return undefined.",
      ">" = "Fill or drop the gaps before calling {.fn pm_backtest}; the
             simulated market's class tables are complete, the weekly
             instrument table is not."
    ))
  }

  reg <- pm_as_register(strategies)

  estimation_window <- pm_check_count(estimation_window, "estimation_window", min = 2L)
  periods_per_year  <- pm_check_count(periods_per_year, "periods_per_year", min = 1L)

  if (!is.character(rebalance_on) || length(rebalance_on) != 1L) {
    cli::cli_abort(c(
      "{.arg rebalance_on} must be a single string.",
      "x" = "You supplied {.obj_type_friendly {rebalance_on}}.",
      ">" = "It is passed to {.fn xts::endpoints}: {.val months},
             {.val quarters}, {.val years}."
    ))
  }
  if (!is.numeric(es_p) || length(es_p) != 1L || is.na(es_p) ||
      es_p <= 0.5 || es_p >= 1) {
    cli::cli_abort(c(
      "{.arg es_p} must be a single number in (0.5, 1).",
      "x" = "You supplied {.val {es_p}}.",
      ">" = "Write the confidence level {.val {0.95}}, not the tail probability."
    ))
  }
  if (!is.numeric(sum_tolerance) || length(sum_tolerance) != 1L ||
      !is.finite(sum_tolerance) || sum_tolerance < 0 || sum_tolerance >= 0.5) {
    cli::cli_abort(c(
      "{.arg sum_tolerance} must be a single number in [0, 0.5).",
      "x" = "You supplied {.obj_type_friendly {sum_tolerance}}.",
      ">" = "Leave it at {.val {1e-06}} unless you are demonstrating the
             {.pkg quadprog} degeneracy, in which case use {.val {0}}."
    ))
  }
  if (estimation_window >= nrow(R)) {
    cli::cli_abort(c(
      "{.arg estimation_window} leaves nothing out of sample.",
      "x" = "The window is {estimation_window} period{?s} and the data has
             {nrow(R)}.",
      ">" = "Shorten the window, or supply a longer history."
    ))
  }

  assets   <- colnames(R)
  cost_vec <- pm_resolve_costs(assets, costs)
  idx      <- zoo::index(R)
  rf       <- pm_align_riskfree(riskfree, idx)

  ep_all <- xts::endpoints(R, on = rebalance_on)
  ep     <- ep_all[ep_all >= estimation_window]
  if (length(ep) < 2L) {
    cli::cli_abort(c(
      "Fewer than two rebalancing dates fall after the estimation window.",
      "x" = "{.arg rebalance_on = \"{rebalance_on}\"} gives
             {length(ep)} date{?s} at or after observation {estimation_window}.",
      ">" = "Shorten {.arg estimation_window} or rebalance more often."
    ))
  }

  workers <- pm_check_count(workers, "workers", min = 1L)
  cluster <- pm_setup_backend(workers)
  if (!is.null(cluster)) {
    on.exit({
      parallel::stopCluster(cluster)
      foreach::registerDoSEQ()
    }, add = TRUE)
  }

  fits       <- vector("list", length(reg))
  names(fits) <- names(reg)
  metrics    <- vector("list", length(reg))
  ret_rows   <- vector("list", length(reg))
  wt_rows    <- vector("list", length(reg))
  cost_rows  <- vector("list", length(reg))
  diag_rows  <- vector("list", length(reg))

  for (i in seq_along(reg)) {
    strat <- reg[[i]]
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Fitting {.strong {strat$name}} ({i}/{length(reg)})")
    }

    spec <- pm_resolve_spec(strat, assets)
    fit  <- pm_fit_strategy(strat, spec, R, ep, estimation_window,
                            rebalance_on, rf, sum_tolerance)
    fits[[i]] <- fit

    dg <- diagnose_rebalancing(fit, tol = tol)
    diag_rows[[i]] <- dplyr::bind_cols(
      tibble::tibble(strategy = strat$name), dg
    )

    W <- pm_weight_matrix(fit, assets, strat$name)
    acc <- pm_accumulate(R, W, ep, cost_vec, strat$name)

    metric <- strat$metric
    if (is.null(metric)) metric <- pm_infer_metric(spec, strat$args)

    rf_oos <- rf[acc$rows]
    m_gross <- pm_metrics_one(acc$gross, rf_oos, acc$turnover,
                              idx[acc$rows], periods_per_year, es_p)
    m_net   <- pm_metrics_one(acc$net, rf_oos, acc$turnover,
                              idx[acc$rows], periods_per_year, es_p)

    metrics[[i]] <- dplyr::bind_cols(
      tibble::tibble(
        strategy = strat$name,
        label    = strat$label,
        basis    = c("gross", "net")
      ),
      dplyr::bind_rows(m_gross, m_net),
      tibble::tibble(
        n_periods        = nrow(dg),
        n_failed         = sum(dg$status == "failed"),
        n_suspect        = sum(dg$status == "suspect"),
        objective_metric = metric
      )
    )

    ret_rows[[i]] <- tibble::tibble(
      strategy = strat$name,
      date     = rep(idx[acc$rows], 2L),
      basis    = rep(c("gross", "net"), each = length(acc$rows)),
      return   = c(acc$gross, acc$net)
    )
    wt_rows[[i]] <- tibble::tibble(
      strategy = strat$name,
      date     = rep(idx[acc$rows], times = length(assets)),
      asset    = rep(assets, each = length(acc$rows)),
      weight   = as.numeric(acc$bop)
    )
    cost_rows[[i]] <- tibble::tibble(
      strategy = strat$name,
      date     = idx[acc$rows],
      turnover = acc$turnover,
      cost     = acc$cost
    )
  }

  oos_dates <- sort(unique(dplyr::bind_rows(cost_rows)$date))

  structure(
    list(
      metrics       = dplyr::bind_rows(metrics),
      returns       = dplyr::bind_rows(ret_rows),
      weights       = dplyr::bind_rows(wt_rows),
      costs_paid    = dplyr::bind_rows(cost_rows),
      diagnosis     = dplyr::bind_rows(diag_rows),
      fits          = fits,
      cost_schedule = cost_vec,
      meta = list(
        estimation_window = estimation_window,
        rebalance_on      = rebalance_on,
        periods_per_year  = periods_per_year,
        es_p              = es_p,
        sum_tolerance     = sum_tolerance,
        workers           = workers,
        tol               = tol,
        assets            = assets,
        n_rebalances      = length(ep),
        oos_start         = oos_dates[1L],
        oos_end           = oos_dates[length(oos_dates)],
        n_oos             = length(oos_dates)
      )
    ),
    class = "pm_backtest"
  )
}


#' Pull the metric that matches each strategy's own objective
#'
#' A minimum-CVaR portfolio reported only by its Sharpe ratio is a misreport. It
#' was not asked to earn a Sharpe ratio. `pm_backtest_metrics()` is the accessor
#' that keeps that honest: by default it returns, for each strategy, the single
#' number that strategy was optimising, as declared in [pm_strategy()].
#'
#' @param x A `pm_backtest` object.
#' @param metric `NULL` to take each strategy's own `objective_metric`,
#'   `"all"` for the full wide table, or a character vector of metric names to
#'   pull those columns for every strategy.
#' @param basis Character vector, any of `"gross"` and `"net"`. Default both.
#'
#' @return For `metric = "all"`, the `metrics` tibble filtered to `basis`. For
#'   anything else, a long tibble with columns `strategy`, `label`, `basis`,
#'   `metric` and `value`, one row per strategy per basis per metric requested,
#'   in register order.
#'
#' @examples
#' \donttest{
#' # The sixteen risky class series are the shipped default; see ?sim-market.
#' bt <- pm_backtest(
#'   sim_class_returns_monthly, pm_book_strategies()[c("equal_weight", "min_variance")],
#'   riskfree = sim_riskfree_monthly, asset = class, value = return,
#'   estimation_window = 36, rebalance_on = "years", verbose = FALSE
#' )
#'
#' # Each strategy by its own objective.
#' pm_backtest_metrics(bt)
#'
#' # The comparison everyone makes anyway, gross only.
#' pm_backtest_metrics(bt, metric = "sharpe", basis = "gross")
#'
#' # What the cost model cost.
#' pm_backtest_metrics(bt, metric = c("ann_return", "ann_turnover"))
#' }
#'
#' @seealso [pm_backtest()].
#' @export
pm_backtest_metrics <- function(x, metric = NULL, basis = c("gross", "net")) {
  if (!inherits(x, "pm_backtest")) {
    cli::cli_abort(c(
      "{.arg x} must be a {.cls pm_backtest} object.",
      "x" = "You supplied {.obj_type_friendly {x}}.",
      ">" = "Run {.fn pm_backtest} first."
    ))
  }
  basis <- match.arg(basis, c("gross", "net"), several.ok = TRUE)
  tab   <- x$metrics[x$metrics$basis %in% basis, , drop = FALSE]

  if (identical(metric, "all")) return(tab)

  if (is.null(metric)) {
    out <- tibble::tibble(
      strategy = tab$strategy,
      label    = tab$label,
      basis    = tab$basis,
      metric   = tab$objective_metric,
      value    = vapply(seq_len(nrow(tab)),
                        function(i) as.numeric(tab[[tab$objective_metric[i]]][i]),
                        numeric(1))
    )
    return(out)
  }

  if (!is.character(metric) || length(metric) == 0L ||
      !all(metric %in% pm_metric_names)) {
    bad <- setdiff(metric, pm_metric_names)
    cli::cli_abort(c(
      "{.arg metric} must be {.val all}, {.code NULL}, or metric names.",
      "x" = "{.val {bad}} {?is/are} not {?a metric/metrics}.",
      "i" = "Available: {.val {pm_metric_names}}.",
      ">" = "Leave it {.code NULL} to get each strategy's own objective."
    ))
  }

  tidyr::pivot_longer(
    tab[, c("strategy", "label", "basis", metric), drop = FALSE],
    cols      = dplyr::all_of(metric),
    names_to  = "metric",
    values_to = "value"
  )
}


#' @rdname pm_backtest
#' @param x A `pm_backtest` object.
#' @param ... Ignored by the print method.
#' @export
print.pm_backtest <- function(x, ...) {
  m <- x$meta
  cli::cli_text("{.cls pm_backtest}: {length(x$fits)} strateg{?y/ies},
                {m$n_oos} out-of-sample period{?s}")
  cli::cli_bullets(c(
    "*" = "window: {m$estimation_window} observation{?s}, rebalanced on
           {.val {m$rebalance_on}} ({m$n_rebalances} date{?s})",
    "*" = "sample: {format(m$oos_start)} to {format(m$oos_end)},
           {length(m$assets)} asset{?s}",
    "*" = "solver failures: {sum(x$metrics$n_failed[x$metrics$basis == 'gross'])};
           suspect periods: {sum(x$metrics$n_suspect[x$metrics$basis == 'gross'])}"
  ))
  print(pm_backtest_metrics(x))
  invisible(x)
}


## ---------------------------------------------------------------------------
## internals
## ---------------------------------------------------------------------------

# optimize.portfolio.rebalancing() runs its periods through %dopar% but never
# registers a backend, so out of the box the loop is sequential and says so only
# in a warning that reads like noise. This registers one, and puts the previous
# state back when the caller returns.
pm_setup_backend <- function(workers) {
  if (!requireNamespace("foreach", quietly = TRUE)) {
    if (workers > 1L) {
      cli::cli_abort(c(
        "{.pkg foreach} is needed to run the rebalancing loop in parallel.",
        ">" = "Install {.pkg foreach} and {.pkg doParallel}, or use
               {.code workers = 1}."
      ))
    }
    return(NULL)
  }

  if (workers == 1L) {
    # Registering the sequential backend explicitly silences foreach's warning
    # without pretending anything is parallel -- but only if the caller has not
    # registered a backend of their own.
    if (!foreach::getDoParRegistered()) foreach::registerDoSEQ()
    return(NULL)
  }

  if (!requireNamespace("doParallel", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg doParallel} is needed for {.code workers > 1}.",
      "x" = "It is not installed.",
      ">" = "Install it, or use {.code workers = 1}."
    ))
  }

  n_cores <- parallel::detectCores()
  if (!is.na(n_cores) && workers >= n_cores) {
    cli::cli_warn(c(
      "{.arg workers} is {workers} on a machine reporting {n_cores} core{?s}.",
      ">" = "Leave headroom: {.code workers = {max(1L, n_cores - 2L)}} is a
             better default than saturating the machine."
    ))
  }

  cl <- parallel::makeCluster(workers)
  doParallel::registerDoParallel(cl)
  cl
}

pm_check_count <- function(x, arg, min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x) ||
      x < min) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a single whole number of at least {min}.",
      "x" = "You supplied {.obj_type_friendly {x}}.",
      ">" = "Write {.code {arg} = {max(min, 60L)}L}."
    ))
  }
  as.integer(x)
}

pm_as_register <- function(strategies) {
  if (inherits(strategies, "pm_register")) return(strategies)
  if (inherits(strategies, "pm_strategy"))  return(pm_register(strategies))
  if (is.list(strategies) && length(strategies) > 0L) {
    return(do.call(pm_register, unname(strategies)))
  }
  cli::cli_abort(c(
    "{.arg strategies} must be a {.cls pm_register}, a {.cls pm_strategy}, or a
     non-empty list of them.",
    "x" = "You supplied {.obj_type_friendly {strategies}}.",
    ">" = "Start from {.fn pm_book_strategies}."
  ))
}

# gmv_opt() writes sum(w) = 1 as two opposing inequalities and hands them to
# quadprog, whose dual active-set method then calls 40 of 313 plainly feasible
# minimum-variance problems infeasible. Separating min_sum from max_sum by any
# positive amount resolves the degeneracy. See the @section in pm_backtest().
pm_relax_weight_sum <- function(portfolio, eps) {
  if (eps <= 0) return(portfolio)
  cons <- portfolio$constraints
  if (!is.list(cons)) return(portfolio)

  for (j in seq_along(cons)) {
    cn <- cons[[j]]
    if (isFALSE(cn$enabled) || !inherits(cn, "weight_sum_constraint")) next
    if (is.null(cn$min_sum) || is.null(cn$max_sum)) next
    if (!isTRUE(all.equal(as.numeric(cn$min_sum), as.numeric(cn$max_sum)))) next
    portfolio$constraints[[j]]$min_sum <- as.numeric(cn$min_sum) - eps
    portfolio$constraints[[j]]$max_sum <- as.numeric(cn$max_sum) + eps
  }
  portfolio
}

pm_resolve_spec <- function(strat, assets) {
  spec <- if (is.function(strat$spec)) strat$spec(assets) else strat$spec
  if (!inherits(spec, "portfolio")) {
    cli::cli_abort(c(
      "The {.arg spec} of strategy {.val {strat$name}} did not give a
       {.cls portfolio}.",
      "x" = "It returned {.obj_type_friendly {spec}}.",
      ">" = "The function must end with a
             {.fn PortfolioAnalytics::portfolio.spec} pipeline, and
             {.fn add.constraint} returns the portfolio, so assign its value."
    ))
  }
  if (length(spec$assets) != length(assets)) {
    cli::cli_abort(c(
      "Strategy {.val {strat$name}} was built for {length(spec$assets)} asset{?s}
       but the universe has {length(assets)}.",
      ">" = "Declare {.arg spec} as a function of the asset names so it follows
             the universe: {.code function(assets) portfolio.spec(assets)}."
    ))
  }
  spec
}

pm_align_riskfree <- function(riskfree, idx) {
  if (is.null(riskfree)) return(rep(0, length(idx)))

  if (is.numeric(riskfree) && length(riskfree) == 1L && !is.na(riskfree)) {
    return(rep(riskfree, length(idx)))
  }

  if (!is.data.frame(riskfree) ||
      !all(c("date", "return") %in% names(riskfree))) {
    cli::cli_abort(c(
      "{.arg riskfree} must be {.code NULL}, a single number, or a data frame
       with {.field date} and {.field return}.",
      "x" = "You supplied {.obj_type_friendly {riskfree}}.",
      ">" = "{.code sim_riskfree_monthly} already has that shape; use its
             {.field return} column, not {.field rate_annual}."
    ))
  }

  hit <- match(idx, riskfree$date)
  if (anyNA(hit)) {
    miss <- format(idx[is.na(hit)])
    cli::cli_abort(c(
      "{.arg riskfree} does not cover the return dates.",
      "x" = "{cli::qty(length(miss))}Date{?s} {.val {utils::head(miss, 3L)}}
             {?is/are} missing ({length(miss)} in total).",
      ">" = "Use the risk-free series at the same frequency as the returns:
             {.code sim_riskfree_monthly} with monthly returns,
             {.code sim_riskfree_weekly} with weekly."
    ))
  }
  rr <- as.numeric(riskfree$return[hit])
  if (anyNA(rr)) {
    cli::cli_abort(c(
      "{.arg riskfree} has missing values on dates the backtest uses.",
      ">" = "Fill them before calling {.fn pm_backtest}; a missing risk-free
             rate silently drops a period from the Sharpe ratio."
    ))
  }
  rr
}

pm_fit_strategy <- function(strat, spec, R, ep, estimation_window,
                            rebalance_on, rf, sum_tolerance) {
  if (isTRUE(strat$excess_returns)) {
    R <- xts::xts(zoo::coredata(R) - rf, order.by = zoo::index(R))
  }

  # The weight-sum relaxation exists for quadprog and must not reach a weighting
  # rule: equal.weight() reads max_sum straight off the specification and would
  # return 1/N scaled by 1 + sum_tolerance.
  if (!is.null(strat$weight_fun)) return(
    pm_fit_weight_fun(strat, spec, R, ep, estimation_window)
  )
  spec <- pm_relax_weight_sum(spec, sum_tolerance)

  args <- c(
    list(
      R               = R,
      portfolio       = spec,
      optimize_method = strat$optimize_method,
      rebalance_on    = rebalance_on,
      training_period = estimation_window,
      # See @section: PortfolioAnalytics uses (ep - rolling_window):ep, which is
      # rolling_window + 1 observations.
      rolling_window  = estimation_window - 1L
    ),
    strat$args
  )
  fit <- try(
    do.call(PortfolioAnalytics::optimize.portfolio.rebalancing, args),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    cli::cli_abort(c(
      "Strategy {.val {strat$name}} could not be fitted at all.",
      "x" = "{.fn optimize.portfolio.rebalancing} threw: {conditionMessage(attr(fit, 'condition'))}",
      ">" = "A whole-run failure is usually a missing ROI plugin or an
             objective the backend cannot solve, not a bad period. Check
             {.code optimize_method} against the objective."
    ))
  }
  fit
}

# optimize.portfolio.rebalancing() takes a method name, not a function, so a
# weighting rule cannot be handed to it. This reproduces its loop exactly --
# same endpoints, same window arithmetic, same list names -- and returns an
# object of the same class, so that diagnose_rebalancing() and everything
# downstream do not care which route a strategy took.
pm_fit_weight_fun <- function(strat, spec, R, ep, estimation_window) {
  per <- lapply(ep, function(e) {
    lo  <- max(1L, e - (estimation_window - 1L))
    win <- R[lo:e, , drop = FALSE]
    tryCatch(
      strat$weight_fun(win, spec),
      error = function(cnd) cnd
    )
  })
  names(per) <- as.character(zoo::index(R)[ep])

  structure(
    list(
      portfolio       = spec,
      R               = R,
      call            = match.call(),
      elapsed_time    = NA,
      opt_rebalancing = per
    ),
    class = "optimize.portfolio.rebalancing"
  )
}

# One row per rebalancing date, NA where the period is unusable.
pm_weight_matrix <- function(fit, assets, strategy_name) {
  per <- fit$opt_rebalancing
  n   <- length(assets)
  W   <- matrix(NA_real_, length(per), n,
                dimnames = list(names(per), assets))
  for (i in seq_along(per)) {
    x <- per[[i]]
    if (!inherits(x, "optimize.portfolio")) next
    w <- x$weights
    if (!is.numeric(w) || length(w) != n || any(!is.finite(w))) next
    W[i, ] <- as.numeric(w)
  }

  bad <- which(!stats::complete.cases(W))
  if (length(bad) > 0L) {
    if (1L %in% bad) {
      cli::cli_abort(c(
        "The first rebalancing period of {.val {strategy_name}} did not solve.",
        "x" = "There is nothing to carry forward, so the backtest cannot start.",
        ">" = "Inspect {.code diagnose_rebalancing(bt$fits[['{strategy_name}']])},
               then loosen the constraints or lengthen the estimation window."
      ))
    }
    cli::cli_warn(c(
      "{length(bad)} of {nrow(W)} periods of {.val {strategy_name}} did not solve.",
      "!" = "The previous period's weights are carried forward, so what was
             backtested is not quite the strategy that was specified.",
      "i" = "The count is in the {.field n_failed} column; the reasons are in
             {.code bt$diagnosis}."
    ))
    for (i in bad) W[i, ] <- W[i - 1L, ]
  }
  W
}

# Drift and rebalance, done here rather than with Return.portfolio() because
# that function silently back-fills equal weights for periods before the first
# weight date and rewrites the index while doing it.
pm_accumulate <- function(R, W, ep, cost_vec, strategy_name) {
  idx  <- zoo::index(R)
  Rm   <- zoo::coredata(R)
  reb  <- as.numeric(idx[ep])
  rows <- (ep[1L] + 1L):nrow(Rm)
  k    <- findInterval(as.numeric(idx[rows]), reb, left.open = TRUE)

  n     <- length(rows)
  nass  <- ncol(Rm)
  bop   <- matrix(NA_real_, n, nass)
  gross <- numeric(n)
  cost  <- numeric(n)
  turn  <- numeric(n)

  eop    <- rep(0, nass)   # flat before the first trade
  k_prev <- 0L

  for (j in seq_len(n)) {
    if (k[j] != k_prev) {
      w      <- W[k[j], ]
      k_prev <- k[j]
    } else {
      w <- eop
    }
    traded    <- abs(w - eop)
    turn[j]   <- 0.5 * sum(traded)
    cost[j]   <- sum(cost_vec * traded)
    bop[j, ]  <- w

    rt <- Rm[rows[j], ]
    rp <- sum(w * rt)
    if (!is.finite(rp)) {
      cli::cli_abort(c(
        "The portfolio return of {.val {strategy_name}} is not finite at
         {format(idx[rows[j]])}.",
        ">" = "Check the return matrix for infinite values; a total loss
               (-100%) also does this."
      ))
    }
    gross[j] <- rp
    eop      <- w * (1 + rt) / (1 + rp)
  }

  list(rows = rows, bop = bop, gross = gross,
       net = gross - cost, cost = cost, turnover = turn)
}

pm_metrics_one <- function(r, rf, turnover, dates, ppy, es_p) {
  x <- xts::xts(matrix(r, ncol = 1L, dimnames = list(NULL, "p")),
                order.by = dates)
  n <- length(r)

  ann_return <- prod(1 + r)^(ppy / n) - 1
  ann_rf     <- prod(1 + rf)^(ppy / n) - 1
  ann_vol    <- stats::sd(r) * sqrt(ppy)

  tibble::tibble(
    ann_return   = ann_return,
    ann_vol      = ann_vol,
    sharpe       = (ann_return - ann_rf) / ann_vol,
    skewness     = as.numeric(PerformanceAnalytics::skewness(x, method = "moment")),
    es_95        = as.numeric(PerformanceAnalytics::ES(
      x, p = es_p, method = "historical", invert = FALSE)),
    max_drawdown = as.numeric(PerformanceAnalytics::maxDrawdown(x)),
    ann_turnover = mean(turnover) * ppy
  )
}
