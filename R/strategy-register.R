#' Declare a strategy once, run it anywhere
#'
#' A `pm_strategy` is a *value*: a name, a way of producing a
#' `PortfolioAnalytics` portfolio specification for whatever universe it is
#' handed, and the metric it should be judged by. It holds no data and runs
#' nothing. `pm_register()` collects strategies into a register, which is what
#' [pm_backtest()] consumes.
#'
#' The register is a value rather than a mutable store in the package
#' environment, and that is a deliberate choice: a Quarto book renders each
#' chapter in a fresh R session, so a registry that lived in `tidypm`'s namespace
#' would be empty by the time chapter 22 asked for it. A chapter that wants its
#' strategy to survive returns it, or adds it to [pm_book_strategies()].
#'
#' @param name Character scalar. The identifier the strategy is reported under.
#'   Must be unique within a register.
#' @param spec Either a `portfolio` object from
#'   [PortfolioAnalytics::portfolio.spec()], or — preferably — a function of one
#'   argument, the character vector of asset names, returning one. A function
#'   lets the same strategy run over the 16 class series and over the 181
#'   instruments without being redeclared, which is what makes a register worth
#'   having.
#' @param optimize_method Character scalar passed to
#'   [PortfolioAnalytics::optimize.portfolio.rebalancing()], typically `"ROI"`.
#'   Must be `NULL` when `weight_fun` is supplied, and must be supplied when it
#'   is not.
#' @param weight_fun A function `(R, portfolio, ...)` returning an
#'   `optimize.portfolio` object, for rules that need no optimisation.
#'   [PortfolioAnalytics::equal.weight()] and
#'   [PortfolioAnalytics::inverse.volatility.weight()] have exactly this
#'   signature and are the intended values; writing your own is a last resort.
#'   When this is supplied, [pm_backtest()] runs its own rebalancing loop over
#'   the same estimation windows rather than calling
#'   `optimize.portfolio.rebalancing()`, because that function takes a method
#'   name and not a function.
#' @param metric Character scalar naming the column of the metrics table this
#'   strategy should be reported by, or `NULL` to infer it from the
#'   specification's objectives. One of `"ann_return"`, `"ann_vol"`, `"sharpe"`,
#'   `"skewness"`, `"es_95"`, `"max_drawdown"`, `"ann_turnover"`. See the section
#'   below on why this exists.
#' @param excess_returns Logical. When `TRUE`, [pm_backtest()] fits this
#'   strategy on returns **in excess of the risk-free rate**, and applies the
#'   resulting weights to the total returns. Required for a maximum-Sharpe
#'   objective to mean a Sharpe ratio over the bill:
#'   `PortfolioAnalytics:::sharpe_obj_fun()` computes `mean / sd` with no
#'   risk-free term, so without this the objective is a Sharpe ratio over zero.
#'   Subtracting the rate from the data is the only route that works — see the
#'   section below.
#' @param label Character scalar used in tables and figures. Defaults to `name`.
#' @param ... Further arguments stored and passed to
#'   [PortfolioAnalytics::optimize.portfolio.rebalancing()] at run time, for
#'   example `maxSR = TRUE`.
#'
#' @return `pm_strategy()` returns a list of class `pm_strategy` with the
#'   elements `name`, `label`, `spec`, `optimize_method`, `weight_fun`, `metric`,
#'   `excess_returns` and `args`. `pm_register()` returns a named list of
#'   `pm_strategy` objects with class `pm_register`, names taken from each
#'   strategy's `name`.
#'
#' @section Why `excess_returns` subtracts from the data rather than the moments:
#' The obvious way to make a maximum-Sharpe objective use excess returns is a
#' custom `momentFUN` returning `mu = colMeans(R) - rf`. It does not work, and it
#' fails quietly. `optimize.portfolio()` finds the target return that maximises
#' `mean / sd` using that `mu`, then sets `moments$mean <- rep(0, N)` before the
#' final solve; `gmv_opt()` sees an all-zero mean vector, reads it as "no mean
#' was supplied", and falls back to `colMeans(R)` — the **total** means — when it
#' imposes `mu'w == target`. The target is chosen on one frontier and imposed on
#' another.
#'
#' Measured on the 16 risky class series with a 60-month window: the `momentFUN`
#' route lands up to 0.29 in weight away from the exact long-only tangency
#' portfolio and loses up to 0.05 of in-sample Sharpe ratio, while subtracting
#' the rate from the returns reproduces the exact tangency to within 0.03 in
#' weight and four decimals of Sharpe ratio. Hence `excess_returns`, not
#' `excess_moments`.
#'
#' @section Why a strategy carries its own metric:
#' A conditional value-at-risk optimisation reported only by its Sharpe ratio is
#' a misreport. It was not asked to maximise a Sharpe ratio, it was asked to
#' minimise a tail, and the number that says whether it did its job is the tail.
#' Declaring the metric alongside the specification means
#' [pm_backtest_metrics()] can pull the matching one for every strategy in a
#' register at once, instead of a fixed column that flatters one objective and
#' libels another. The full table is still there in `$metrics` for anyone who
#' wants to make the comparison anyway — deliberately, and with the mismatch in
#' plain sight.
#'
#' When `metric` is `NULL` it is inferred from the enabled objectives of the
#' specification: an `ES`, `ETL` or `CVaR` objective gives `"es_95"`; `maxSR =
#' TRUE`, or a `mean` and a `StdDev` objective together, gives `"sharpe"`; a
#' `StdDev`, `sd` or `var` objective alone gives `"ann_vol"`; a `mean` objective
#' alone gives `"ann_return"`; no objective at all gives `"sharpe"`.
#'
#' @examples
#' # Minimum variance, long only. The spec is a function of the universe, so the
#' # same declaration runs at class level and at instrument level.
#' min_var <- pm_strategy(
#'   "min_variance",
#'   spec = function(assets) {
#'     p <- PortfolioAnalytics::portfolio.spec(assets)
#'     p <- PortfolioAnalytics::add.constraint(p, "full_investment")
#'     p <- PortfolioAnalytics::add.constraint(p, "long_only")
#'     PortfolioAnalytics::add.objective(p, type = "risk", name = "StdDev")
#'   },
#'   optimize_method = "ROI",
#'   label = "Minimum variance"
#' )
#' min_var
#'
#' # A rule that needs no optimiser still carries a spec, because
#' # equal.weight() reads max_sum and the asset names off it.
#' one_over_n <- pm_strategy(
#'   "equal_weight",
#'   spec = function(assets) {
#'     p <- PortfolioAnalytics::portfolio.spec(assets)
#'     p <- PortfolioAnalytics::add.constraint(p, "full_investment")
#'     PortfolioAnalytics::add.constraint(p, "long_only")
#'   },
#'   weight_fun = PortfolioAnalytics::equal.weight,
#'   metric = "sharpe",
#'   label = "Equal weight"
#' )
#'
#' reg <- pm_register(one_over_n, min_var)
#' reg
#' names(reg)
#'
#' @seealso [pm_book_strategies()] for the register the book ships with,
#'   [pm_backtest()] to run one.
#' @name pm_strategy
NULL

pm_metric_names <- c(
  "ann_return", "ann_vol", "sharpe", "skewness",
  "es_95", "max_drawdown", "ann_turnover"
)

#' @rdname pm_strategy
#' @export
pm_strategy <- function(name,
                        spec,
                        optimize_method = "ROI",
                        weight_fun      = NULL,
                        metric          = NULL,
                        excess_returns  = FALSE,
                        label           = name,
                        ...) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    cli::cli_abort(c(
      "{.arg name} must be a single non-empty string.",
      "x" = "You supplied {.obj_type_friendly {name}}.",
      ">" = "It becomes the row label in the metrics table and must be unique
             within a register."
    ))
  }

  if (!is.function(spec) && !inherits(spec, "portfolio")) {
    cli::cli_abort(c(
      "{.arg spec} must be a {.cls portfolio} object or a function of the asset names.",
      "x" = "You supplied {.obj_type_friendly {spec}}.",
      ">" = "Write {.code spec = function(assets) portfolio.spec(assets)} so the
             strategy can run over any universe."
    ))
  }
  if (is.function(spec) && length(formals(spec)) < 1L) {
    cli::cli_abort(c(
      "A {.arg spec} function must take the asset names as its first argument.",
      "x" = "{.arg spec} takes no arguments.",
      ">" = "Write {.code function(assets) ...}; {.fn pm_backtest} calls it with
             {.code colnames(R)}."
    ))
  }

  if (!is.null(weight_fun)) {
    if (!is.function(weight_fun)) {
      cli::cli_abort(c(
        "{.arg weight_fun} must be a function or {.code NULL}.",
        "x" = "You supplied {.obj_type_friendly {weight_fun}}.",
        ">" = "Use {.fn PortfolioAnalytics::equal.weight} or
               {.fn PortfolioAnalytics::inverse.volatility.weight}; both already
               have the required {.code (R, portfolio, ...)} signature."
      ))
    }
    optimize_method <- NULL
  } else if (is.null(optimize_method) ||
             !is.character(optimize_method) || length(optimize_method) != 1L) {
    cli::cli_abort(c(
      "A strategy needs either an {.arg optimize_method} or a {.arg weight_fun}.",
      "x" = "Both are missing.",
      ">" = "Pass {.code optimize_method = \"ROI\"} for an optimisation, or
             {.code weight_fun = PortfolioAnalytics::equal.weight} for a rule."
    ))
  }

  if (!is.null(metric)) {
    if (!is.character(metric) || length(metric) != 1L ||
        !metric %in% pm_metric_names) {
      cli::cli_abort(c(
        "{.arg metric} must be one of the metric columns, or {.code NULL}.",
        "x" = "You supplied {.val {metric}}.",
        "i" = "Available: {.val {pm_metric_names}}.",
        ">" = "Leave it {.code NULL} to have it inferred from the objectives."
      ))
    }
  }

  if (!is.logical(excess_returns) || length(excess_returns) != 1L ||
      is.na(excess_returns)) {
    cli::cli_abort(c(
      "{.arg excess_returns} must be {.code TRUE} or {.code FALSE}.",
      "x" = "You supplied {.obj_type_friendly {excess_returns}}.",
      ">" = "Set it {.code TRUE} for a maximum-Sharpe objective, so the mean is
             measured over the bill rather than over zero."
    ))
  }

  structure(
    list(
      name            = name,
      label           = as.character(label)[1L],
      spec            = spec,
      optimize_method = optimize_method,
      weight_fun      = weight_fun,
      metric          = metric,
      excess_returns  = excess_returns,
      args            = list(...)
    ),
    class = "pm_strategy"
  )
}

#' @rdname pm_strategy
#' @param x A `pm_strategy` or `pm_register`, for the print and subset methods.
#' @export
pm_register <- function(...) {
  items <- list(...)
  flat  <- list()
  for (it in items) {
    if (inherits(it, "pm_strategy")) {
      flat <- c(flat, list(it))
    } else if (is.list(it)) {
      flat <- c(flat, unname(it))
    } else {
      cli::cli_abort(c(
        "{.fn pm_register} takes {.cls pm_strategy} objects, or lists of them.",
        "x" = "One argument was {.obj_type_friendly {it}}.",
        ">" = "Build each strategy with {.fn pm_strategy} first."
      ))
    }
  }

  bad <- !vapply(flat, inherits, logical(1), what = "pm_strategy")
  if (any(bad)) {
    cli::cli_abort(c(
      "Every element of a register must be a {.cls pm_strategy}.",
      "x" = "{sum(bad)} element{?s} {?was/were} not.",
      ">" = "Build them with {.fn pm_strategy}."
    ))
  }

  nms <- vapply(flat, function(s) s$name, character(1))
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    cli::cli_abort(c(
      "Strategy names must be unique within a register.",
      "x" = "{.val {dup}} appear{?s/} more than once.",
      ">" = "Rename one of them; the name is the key the metrics table joins on."
    ))
  }

  structure(stats::setNames(flat, nms), class = "pm_register")
}

#' @rdname pm_strategy
#' @export
print.pm_strategy <- function(x, ...) {
  how <- if (is.null(x$weight_fun)) {
    paste0("optimize_method = \"", x$optimize_method, "\"")
  } else {
    "weight function, run through pm_backtest()'s own rebalancing loop"
  }
  met  <- if (is.null(x$metric)) "inferred from the objectives" else x$metric
  spc  <- if (is.function(x$spec)) {
    "function of the asset names"
  } else {
    "fixed portfolio object"
  }
  cli::cli_text("{.cls pm_strategy} {.strong {x$name}} ({x$label})")
  cli::cli_bullets(c(
    "*" = "engine:         {how}",
    "*" = "metric:         {met}",
    "*" = "spec:           {spc}",
    "*" = "excess returns: {x$excess_returns}"
  ))
  invisible(x)
}

#' @rdname pm_strategy
#' @param i Index, name, or logical vector selecting strategies from a register.
#' @export
`[.pm_register` <- function(x, i) {
  out <- NextMethod()
  if (length(out) == 0L) {
    cli::cli_abort(c(
      "That selection leaves no strategies.",
      "i" = "The register holds {.val {names(x)}}.",
      ">" = "Select by name, e.g. {.code reg[c(\"equal_weight\", \"min_variance\")]}."
    ))
  }
  if (anyNA(names(out)) || any(vapply(out, is.null, logical(1)))) {
    cli::cli_abort(c(
      "That selection names strategies the register does not hold.",
      "i" = "The register holds {.val {names(x)}}.",
      ">" = "Check the spelling; register names are the {.arg name} argument of
             {.fn pm_strategy}, not the labels."
    ))
  }
  structure(out, class = "pm_register")
}

#' @rdname pm_strategy
#' @export
print.pm_register <- function(x, ...) {
  cli::cli_text("{.cls pm_register} with {length(x)} strateg{?y/ies}")
  for (s in x) {
    cli::cli_bullets(c("*" = "{.strong {s$name}}: {s$label}"))
  }
  invisible(x)
}


#' The register the book runs on
#'
#' Five strategies covering the standard span of an allocation chapter: two rules
#' that need no optimiser, two mean-variance problems, and one tail problem whose
#' headline metric is not a Sharpe ratio. Chapter 22 runs the whole register over
#' one window; every construction chapter cites one line of the result.
#'
#' All five are declared with `spec` as a function of the asset names, so the
#' register runs unchanged over the 16 risky class series and over the 181
#' instruments. All are long-only and fully invested, because the point of the
#' comparison is the objective, not the constraint set.
#'
#' @param box_max Upper bound on any single weight, in weight units. Default `1`,
#'   which is no cap. Chapters 9 and 20 tighten it; at `0.25` the minimum-variance
#'   solution stops being a Treasury proxy.
#' @param es_p Confidence level for the conditional value-at-risk objective of
#'   `min_cvar`, and for the `es_95` metric column. Default `0.95`.
#'
#' @return A [pm_register] of five `pm_strategy` objects, named
#'   `equal_weight`, `inverse_vol`, `min_variance`, `max_sharpe` and `min_cvar`.
#'
#' @section The five:
#' \describe{
#'   \item{`equal_weight`}{[PortfolioAnalytics::equal.weight()]. The benchmark
#'     that is hard to beat out of sample. Reported by `sharpe`.}
#'   \item{`inverse_vol`}{[PortfolioAnalytics::inverse.volatility.weight()].
#'     Uses the diagonal of the covariance matrix and none of the off-diagonal,
#'     so it is the natural control for how much the correlations are worth.
#'     Reported by `ann_vol`.}
#'   \item{`min_variance`}{ROI, `StdDev` objective. Reported by `ann_vol`.}
#'   \item{`max_sharpe`}{ROI, `mean` and `StdDev` objectives with `maxSR = TRUE`
#'     and `excess_returns = TRUE`. Reported by `sharpe`.}
#'   \item{`min_cvar`}{ROI, `ES` objective at `es_p`. Reported by `es_95`, which
#'     is the whole point of the register carrying a metric per strategy.}
#' }
#'
#' @examples
#' reg <- pm_book_strategies()
#' reg
#' reg$max_sharpe
#'
#' # Tighten the position cap for a chapter that needs one.
#' pm_book_strategies(box_max = 0.25)
#'
#' # Registers compose: add one of your own and run the six together.
#' pm_register(
#'   reg,
#'   pm_strategy(
#'     "min_variance_capped",
#'     spec = function(assets) {
#'       p <- PortfolioAnalytics::portfolio.spec(assets)
#'       p <- PortfolioAnalytics::add.constraint(p, "full_investment")
#'       p <- PortfolioAnalytics::add.constraint(p, "box", min = 0, max = 0.2)
#'       PortfolioAnalytics::add.objective(p, type = "risk", name = "StdDev")
#'     }
#'   )
#' )
#'
#' @seealso [pm_strategy()], [pm_backtest()].
#' @export
pm_book_strategies <- function(box_max = 1, es_p = 0.95) {
  if (!is.numeric(box_max) || length(box_max) != 1L || is.na(box_max) ||
      box_max <= 0 || box_max > 1) {
    cli::cli_abort(c(
      "{.arg box_max} must be a single number in (0, 1].",
      "x" = "You supplied {.val {box_max}}.",
      ">" = "Use {.code box_max = 1} for no cap, or e.g. {.code 0.25} for a
             quarter of the book in any one series."
    ))
  }
  if (!is.numeric(es_p) || length(es_p) != 1L || is.na(es_p) ||
      es_p <= 0.5 || es_p >= 1) {
    cli::cli_abort(c(
      "{.arg es_p} must be a single number in (0.5, 1).",
      "x" = "You supplied {.val {es_p}}.",
      ">" = "Write the confidence level, {.val {0.95}}, not the tail
             probability {.val {0.05}}."
    ))
  }

  base_spec <- function(assets) {
    p <- PortfolioAnalytics::portfolio.spec(assets)
    p <- PortfolioAnalytics::add.constraint(p, "full_investment")
    PortfolioAnalytics::add.constraint(p, "box", min = 0, max = box_max)
  }

  pm_register(
    pm_strategy(
      "equal_weight",
      spec       = base_spec,
      weight_fun = PortfolioAnalytics::equal.weight,
      metric     = "sharpe",
      label      = "Equal weight"
    ),
    pm_strategy(
      "inverse_vol",
      spec       = base_spec,
      weight_fun = PortfolioAnalytics::inverse.volatility.weight,
      metric     = "ann_vol",
      label      = "Inverse volatility"
    ),
    pm_strategy(
      "min_variance",
      spec = function(assets) {
        PortfolioAnalytics::add.objective(base_spec(assets),
                                          type = "risk", name = "StdDev")
      },
      optimize_method = "ROI",
      metric          = "ann_vol",
      label           = "Minimum variance"
    ),
    pm_strategy(
      "max_sharpe",
      spec = function(assets) {
        p <- PortfolioAnalytics::add.objective(base_spec(assets),
                                               type = "return", name = "mean")
        PortfolioAnalytics::add.objective(p, type = "risk", name = "StdDev")
      },
      optimize_method = "ROI",
      metric          = "sharpe",
      excess_returns  = TRUE,
      label           = "Maximum Sharpe",
      maxSR           = TRUE
    ),
    pm_strategy(
      "min_cvar",
      spec = function(assets) {
        PortfolioAnalytics::add.objective(
          base_spec(assets), type = "risk", name = "ES",
          arguments = list(p = es_p)
        )
      },
      optimize_method = "ROI",
      metric          = "es_95",
      label           = "Minimum CVaR"
    )
  )
}


# Infer the reporting metric from the objectives a specification carries.
pm_infer_metric <- function(portfolio, args) {
  objs <- vapply(portfolio$objectives, function(o) {
    if (isFALSE(o$enabled)) NA_character_ else as.character(o$name)[1L]
  }, character(1))
  objs <- objs[!is.na(objs)]

  max_sr <- isTRUE(args$maxSR)

  if (any(objs %in% c("ES", "ETL", "CVaR"))) return("es_95")
  if (max_sr) return("sharpe")
  has_mean <- "mean" %in% objs
  has_risk <- any(objs %in% c("StdDev", "sd", "var"))
  if (has_mean && has_risk) return("sharpe")
  if (has_risk) return("ann_vol")
  if (has_mean) return("ann_return")
  "sharpe"
}
