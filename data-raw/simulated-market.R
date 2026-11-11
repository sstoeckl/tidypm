# Import the modelled multi-asset market into the package.
#
# Source: ../simulated_data/, dataset version 6.2.0. The raw files are read
# from there and are never copied into this repository: they carry the source
# calendar, and what ships is on the model calendar (below).
#
# Three decisions are taken here rather than in the chapters, because every
# chapter would otherwise take them in its first code block.
#
# 1. The universe. The five money-market series leave the class universe. The
#    book allocates across sixteen risky classes -- five currencies x
#    (government, credit, equity) plus one commodity index -- and carries the
#    money markets beside them as `sim_money_market_*`. The risk-free rate is
#    the Treasury bill, `sim_riskfree_*`.
#
# 2. The calendar. The dataset is a modelled market, not a history: factor
#    models estimated on market data were replayed and the idiosyncratic
#    components drawn, so no series is the record of a real security. It is
#    therefore shipped on a MODEL CALENDAR of 31 model years, written as the
#    calendar years 2101 to 2131 (model year = year - 2100), so that nothing
#    downstream mistakes a model month for a historical one. Monthly dates are
#    the last day of the model month; weekly dates keep their month and day, so
#    every week stays in its month and a weekly-to-monthly aggregation
#    reproduces the monthly files exactly. `CALENDAR = "source"` (environment
#    variable TIDYPM_CALENDAR) keeps the source dates, for checking only.
#
# 3. The episodes. Three stress episodes and one long bear market are named
#    once, in `sim_episodes`, by what happened in them rather than by a date,
#    and every chapter takes its windows from that table.
#
# Beyond that the import does no screening. It repairs one metadata defect and
# verifies seven properties, all reported to the console so that the import is
# auditable rather than trusted.
#
# Run from the book project so that renv is active (the package directory has
# no library of its own):
#
#   Set-Location "...\Tidy_Asset_Allocation"
#   & "C:\R\R-4.6.1\bin\Rscript.exe" ../tidypm/data-raw/simulated-market.R

library(dplyr)

pkg_root <- if (file.exists("DESCRIPTION") && file.exists("data-raw")) "." else "../tidypm"
raw_dir  <- normalizePath(file.path(pkg_root, "..", "simulated_data"), mustWork = TRUE)
out_dir  <- file.path(pkg_root, "data")
stopifnot(dir.exists(out_dir))

CALENDAR    <- match.arg(Sys.getenv("TIDYPM_CALENDAR", "model"), c("model", "source"))
MODEL_YEAR0 <- 2100L         # model year = calendar year - MODEL_YEAR0

# The mapping from the source calendar to the model calendar, and the episode
# windows on the source calendar, live in an untracked local file beside this
# script: the package ships the model calendar only.
local_cal <- file.path(pkg_root, "data-raw", "calendar.local.R")
if (!file.exists(local_cal)) {
  stop("data-raw/calendar.local.R is missing. It defines YEAR_OFFSET (an integer) ",
       "and EPISODE_WINDOWS (a tibble with episode, start, end on the source calendar); ",
       "it is kept out of version control on purpose.", call. = FALSE)
}
source(local_cal, local = TRUE)
stopifnot(is.integer(YEAR_OFFSET), is.data.frame(EPISODE_WINDOWS),
          all(c("episode", "start", "end") %in% names(EPISODE_WINDOWS)))

say <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- the model calendar ------------------------------------------------------

is_leap <- function(y) (y %% 4L == 0L & y %% 100L != 0L) | y %% 400L == 0L

# Weekly dates: same month and day in the model year. A source 29 February
# lands in a model year that is not a leap year and becomes the 28th; weekly
# dates are seven days apart, so that can never collide with a neighbour.
to_model_day <- function(d) {
  if (CALENDAR == "source") return(as.Date(d))
  lt <- as.POSIXlt(as.Date(d))
  y <- lt$year + 1900L + YEAR_OFFSET; m <- lt$mon + 1L; dd <- lt$mday
  dd[m == 2L & dd == 29L & !is_leap(y)] <- 28L
  as.Date(sprintf("%04d-%02d-%02d", y, m, dd))
}

# Monthly dates: the last day of the model month.
to_model_month_end <- function(d) {
  if (CALENDAR == "source") return(as.Date(d))
  lt <- as.POSIXlt(as.Date(d))
  y <- lt$year + 1900L + YEAR_OFFSET; m <- lt$mon + 1L
  ny <- ifelse(m == 12L, y + 1L, y); nm <- ifelse(m == 12L, 1L, m + 1L)
  as.Date(sprintf("%04d-%02d-01", ny, nm)) - 1
}

read_raw <- function(stem, monthly = grepl("_monthly$", stem)) {
  pq <- file.path(raw_dir, paste0(stem, ".parquet"))
  out <- if (file.exists(pq)) {
    arrow::read_parquet(pq)
  } else {
    utils::read.csv(file.path(raw_dir, paste0(stem, ".csv")), stringsAsFactors = FALSE)
  }
  out <- tibble::as_tibble(out)
  if ("date" %in% names(out)) {
    out$date <- if (monthly) to_model_month_end(out$date) else to_model_day(out$date)
  }
  out
}

version <- sub(".*Dataset version ([0-9]+[.][0-9]+[.][0-9]+).*", "\\1",
               grep("Dataset version", readLines(file.path(raw_dir, "README.md")), value = TRUE)[1])
say("Importing the modelled market, dataset version %s, on the %s calendar", version, CALENDAR)

# ---- metadata ----------------------------------------------------------------
#
# One defect to repair, inherited from 4.1.0 and still present: the export
# labels government, credit and money-market instruments
# `currency_basis = "local"` with `currency = NA`, but their returns are in
# dollars. Left alone, any screen on `currency_basis` silently drops all fifty
# bonds. The repair is verified against the exchange rates below.

assets_raw <- read_raw("assets")

sim_assets <- assets_raw |>
  mutate(
    currency       = if_else(asset_class %in% c("government", "credit", "money_market"),
                             "US$", currency),
    currency_basis = "USD"
  ) |>
  arrange(asset_id)

n_repaired <- sum(assets_raw$currency_basis != "USD")
say("  metadata: %d instruments, repaired currency_basis on %d", nrow(sim_assets), n_repaired)

# ---- returns, exchange rates, the bill ---------------------------------------

sim_asset_returns_weekly <- read_raw("assets_weekly") |>
  select(date, asset = asset_id, return) |> arrange(date, asset)
sim_asset_returns_monthly <- read_raw("assets_monthly") |>
  select(date, asset = asset_id, return) |> arrange(date, asset)

classes_weekly  <- read_raw("classes_weekly")  |>
  select(date, class = series, return, index) |> arrange(date, class)
classes_monthly <- read_raw("classes_monthly") |>
  select(date, class = series, return, index) |> arrange(date, class)

is_mm <- function(x) grepl("_money_market$", x)
sim_class_returns_weekly  <- filter(classes_weekly,  !is_mm(class))
sim_class_returns_monthly <- filter(classes_monthly, !is_mm(class))
sim_money_market_weekly   <- filter(classes_weekly,   is_mm(class))
sim_money_market_monthly  <- filter(classes_monthly,  is_mm(class))

say("  classes: %d risky series and %d money-market series over %d months, %s to %s",
    n_distinct(sim_class_returns_monthly$class), n_distinct(sim_money_market_monthly$class),
    n_distinct(sim_class_returns_monthly$date),
    format(min(sim_class_returns_monthly$date)), format(max(sim_class_returns_monthly$date)))

sim_riskfree_weekly  <- read_raw("riskfree_weekly")  |> select(date, rate_annual, return)
sim_riskfree_monthly <- read_raw("riskfree_monthly") |> select(date, rate_annual, return)

sim_fx_weekly  <- read_raw("fx_weekly")  |> select(date, region, rate, fx_return) |> arrange(date, region)
sim_fx_monthly <- read_raw("fx_monthly") |> select(date, region, fx_return)        |> arrange(date, region)

sim_curve_weekly <- read_raw("bond_factors_weekly") |>
  select(date, series, value) |> arrange(date, series)
sim_marketcap_weekly <- read_raw("market_cap_weekly") |>
  select(date, asset = asset_id, market_cap) |> arrange(date, asset)

# ESG rating changes: the export keys them by a quarter label. They become a
# model year, a quarter and the quarter-end date on the model calendar.
esg_raw <- read_raw("esg_rating_changes_quarterly")
esg_q   <- as.integer(sub("^Q([1-4]) .*$", "\\1", esg_raw$quarter))
esg_y   <- as.integer(sub("^Q[1-4] ", "", esg_raw$quarter))
stopifnot(!anyNA(esg_q), !anyNA(esg_y))
sim_esg_changes_quarterly <- tibble::tibble(
  date       = to_model_month_end(as.Date(sprintf("%04d-%02d-01", esg_y, esg_q * 3L))),
  asset      = esg_raw$asset_id,
  esg_change = as.integer(esg_raw$esg_change)
) |> arrange(date, asset)

# ---- the episodes ------------------------------------------------------------
#
# Named for what happened, not for when. Windows are closed intervals of dates
# on whichever calendar is in force; a chapter never types one.

episode_text <- tibble::tribble(
  ~episode,          ~label,               ~description,
  "equity_bear",     "equity bear market",
    "A long decline in equities, led by the most highly valued markets, while the safest government bonds rise.",
  "credit_crisis",   "credit crisis",
    "A flight to quality: equities and most credit fall together, the safest government bonds rally, and correlations among risky assets rise.",
  "liquidity_shock", "liquidity shock",
    "A sudden, short sell-off across every risky class, followed by a fast recovery.",
  "rates_shock",     "rates shock",
    "A rise in yields in which government bonds and credit lose in every region, most equity markets fall, and commodities gain."
)
stopifnot(setequal(episode_text$episode, EPISODE_WINDOWS$episode))
sim_episodes <- episode_text |>
  left_join(EPISODE_WINDOWS, by = "episode") |>
  mutate(start = to_model_day(as.Date(start)), end = to_model_day(as.Date(end))) |>
  select(episode, label, start, end, description)

# ---- verification ------------------------------------------------------------

check <- function(label, ok, detail = "") {
  say("  [%s] %s%s", if (isTRUE(ok)) "ok  " else "FAIL", label,
      if (nzchar(detail)) paste0(" -- ", detail) else "")
  if (!isTRUE(ok)) stop("import check failed: ", label, call. = FALSE)
}

say("Verifying:")

# 1. No return is missing and every panel is rectangular.
gap <- function(d, id) dplyr::n_distinct(d$date) * dplyr::n_distinct(d[[id]]) - nrow(d)
panels <- list(asset_monthly = sim_asset_returns_monthly, asset_weekly = sim_asset_returns_weekly,
               class_monthly = sim_class_returns_monthly, class_weekly = sim_class_returns_weekly,
               mm_monthly = sim_money_market_monthly, mm_weekly = sim_money_market_weekly)
check("no missing returns anywhere", !any(vapply(panels, function(d) anyNA(d$return), logical(1))))
gaps <- vapply(panels, function(d) gap(d, if ("asset" %in% names(d)) "asset" else "class"), numeric(1))
check("every panel is rectangular", all(gaps == 0),
      sprintf("absent cells: %s", paste(sprintf("%s %d", names(gaps), gaps), collapse = ", ")))

# 2. The split is exact and the identifiers agree.
check("sixteen risky classes and five money markets",
      n_distinct(sim_class_returns_monthly$class) == 16L &&
        n_distinct(sim_money_market_monthly$class) == 5L &&
        setequal(c(sim_class_returns_monthly$class, sim_money_market_monthly$class),
                 classes_monthly$class))
check("metadata covers every instrument in the returns",
      setequal(sim_assets$asset_id, unique(sim_asset_returns_monthly$asset)))

# 3. The currency repair is true, not asserted: an unhedged foreign money
#    market is nearly a pure currency position.
mm_cor <- vapply(c("EU", "UK", "CH", "JP"), function(rg) {
  mm <- sim_money_market_monthly |> filter(class == paste0(rg, "_money_market")) |> arrange(date)
  fx <- sim_fx_monthly |> filter(region == rg) |> arrange(date)
  stats::cor(mm$return, fx$fx_return[match(mm$date, fx$date)])
}, numeric(1))
check("bond returns really are in dollars", all(mm_cor > 0.95),
      paste(sprintf("%s %+.2f", names(mm_cor), mm_cor), collapse = "  "))

# 4. The bill is a usable risk-free rate; the money market is a duration asset.
neg_bill <- mean(sim_riskfree_monthly$return < 0)
us_mm    <- sim_money_market_monthly |> filter(class == "US_money_market") |> arrange(date)
neg_mm   <- mean(us_mm$return < 0)
cor_mm   <- stats::cor(us_mm$return,
                       sim_riskfree_monthly$return[match(us_mm$date, sim_riskfree_monthly$date)])
check("bill is near-monotone, money market is a duration asset",
      neg_bill < 0.02 && neg_mm > 0.05 && cor_mm < 0.99,
      sprintf("negative months: bill %.1f%%, US money market %.1f%%; correlation %.2f",
              100 * neg_bill, 100 * neg_mm, cor_mm))

# 5. The commodity block carries eight independent series.
cmd <- sim_assets$asset_id[sim_assets$asset_class == "commodity"]
cmd_m <- sim_asset_returns_monthly |> filter(asset %in% cmd) |>
  tidyr::pivot_wider(names_from = asset, values_from = return) |> select(-date) |> as.matrix()
check("commodities are not eight copies of one series",
      qr(stats::cov(cmd_m))$rank == length(cmd),
      sprintf("rank %d of %d, min pairwise correlation %+.2f",
              qr(stats::cov(cmd_m))$rank, length(cmd), min(stats::cor(cmd_m))))

# 6. The euro-area equity overlay is present (a stated capital-market
#    assumption of the model since 5.1.0).
eq_ret <- sim_class_returns_monthly |> filter(grepl("_equity$", class)) |>
  group_by(class) |> summarise(ann = prod(1 + return)^(12 / n()) - 1, .groups = "drop")
check("the euro-area equity overlay is present",
      eq_ret$ann[eq_ret$class == "EU_equity"] > eq_ret$ann[eq_ret$class == "JP_equity"] + 0.03,
      paste(sprintf("%s %.1f%%", eq_ret$class, 100 * eq_ret$ann), collapse = "  "))

# 7. The calendar. Dates are strictly increasing within every table, monthly
#    dates are true month ends, every week sits in a month that has a monthly
#    observation, compounding the weekly class returns within a month gives the
#    monthly ones, and every episode lies inside the sample.
mon_ok <- all(as.integer(format(unique(sim_class_returns_monthly$date) + 1, "%d")) == 1L)
wk     <- sim_class_returns_weekly |> mutate(ym = format(date, "%Y-%m")) |>
  group_by(class, ym) |> summarise(r = prod(1 + return) - 1, .groups = "drop")
mo     <- sim_class_returns_monthly |> mutate(ym = format(date, "%Y-%m"))
cmp    <- inner_join(wk, mo, by = c("class", "ym"))
agg_ok <- nrow(cmp) == nrow(mo) && max(abs(cmp$r - cmp$return)) < 1e-8
inc_ok <- all(vapply(panels, function(d) !is.unsorted(sort(unique(d$date)), strictly = TRUE), logical(1)))
ep_ok  <- all(sim_episodes$start >= min(sim_class_returns_weekly$date) &
              sim_episodes$end   <= max(sim_class_returns_monthly$date) &
              sim_episodes$start <  sim_episodes$end)
check("the calendar is consistent", mon_ok && agg_ok && inc_ok && ep_ok,
      sprintf("month ends %s, weekly compounds to monthly %s (max gap %.1e), episodes inside %s",
              mon_ok, agg_ok, max(abs(cmp$r - cmp$return)), ep_ok))
if (CALENDAR == "model") {
  yrs <- as.integer(format(range(sim_class_returns_monthly$date), "%Y")) - MODEL_YEAR0
  check("the model calendar runs from year 1 to year 31", identical(yrs, c(1L, 31L)))
}

# ---- write -------------------------------------------------------------------
#
# save() with these arguments is exactly what usethis::use_data() does.

if (CALENDAR != "model") {
  say("CALENDAR = '%s': nothing written. The package ships the model calendar only.", CALENDAR)
} else {
  objects <- c("sim_assets",
               "sim_asset_returns_monthly", "sim_asset_returns_weekly",
               "sim_class_returns_monthly", "sim_class_returns_weekly",
               "sim_money_market_monthly", "sim_money_market_weekly",
               "sim_riskfree_monthly", "sim_riskfree_weekly",
               "sim_fx_monthly", "sim_fx_weekly",
               "sim_curve_weekly", "sim_marketcap_weekly",
               "sim_esg_changes_quarterly", "sim_episodes")
  for (o in objects) {
    save(list = o, file = file.path(out_dir, paste0(o, ".rda")), compress = "xz", version = 3)
  }
  say("Wrote %d objects, %s, to %s", length(objects),
      format(structure(sum(file.size(list.files(out_dir, full.names = TRUE))),
                       class = "object_size"), units = "auto"), out_dir)
}
