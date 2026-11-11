#' The modelled multi-asset market
#'
#' A calibrated, licence-free market of five regions and five asset classes,
#' weekly and monthly over thirty-one model years, **all in US dollars,
#' unhedged**. The dataset is *modelled*: it contains no real data. Factor
#' models for bonds, credit, equities and commodities were estimated by
#' historical simulation on market data from 1992 to 2022, the factors were
#' replayed and the idiosyncratic components drawn afresh, and the asset paths
#' were reassembled from them. No series is the history of a real security, so
#' the whole dataset ships with the package and every number in the book can be
#' reproduced offline.
#'
#' Two things follow from its being a model rather than a record. It runs on
#' its own calendar (see [model_year()]): model years 1 to 31, written as the
#' calendar years 2101 to 2131, so that no model month is mistaken for a
#' historical one. And because every series is the output of a factor model,
#' the market can be re-run with its factors shocked, which a history cannot.
#'
#' What the calibration buys is realism of structure. The stress episodes
#' arrive with the right relative magnitudes and in the right places: in the
#' credit crisis equities and credit fall together while the safest government
#' bonds rally; in the rates shock both bond classes lose alongside equities.
#' The episodes are named in [sim_episodes] by what happens in them.
#'
#' @section Which one to reach for:
#'
#' `sim_class_returns_monthly` is the allocation spine and what most chapters
#' want: **sixteen risky class series** — five currencies × (government bonds,
#' credit, equity) plus one global commodity index — over 372 complete months.
#' The five money-market series are deliberately not in it; they ship beside it
#' as `sim_money_market_monthly` for the chapters that need a short-duration
#' asset or a pure currency position. The risk-free rate is
#' `sim_riskfree_monthly`, a three-month Treasury bill. The split is taken once
#' at import so that no chapter has to take it in its first code block.
#'
#' `sim_asset_returns_monthly` is the same market at instrument level, 181
#' series including the five money-market instruments, for the sleeve chapters
#' and the chapters about estimation error. The weekly pair is the model's
#' native frequency; the monthly returns are compounded from it.
#'
#' @section Three things to know before modelling on this:
#'
#' * **The money market is a short-duration bond, not the bill.**
#'   `*_money_market` is the 0.25-year point on a fitted curve, marked to
#'   market. `US_money_market` tracks the bill closely (correlation 0.93,
#'   drawdown under one percent), but it is negative in about 19% of months
#'   where the bill is negative in one month of 372. Use [sim_riskfree_monthly]
#'   as the risk-free rate.
#'
#' * **Currency is a first-order driver.** Everything is unhedged in dollars, so
#'   a non-US instrument carries its local return *and* its currency's move. A
#'   foreign money market has almost no duration and is therefore nearly a pure
#'   currency position: it correlates +1.00 with its own exchange rate and runs
#'   at 9–10% volatility. Subtracting the matching `fx_return` from
#'   [sim_fx_monthly] hedges the position and takes that volatility to under
#'   one percent. Roughly two-thirds of the 9–11% volatility on foreign
#'   government bonds is exchange rate. Only US and `GLOBAL` instruments are
#'   unaffected.
#'
#' * **The bond block is rank-deficient.** Three maturities per region are
#'   priced off a three-factor curve, and the two credit grades at each
#'   maturity share one credit factor, so some bond combinations are near-exact
#'   replicas of others, and the covariance matrix of all 181 instruments is
#'   numerically singular. Unconstrained mean-variance at instrument level
#'   produces nonsense — which is worth demonstrating rather than preventing.
#'   At class level the problem does not exist.
#'
#' @section Provenance and known gaps:
#'
#' **Pinned to dataset version 6.2.0.** The idiosyncratic components are one
#' draw, so a regenerated export is a different realised path, not an update.
#' The dataset is therefore frozen; regenerating it is an event, and
#' `R/verify-claims.R` in the book repository reports which printed numbers a
#' new export has invalidated.
#'
#' Two things about the model are choices rather than measurements and are
#' stated as such. Every euro-area equity carries an expected-return overlay of
#' +3 percentage points a year, a capital-market assumption that keeps
#' `EU_equity` from being excluded by every optimizer on the sample. And the
#' Swiss credit loadings are derived rather than measured, since no Swiss
#' credit series with usable history exists.
#'
#' Imported by `data-raw/simulated-market.R`, which places the data on the
#' model calendar, repairs one defect and verifies seven properties on every
#' run. The defect: the export labels the 50 bond instruments
#' `currency_basis = "local"` although their returns are in dollars, so a
#' screen on that column would drop every bond. Repaired here, and checked
#' against the exchange rates.
#'
#' All return panels are rectangular: every instrument has an observation in
#' every period, and nothing is padded or carried forward to make that true.
#' The factor paths are fixed, so this is one realised path rather than a
#' distribution over paths. And the equity series are individual securities
#' whose idiosyncratic components are drawn independently, so a portfolio of
#' them diversifies more readily than a real index would.
#'
#' @format
#' `sim_assets` is a tibble with 181 rows and 7 columns, one per instrument:
#' \describe{
#'   \item{asset_id}{Anonymised identifier, e.g. `US_EQU_01`, `EU_CRE_03`.}
#'   \item{region}{`US`, `EU`, `UK`, `CH`, `JP` or `GLOBAL`.}
#'   \item{currency, currency_basis}{Both `"US$"` and `"USD"` throughout, after
#'     the repair described above.}
#'   \item{asset_class}{`equity` (123), `credit` (30), `government` (15),
#'     `commodity` (8) or `money_market` (5).}
#'   \item{sector}{Equities only; `NA` elsewhere.}
#'   \item{credit_grade}{`IG` or `HY` for the 30 credit instruments; `NA`
#'     elsewhere.}
#' }
#'
#' The return tables share a layout: `date` (`Date`, on the model calendar), an
#' identifier, and `return`, a simple total return as a decimal — 0.0096 is
#' 0.96%. The identifier is `asset` in `sim_asset_returns_*` and `class` in
#' `sim_class_returns_*` and `sim_money_market_*`; the class tables carry an
#' additional `index`, a total-return index rebased to 100 at the first
#' observation. Monthly dates are the last day of the model month; weekly dates
#' are a week apart and each lies in the month whose return it compounds into.
#'
#' `sim_class_returns_monthly` has 16 × 372 rows; its classes are
#' `US_equity`, `US_government`, `US_credit`, the same three for `UK`, `EU`,
#' `CH` and `JP`, and `GLOBAL_commodity`. Equity classes are value-weighted on
#' the previous week's capitalisation; the others are equally weighted, because
#' no capitalisation exists for them. `sim_money_market_monthly` has 5 × 372
#' rows, one series per region.
#'
#' `sim_riskfree_monthly` and `sim_riskfree_weekly` hold `date`, `rate_annual`
#' (the quoted annual bill rate, a decimal) and `return` (the simple return over
#' one period): a three-month Treasury bill carried onto the model's grid,
#' negative in 1 of 372 months and annualising to 2.26% over the sample.
#'
#' `sim_fx_monthly` and `sim_fx_weekly` hold `date`, `region` (`EU`, `UK`, `CH`,
#' `JP`), `fx_return`, and at weekly frequency the level `rate`. The quote is one
#' unit of local currency in dollars, so a **positive** `fx_return` means the
#' local currency appreciated and an unhedged dollar investor gained.
#'
#' `sim_curve_weekly` holds `date`, `series`, `value` — Nelson-Siegel level,
#' slope and curvature for each of the five regions, plus ten credit spreads.
#' `sim_marketcap_weekly` holds `date`, `asset`, `market_cap` for the 123
#' equities, which is what a capitalisation-weighted benchmark needs; it
#' reaches back before model year 1, so that a lagged weight exists for the
#' first return. `sim_esg_changes_quarterly` holds `date` (the quarter end),
#' `asset` and `esg_change`, an integer change in an ESG rating for the
#' equities; it carries changes only, no starting level, and has no effect on
#' the returns.
#'
#' `sim_episodes` has one row per named episode: `episode` (the key that
#' [in_episode()] takes), `label` (the name used in the book's text), `start`
#' and `end` (a closed interval of dates) and `description`.
#'
#' @source Dataset version 6.2.0. Factor models estimated by historical
#'   simulation on market data from 1992 to 2022; the series themselves are
#'   modelled and contain no real data. The risk-free rate and the exchange
#'   rates enter the model as inputs. Import script in `data-raw/`, not shipped.
#'
#' @examples
#' # The allocation spine: sixteen risky classes.
#' head(sim_class_returns_monthly)
#' unique(sim_class_returns_monthly$class)
#'
#' # Crossing into the matrix PortfolioAnalytics wants, and into excess returns.
#' R  <- as_xts_pm(sim_class_returns_monthly, date, class, return)
#' Re <- R - sim_riskfree_monthly$return
#' round(sqrt(diag(cov(R)) * 12), 4)
#'
#' # The calendar is a model calendar: years 1 to 31.
#' range(model_year(zoo::index(R)))
#'
#' # A foreign money market is a currency position; hedging is a subtraction.
#' jp <- subset(sim_money_market_monthly, class == "JP_money_market")
#' fx <- subset(sim_fx_monthly, region == "JP")
#' c(unhedged = sd(jp$return),
#'   hedged   = sd(jp$return - fx$fx_return[match(jp$date, fx$date)])) * sqrt(12)
#'
#' @name sim-market
#' @keywords datasets
NULL

#' @rdname sim-market
"sim_assets"

#' @rdname sim-market
"sim_asset_returns_monthly"

#' @rdname sim-market
"sim_asset_returns_weekly"

#' @rdname sim-market
"sim_class_returns_monthly"

#' @rdname sim-market
"sim_class_returns_weekly"

#' @rdname sim-market
"sim_money_market_monthly"

#' @rdname sim-market
"sim_money_market_weekly"

#' @rdname sim-market
"sim_riskfree_monthly"

#' @rdname sim-market
"sim_riskfree_weekly"

#' @rdname sim-market
"sim_fx_monthly"

#' @rdname sim-market
"sim_fx_weekly"

#' @rdname sim-market
"sim_curve_weekly"

#' @rdname sim-market
"sim_marketcap_weekly"

#' @rdname sim-market
"sim_esg_changes_quarterly"

#' @rdname sim-market
"sim_episodes"
