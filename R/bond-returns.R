#' Total return of a constant-maturity par bond
#'
#' Two of the Tier 1 sleeves are not downloaded but constructed: free
#' total-return series for government bonds do not reach 1990, while yields do,
#' back to 1953. This function turns a constant-maturity yield series into the
#' monthly total return of a portfolio that holds a par bond of that maturity and
#' rolls it every month.
#'
#' No duration approximation is used. The bond is repriced with the full
#' closed-form bond price rather than a first-order expansion, which is both more
#' defensible and easier to explain — the duration story belongs in the
#' fixed-income chapter, not hidden in a data loader. The closed form is
#' evaluated at a fractional remaining maturity, which is itself an
#' idealisation of the stub period between coupon dates; see Details.
#'
#' @param yield Numeric vector of annualised yields in **decimal** form
#'   (0.0425, not 4.25), ordered in time, one observation per month, no gaps.
#'   Values above `1` are rejected rather than silently treated as decimals,
#'   because a percent-scaled series produces plausible-looking nonsense.
#'   Negative yields are allowed — they happened. Values at or below `-freq` are
#'   not: they make `1 + yield/freq` non-positive, so the discount factor is
#'   `NaN` and the sleeve would come back empty without saying why. `NA`
#'   propagates: an `NA` yield makes both the return of that month and the
#'   return of the next month `NA`.
#' @param maturity Constant maturity in years, e.g. `1` or `10`. A single finite
#'   value greater than one month (`1/12`); fractional values such as `2.5` are
#'   fine. `NA`, `Inf` and non-numeric input are refused with a message rather
#'   than propagated.
#' @param freq Coupon frequency per year. A single positive value. Defaults to
#'   `2`, matching US Treasury convention. Use `1` for an annual-coupon market
#'   such as most of the euro area. The choice moves the answer by a few basis
#'   points a year, not more.
#'
#' @return Numeric vector of monthly simple total returns in decimal form, the
#'   same length as `yield` and in the same order, with `NA` in the **first**
#'   position because a return needs two yields. Element `i` is the return earned
#'   over the month that ends at `yield[i]`. A zero-length input returns
#'   `numeric(0)`; a length-one input returns a single `NA`.
#'
#' @details
#' Each month the portfolio is assumed to hold a freshly issued par bond, so its
#' coupon equals last month's yield and its price is 100. One month later the
#' bond has one month less to run and is repriced at this month's yield. The
#' return is the price change plus one month of accrued coupon.
#'
#' Writing \eqn{c} for the coupon rate, \eqn{y} for the yield, \eqn{T} for the
#' remaining maturity and \eqn{f} for the coupon frequency, the price of a bond
#' per 100 of face value is
#'
#' \deqn{P = 100 \left[ \frac{c}{y}\left(1 - (1 + y/f)^{-fT}\right) + (1 + y/f)^{-fT} \right]}
#'
#' which holds for fractional \eqn{T} as well, so the eleven-months-and-a-bit
#' case needs no special treatment.
#'
#' **What this leaves out.** The roll is frictionless, there is no bid-ask
#' spread, and the par-bond assumption means the portfolio never holds a premium
#' or discount bond. Evaluating the formula at a fractional \eqn{T} discounts
#' every cash flow as though the coupon dates shifted with the valuation date,
#' rather than treating the first coupon as a stub. That is the one place the
#' word *approximation* still applies, and it is far smaller than the duration
#' error it replaces. For an asset-allocation sleeve at monthly frequency these
#' are all second-order; for anything at the instrument level they are not.
#'
#' @examples
#' # Ten-year yields falling from 4% to 3.5% over three months
#' y <- c(0.0400, 0.0390, 0.0370, 0.0350)
#' bond_total_return(y, maturity = 10)
#'
#' # A rate rise hurts a long bond far more than a short one
#' up <- c(0.0300, 0.0400)
#' c(short = bond_total_return(up, 1)[2],
#'   long  = bond_total_return(up, 10)[2])
#'
#' # On a flat curve the sleeve earns exactly its coupon, one twelfth a month.
#' # The twelve returns sum to the yield and compound to slightly more.
#' flat <- bond_total_return(rep(0.05, 13), maturity = 5)[-1]
#' c(sum = sum(flat), compounded = prod(1 + flat) - 1)
#'
#' # Percent input is refused rather than mis-scaled by a factor of a hundred.
#' try(bond_total_return(c(4.00, 4.10), maturity = 10))
#'
#' # A missing maturity gets the function's own message, not a bare R error
#' # about a missing value in an if() condition.
#' try(bond_total_return(c(0.04, 0.041), maturity = NA))
#'
#' @family constructed return series
#' @seealso [credit_total_return()], which wraps this at a fixed maturity and
#'   nets off a default-loss assumption.
#' @export
bond_total_return <- function(yield, maturity, freq = 2) {

  if (!is.numeric(yield)) {
    cli::cli_abort("{.arg yield} must be numeric.")
  }
  if (any(yield > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{.arg yield} must be in decimal form, not percent.",
      "x" = "Largest value seen: {.val {max(yield, na.rm = TRUE)}}.",
      "i" = "A 4.25% yield is {.val 0.0425}."
    ))
  }
  # `is.finite()` rather than a bare comparison: `if (NA <= 1/12)` is a bare R
  # error about a missing value, which tells a reader nothing about what this
  # function wanted. It also catches Inf, NaN and a character maturity.
  if (length(maturity) != 1L || !is.finite(maturity) || maturity <= 1 / 12) {
    cli::cli_abort(c(
      "{.arg maturity} must be a single value greater than one month.",
      "x" = "Got {.val {maturity}}.",
      "i" = "It is a number of years: {.code 1} for a one-year bond, {.code 10} for a ten-year."
    ))
  }
  if (length(freq) != 1L || !is.finite(freq) || freq <= 0) {
    cli::cli_abort(c(
      "{.arg freq} must be a single positive number of coupons per year.",
      "x" = "Got {.val {freq}}.",
      "i" = "{.code 2} is the US Treasury convention, {.code 1} an annual-coupon market."
    ))
  }
  # A yield at or below -freq makes 1 + yield/freq zero or negative, and a
  # non-positive base raised to the fractional exponent -freq * maturity is NaN
  # (or Inf at the boundary). That is a silent all-NA sleeve, so refuse it here.
  # This is a bound on absurdity, not on sign: realistic negative yields such as
  # -0.01 are far from it and price normally, which the tests pin.
  if (any(yield <= -freq, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{.arg yield} must be greater than {.code -freq}, i.e. above {.val {-freq}}.",
      "x" = "Smallest value seen: {.val {min(yield, na.rm = TRUE)}}.",
      "i" = "Below that the discount factor is undefined and the result would be
             silently {.code NaN}.",
      "i" = "Negative yields are allowed and priced normally; a value this far
             below zero is a sign or scale problem in the series, not a market."
    ))
  }

  n <- length(yield)
  if (n < 2L) return(rep(NA_real_, n))

  # Price per 100 face of a bond with coupon rate `coupon`, remaining maturity
  # `t_rem` years, priced at `y`. Closed form, valid for fractional maturities.
  price <- function(coupon, y, t_rem) {
    disc <- (1 + y / freq)^(-freq * t_rem)
    ifelse(
      abs(y) < 1e-10,
      100 * (1 + coupon * t_rem),          # zero-yield limit
      100 * ((coupon / y) * (1 - disc) + disc)
    )
  }

  y0 <- yield[-n]      # yield at which the bond was bought, at par
  y1 <- yield[-1]      # yield one month later

  p1       <- price(coupon = y0, y = y1, t_rem = maturity - 1 / 12)
  accrued  <- 100 * y0 / 12

  c(NA_real_, (p1 + accrued) / 100 - 1)
}


#' Approximate credit total return from a yield series
#'
#' Investment-grade credit has the same problem as government bonds — free
#' total-return history does not reach 1990, but Moody's seasoned corporate bond
#' yields reach 1919. This wraps [bond_total_return()] at a fixed assumed
#' maturity and subtracts an assumed default loss.
#'
#' @param yield Numeric vector of annualised yields in decimal form, ordered in
#'   time, one observation per month. Validated by [bond_total_return()], so a
#'   percent-scaled series is rejected here too.
#' @param maturity Assumed constant maturity in years. Moody's seasoned indices
#'   track bonds with roughly twenty years remaining, which is the default.
#' @param annual_loss Assumed annual credit loss from default, net of recovery,
#'   in decimal. Subtracted in equal monthly instalments of `annual_loss / 12`.
#'   Default `0` returns the gross figure; the panel built by [pm_init()] uses
#'   `0.004`, which is roughly the long-run realised loss rate on Baa credit.
#'
#' @return Numeric vector of monthly simple total returns in decimal form, the
#'   same length as `yield`, `NA` in the first position. Identical to
#'   `bond_total_return(yield, maturity)` less the constant `annual_loss / 12`,
#'   so the loss assumption shifts the mean and leaves the volatility untouched —
#'   which is precisely the criticism in Details.
#'
#' @details
#' **This is an approximation and the book says so.** Two assumptions are doing
#' visible work: a constant twenty-year maturity, and a constant default loss
#' that in reality is strongly cyclical and clusters exactly when the sleeve is
#' already losing money. The second matters more than the first, and it means
#' this series understates credit's downside in a crisis — which is the moment
#' an allocator cares about.
#'
#' The assumption is exposed as an argument rather than buried so that a reader
#' can vary it and see what changes.
#'
#' @examples
#' y <- c(0.0550, 0.0560, 0.0540)
#' credit_total_return(y, annual_loss = 0.004)
#'
#' # Vary the assumption and see what it costs: forty basis points a year is
#' # a level shift, not a change in risk.
#' vapply(c(0, 0.002, 0.004, 0.008),
#'        function(l) mean(credit_total_return(y, annual_loss = l), na.rm = TRUE),
#'        numeric(1))
#'
#' @family constructed return series
#' @seealso [bond_total_return()] for the pricing that sits underneath.
#' @export
credit_total_return <- function(yield, maturity = 20, annual_loss = 0) {
  gross <- bond_total_return(yield, maturity = maturity)
  gross - annual_loss / 12
}
