#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom zoo coredata
#' @importFrom zoo index
## usethis namespace: end
NULL

# Roadmap. Each block corresponds to a chapter of the book; a function is only
# added here once the chapter that needs it is being written, so that nothing
# ships without a worked example behind it.
#
#  ch. 1  as_pm(), as_xts_pm()             tibble <-> xts boundary
#  ch. 2  pm_init(), pm_data(), pm_check(), pm_sources(),
#         pm_cache_dir(), pm_universe(), pm_screen()   reproducible data layer
#         sim_dir(), sim_assets(), sim_returns(), sim_riskfree(), sim_fx(),
#         sim_bond_factors(), sim_market_caps()        the simulated market
#  ch. 3  to_real(), hedge_currency()      conventions
#         (sim_returns(currency=) hedges the simulated market only; the general
#          function still has to be written)
#  ch. 4  equilibrium_returns()            reverse optimisation
#  ch. 6  sharpe_gap()                     the mean and covariance channels
#  ch. 10 esg_screen(), green_factor()     screens from public data
#  ch. 16 ppp_fit(), ppp_weights()         parametric portfolio policies
#  ch. 17 detect_regime()
#  ch. 19 curve_pca(), diebold_li(), duration_overlay()
#  ch. 20 rebalance_bands()
#  ch. 21 max_ir(), te_constraint(), attribute_active()
#  ch. 22 diagnose_rebalancing()
#  ch. 23 rafe(), c_rafe()
#  ch. 24 trafe_loss(), correct_moments()
#  ch. 25 resample_frontier(), kl_ball()
#  ch. 26 crra_objective(), copula_moments()
#  appx.  package_notes()                  generates the Package Notes appendix
