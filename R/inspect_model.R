
#' Bayesian Equivalence Testing (BET): Inspect Model
#'
#' This function reduces six important BET inspection steps into one. That is, this helper function
#' cautomates the following: 1) visualizing your model (relies on 'sjPlot');
#' 2) examine priors (relies on)
#' @param model_name Name of model
#' @keywords Bayesian Equivalence Testing
#' @export
#' @examples
#' tess_glm <- stan_glm(FINAL_1mother_0other ~ condition, family = "binomial",
#' data = tess_analyze[tess_analyze$seen_before == 3, ], iter = 10000, seed = 1839,
#' diagnostic_file = file.path(tempdir(), "df.csv"))
#'  inspect_model(tess_glm)

inspect_model <- function(model_name){
  dv <- model_name$formula[[2]]
  iv <- model_name$formula[[3]]
  print("----------------------------Model----------------------------")
  print(model_name$call)
  print("------------------------Inspect Model------------------------")
  print("STEP 1: VISUALIZE MODEL")
  plot_model(model_name, bpe = "mean", type = "pred", terms = iv)
  readline(prompt="Press [enter] to examine priors")
  print("STEP 2: EXAMINE PRIORS")
  print(prior_summary(model_name))
  readline(prompt="Press [enter] to check sampling quality")
  print("STEP 3: CHECK SAMPLING QUALITY")
  print(stan_rhat(model_name))
  readline(prompt="Press [enter] to examine trace plots")
  print("STEP 4: EXAMINE TRACE PLOTS")
  post.model_name <- as.array(model_name)
  print(mcmc_trace(model_name))
  readline(prompt="Press [enter] to examine number of effective sample sizes (1/2)")
  print("STEP 5: N EFFECTIVE SAMPLE SIZES")
  print(neff_ratio(model_name))
  readline(prompt="Press [enter] to examine number of effective sample sizes (2/2)")
  print(mcmc_neff(neff_ratio(model_name), size = 2))
  readline(prompt="Press [enter] for posterior predictive check")
  print("STEP 6: POSTERIOR PREDICTIVE CHECK")
  pp_check(model_name, nreps = 100)
}
