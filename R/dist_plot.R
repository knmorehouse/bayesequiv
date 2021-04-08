
#' Bayesian Equivalence Testing (BET): Visualize Model
#'
#' This function reduces six important BET inspection steps into one. That is, this helper function
#' cautomates the following: 1) visualizing your model (relies on 'sjPlot');
#' 2) examine priors (relies on)
#' @param model_name Name of model
#' @param main Title of Graph
#' @param lower Lower bound of x axis
#' @param lower Upper bound of x axis
#' @keywords Bayesian Equivalence Testing
#' @export
#' @examples
#' tess_glm <- stan_glm(FINAL_1mother_0other ~ condition, family = "binomial",
#' data = tess_analyze[tess_analyze$seen_before == 3, ], iter = 10000, seed = 1839,
#' diagnostic_file = file.path(tempdir(), "df.csv"))
#'  inspect_model(tess_glm)

dist_plot <- function(df, main, lower, upper){
  plot(density(df$mean), main = main, xlim=c(lower,upper), #axes = FALSE,
       xlab = "OR", col = "transparent")
  #axis(1)
  #axis(2)
  polygon(x = c(density(df$mean)$x[density(df$mean)$x < .9],
                max(density(df$mean)$x[density(df$mean)$x < .9]),
                min(density(df$mean)$x[density(df$mean)$x < .9])),
          y = c(density(df$mean)$y[density(df$mean)$x < .9], 0, 0),
          col = rainbow(2, alpha = 0.4)[2], border = "transparent")
  polygon(x = c(density(df$mean)$x[(density(df$mean)$x > .9) & (density(df$mean)$x < 1.1)],
                1.1, .9),
          y = c(density(df$mean)$y[(density(df$mean)$x > .9) & (density(df$mean)$x < 1.1)], 0, 0),
          col = rainbow(2, alpha = 0.4)[1], border = "transparent")
  polygon(x = c(density(df$mean)$x[density(df$mean)$x > 1.1],
                max(density(df$mean)$x[density(df$mean)$x > 1.1]),
                min(density(df$mean)$x[density(df$mean)$x > 1.1])),
          y = c(density(df$mean)$y[density(df$mean)$x > 1.1], 0, 0),
          col = rainbow(3, alpha = 0.4)[3], border = "transparent")
  abline(v = .9, col = "red")
  abline(v = 1.1,  col = "red")
  abline(v = mean(df$mean), lty = 2)
}
