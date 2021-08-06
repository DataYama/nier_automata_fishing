library(docstring)

get_hdi_from_posterior <- function(sample, ci = 0.95) {
  #' Computes a credible interval from a posterior distribution of frequency probability
  #' 
  #' The interval returned is a high density interval, whose computation method was taken from bayestestR package. 
  #' See https://easystats.github.io/bayestestR/articles/credible_interval.html for more information
  #'
  #' @param sample A vector representing the posterior distribution of frequency probability.
  #' @param ci A number between 0 and 1 referring to the probability that the true value falls within the credible interval.
  #' @return A names vector giving credible high density interval
  #' @examples
  #' get_hdi_from_posterior(sample = rnorm(1e6))
  #' get_hdi_from_posterior(sample = rnorm(1e6), ci = 0.99)
  #' 
  
  sorted_sample <- sort(sample)
  ci_size <- ceiling(ci * length(sorted_sample))
  ci_possiblities <- length(sorted_sample) - ci_size
  ci_amplitude = rep(0 ,ci_possiblities)
  for (i in 1:ci_possiblities) {
    ci_amplitude[i] <- sorted_sample[i + ci_size] - sorted_sample[i]
  }
  hdi_min <- sorted_sample[which.min(ci_amplitude)]
  hdi_max <- sorted_sample[which.min(ci_amplitude) + ci_size]
  hdi <- c(hdi_min = hdi_min, hdi_max = hdi_max)
  return(hdi)
  
}

#' Computes a posterior distribution of frequency probability given a number of fish catches
#' 
#' Prior distribution currently use a uniform distribution, might be improve later with data from previous years for example.
#' The prior value are the results of a fitted beta distribution from each events.
#'
#' @param catches A number giving the number of fish catches from a single species in a biome
#' @param total A number giving the total number of fishes catches in a biome
#' @param prior_shape1 shape1 parameter for the beta distribution of prior (see ?rbeta). Default at 1 when the prior is unknown.
#' @param prior_shape2 shape2 parameter for the beta distribution of prior (see ?rbeta). Default at 1 when the prior is unknown.
#' @param ci A number between 0 and 1 referring to the probability that the true value falls within the credible interval.
#' @param n_samples A number specifying the size of the sampling the higher the value the more precision will 
#' be obtain for credible interval estimation, at the cost of computing time
#' @return A tibble giving the estimated mean and median frequency as well as the credible high density interval (see get_hdi_from_posterior) 
#' @examples
#' get_hdi_from_posterior(sample = rnorm(1e6))
#' get_hdi_from_posterior(sample = rnorm(1e6), ci = 0.99)
#' 
get_hdi <- function(catches, total, prior_shape1 = 1, prior_shape2 = 1, ci = 0.95, n_samples = 1e5){
  # Default at 1 when the prior is unknown.
  
  posterior <- rbeta(n = n_samples, shape1 = prior_shape1 + catches, shape2 = prior_shape2 + total - catches)
  hdi <- get_hdi_from_posterior(posterior, ci = ci)
  
  return(
    tibble(
      freq_mean = mean(posterior),
      freq_median = median(posterior),
      hdi_low = hdi[1],
      hdi_high = hdi[2]
    )
  )
  
}
