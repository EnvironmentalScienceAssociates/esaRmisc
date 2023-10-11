#' Exceedance Probability
#'
#' Calculate exceedance probability for a flow time series
#'
#' @md
#' @param flow     Flow time series
#'
#' @export
#' @examples
#' exceedance_prob(runif(30, 0, 10000))
#'

exceedance_prob <- function(flow){
  # follows https://streamflow.engr.oregonstate.edu/analysis/flow/index.htm
  # this function is unoptimized and slow on larger datasets
  n = length(flow)
  # rank function ranks smallest to biggest; need to reverse that via subtraction
  M = n + 1 - rank(flow, ties.method = "max")
  M/(n + 1)
}


#' Exceedance Flow
#'
#' Calculate flow value that meet a vector of exceedance probabilities
#'
#' @md
#' @param flow     Flow time series
#' @param probs    Exceedance probabilities
#'
#' @export
#' @examples
#' exceedance_flow(runif(30, 0, 10000), seq(0.1, 0.9, 0.1))
#'

exceedance_flow <- function(flow, probs){
  p = exceedance_prob(flow)
  df1 = unique(data.frame(flow = flow, prob = p))
  df2 = df1[order(df1$prob),]
  fn = approxfun(x = df2$prob, y = df2$flow)
  fn(probs)
}

