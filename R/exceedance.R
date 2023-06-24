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
#' Calculate flow value that meets a given exceedance probability
#'
#' @md
#' @param flow     Flow time series
#' @param prob     Exceedance probability
#'
#' @export
#' @examples
#' exceedance_flow(runif(30, 0, 10000), 0.5)
#'

exceedance_flow <- function(flow, prob){
  p = exceedance_prob(flow)
  df1 = data.frame(flow = x, prob = p, p_diff = abs(p - prob))
  df2 = unique(df1[order(df1$p_diff),])
  # first two rows of df2 are used for interpolation
  df2$flow[1] + (df2$flow[2] - df2$flow[1])/(df2$prob[2] - df2$prob[1]) * (prob - df2$prob[1])
}

