

#' Matrix generation from Bernoulli random variables
#'
#'@param prob_mat probability matrix
#'
#'@returns a binary matrix
#'@export
#'@importFrom stats rbinom
binary_simulation <- function(prob_mat){
    binary_matrix <- matrix(rbinom(length(prob_mat), size = 1, prob = prob_mat),
                            nrow = nrow(prob_mat), ncol = ncol(prob_mat))
    return(binary_matrix)
}



#' Matrix generation from Gamma distribution
#' @param mean_mat mean of gamma distribution for each entry
#' @param scale scale of gamma distribution for all entries
#' @returns matrix of values from gamma dstributions
#' @importFrom stats rgamma
gamma_simulation <- function(mean_mat, scale=1){

    shape_mat <- mean_mat / scale
    gamma_matrix <- matrix(rgamma(length(shape_mat),
                                  shape = shape_mat,
                                  scale = scale),
                           nrow = nrow(shape_mat),
                           ncol = ncol(shape_mat))

    return(gamma_matrix)

}


#' Median of zero inflated Gamma distribution
#' @param zero_prob zero inflation probability
#' @param mean_mat mean matrix
#' @param scale gamma distribution scale
#' @importFrom  stats qgamma
ZIG_median <- function(zero_prob, mean_mat, scale=1){

    median_mat <- matrix(0, nrow=nrow(zero_prob),
                         ncol=ncol(zero_prob))

    median_mat[zero_prob >=0.5] <- 0

    gshape <- mean_mat / scale
    gscale <- scale

    mask <- zero_prob < 0.5
    trunc_quantile <- (0.5 - zero_prob[mask]) / (1 - zero_prob[mask])

    median_mat[mask] <- qgamma(p=trunc_quantile,
                               shape=mean_mat[mask] / gscale,
                               scale=gscale)

    return(median_mat)

}


#' Variance of zero inflated gamma distribution
#' @param zero_prob zero inflation probability
#' @param mean_mat mean matrix
#' @param scale gamma distribution scale
ZIG_variance <- function(zero_prob, mean_mat, scale=1){
    (1-zero_prob)*mean_mat*scale + zero_prob*(1-zero_prob)*mean_mat^2
}

