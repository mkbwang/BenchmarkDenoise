

#' Calculate the frobenious norm error between two composition matrix
#'
#' @param truth true abundance matrix
#' @param estimation estimated abundance matrix
#' @returns a scalar representing frobenious norm error
frobenius_norm <- function(truth, estimation){

    normalized_truth <- normalize(truth)
    normalized_estimation <- normalize(estimation)

    mean_squares <- mean((normalized_truth - normalized_estimation)^2)
    return(sqrt(mean_squares))

}



#' Calculate the KL divergence between true composition and estimated composition
#'
#' @param truth true abundance matrix
#' @param estimation estimated abundance matrix
#' @returns a scalar representing average KL divergence of sample compositions
#'
KL_divergence <- function(truth, estimation){

    normalized_truth <- normalize(truth)
    normalized_truth <- simple_impute(normalized_truth)
    normalized_estimation <- normalize(estimation)
    normalized_estimation <- simple_impute(normalized_estimation)

    divergence <- rowSums(normalized_truth *
                              (log(normalized_truth) - log(normalized_estimation)))

    mean_divergence <- mean(divergence)

    return(mean_divergence)

}


#' Calculate the Bray-Curtis distance between true composition and estimated composition
#'
#' @param truth true abundance matrix
#' @param estimation estimated abundance matrix
#' @returns a scalar representing average bray-curtis distance
#'
bray_curtis_distance <- function(truth, estimation){

    normalized_truth <- normalize(truth)
    normalized_estimation <- normalize(estimation)

    bc_distance <- rowSums(abs(normalized_truth - normalized_estimation)/2)

    return(mean(bc_distance))

}


#' Calculate average correlation of relative abundance for all the features
#'
#' @param truth true abundance matrix
#' @param estimation estimated abundance matrix
#' @returns a scalar representing average correlation with truth
#' @importFrom stats cor
feature_correlation <- function(truth, estimation){

    normalized_truth <- normalize(truth)
    normalized_estimiation <- normalize(estimation)

    feature_correlation <- mapply(function(x, y) cor(x, y),
                                  as.data.frame(normalized_truth),
                                  as.data.frame(normalized_estimiation))

    mean_correlation <- mean(feature_correlation)
    return(mean_correlation)

}



#' Calculate coefficient of variation of relative abundance for all the features
#'
#' @param truth true abundance matrix
#' @param estimation estimated abundance matrix
#' @returns a scalar representing average coefficient of variation difference
#'
coef_var <- function(truth, estimation){

    normalized_truth <- normalize(truth)
    normalized_estimation <- normalize(estimation)

    mean_truth <- colMeans(normalized_truth)
    sd_truth <- apply(normalized_truth, 2, sd)
    cv_truth <- mean_truth/sd_truth

    mean_estimation <- colMeans(normalized_estimation)
    sd_estimation <- apply(normalized_estimation, 2, sd)
    cv_estimation <- mean_estimation/sd_estimation

    cv_error <- mean((cv_truth - cv_estimation)^2)

    return(cv_error)

}
