



#' Normalize counts for each row of samples
#'
#' @param count_mat count matrix (nsample*nfeature)
#' @param libsize library size to normalize at
#' @export
#' @return normalized count matrix
#'
normalize <- function(count_mat, libsize=1){
    normalized_count_mat <- t(apply(count_mat, 1, function(row) {
        row_sum <- sum(row)
        row / row_sum * libsize
    }))

    return(normalized_count_mat)
}




#' Impute the zeros with a pseudocount
#'
#' @param count_mat count matrix (nsample*nfeature)
#' @param scale for an entry with zeros, impute the zeros with scale* smallest nonzero value in each row (sample)
#'  @return imputed count matrix
#' @export
simple_impute <- function(count_mat, scale=0.5){

    row_impute <- function(values){
        if (sum(values == 0) > 0){
            values[values == 0] <- min(values[values > 0])*scale
        }
        return(values)
    }
    imputed_count_mat <- apply(count_mat, 1, row_impute)
    return(imputed_count_mat)

}


#' centered log ratio transformation
#'
#' @param count_mat count matrix (nsample*nfeature)
#' @param impute_scale for an entry with zeros, impute the zeros with scale* smallest nonzero value in the column
#'  @return clr transformed matrix
#' @export
#' @importFrom compositions clr
clr_transform <- function(count_mat, impute_scale=0.5){

    normalized_count <- normalize(count_mat=count_mat)
    imputed_normalized_count <- simple_impute(count_mat=normalized_count, scale=impute_scale) |>
        t()

    nsamples <- nrow(imputed_normalized_count)
    clr_counts <- matrix(0, nrow=nrow(imputed_normalized_count),
                         ncol=ncol(imputed_normalized_count))

    for (j in 1:nsamples){
        clr_counts[j, ] <- clr(imputed_normalized_count[j, ])
    }

    return(clr_counts)

}



#' Calculate distance between sample compositions and carry out statistical tests
#'
#' @param count_mat count matrix
#' @param metric metric for calculating sample distance, by default bray-curtis
#' @param nperm number of permutations needed for permanova test
#' @param covariate vector of a covariate for permanova test
#' @importFrom vegan vegdist adonis2
#' @importFrom stats cmdscale
#'
#' @export
#' @return a list of distance matrix, MDS coordinates and permanova test result
#'
dist_composition <- function(count_mat, metric="bray", nperm=999, covariate=NULL){

    # calculate composition
    composition_mat <- normalize(count_mat)

    # calculate pairwise distance
    distances <- vegdist(composition_mat, method=metric)

    # multidimensional scaling plot coordinates
    mds_coordinates <- cmdscale(distances, k=2) |> as.data.frame()

    permanova_test <- NULL
    if (!is.null(covariate)){
        # permanova test
        permanova_test <- adonis2(formula=distances ~ covariate, permutations=nperm)
        mds_coordinates$var <- covariate
    }

    return(list(dist_mat = distances, mds_coordinates=mds_coordinates,
                permanova=permanova_test))

}







