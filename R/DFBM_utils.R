

#' Select Cutoffs
#'
#'
#' @param count_mat count matrix
#' @param begin smallest threshold
#' @param rate geometric sequence rate for selecting thresholds
#' @param gap minimum proportion of entries that change from1 to zero between adjacent thresholds
#' @param min_prop the minimum of the proportion of entries larger than the largest threshold
#'
#' @returns a list with the sequence of thresholds and the proportion of entries larger than each entry
#' @export
cutoff_selection <- function(count_mat, begin=0, rate=0.9, gap=0.005, min_prop=0.005){

    unique_values <- unique(as.vector(count_mat)) |> sort()
    props <- rep(0, length(unique_values))
    for(j in 1:length(unique_values)){
        props[j] <- mean(count_mat > unique_values[j])
    }
    cutoffs <- c(begin)
    selected_props <- c(mean(count_mat > begin))
    current_prop <- mean(count_mat > cutoffs[1])
    while(1){
        next_prop <- min(current_prop * rate, current_prop - gap)
        id <- which.min(props >= next_prop)
        cutoffs <- c(cutoffs, unique_values[id])
        current_prop <- props[id]
        selected_props <- c(selected_props, current_prop)
        if (current_prop < min_prop){
            break
        }
    }
    return(list(thresholds=cutoffs, props=selected_props))

}


#' Tune the performance of DFBM with different number of thresholds
#'
#' @param X count matrix
#' @param rates a vector of geometric sequence rates to tune for adjusting the number of thresholds. The higher the rate the more thresholds there are
#' @param gap minimum of the proportion of entries that change from one to zero between adjacent thresholds
#' @param min_prop minimum of the proportion of entries being larger than the largest threshold
#' @param truth true expected abundance matrix
#'
#' @returns the 25%, 50% and 75% percentile of median absolute error to the mean of each entry
#' @importFrom DFBM dfbm
#' @importFrom stats quantile
#' @export
tune_dfbm <- function(X, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93, 0.95), gap=0.002, min_prop=0.002, truth){

    errors <- matrix(0, nrow=3, ncol=length(rates))
    thresholds_list <- list()
    cdf_list <- list()
    for (j in 1:length(rates)){
        print(j)
        cutoffs_choice <- cutoff_selection(X, rate=rates[j], gap=gap, min_prop=min_prop)
        thresholds_list[[j]] <- cutoffs_choice$thresholds
        cdf_list[[j]] <- cutoffs_choice$props
        denoise_result <- dfbm(count_mat=X, cutoffs=cutoffs_choice$thresholds,
                               ncores=4)
        errors[, j] <- quantile(abs(denoise_result$denoised_counts - truth), c(0.25, 0.5, 0.75))
    }

    return(list(thresholds_list=thresholds_list,
                cdf_list=cdf_list,
                errors=errors))

}



#' Plot the mean absolute error against the number of thresholds
#'
#' @param performance MAE of the same DFBM model with different number of thresholds
#' @returns a ggplot with number of thresholds on x axis and MAE on y axis
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_errorbar scale_x_continuous ylab .data
plot_error <- function(performance){

    num_thresholds <- rep(0, length(performance$thresholds_list))
    for(j in 1:length(performance$thresholds_list)){
        num_thresholds[j] <- length(performance$thresholds_list[[j]])
    }
    performance_df <- data.frame(cbind(num_thresholds, t(performance$errors)))
    colnames(performance_df) <- c("Num_Threshold", "lower", "med", "upper")
    performance_df$ID <- seq(nrow(performance_df))
    output <- ggplot(performance_df, aes(x=.data$ID, y=.data$med)) + geom_point() + geom_line() +
        geom_errorbar(aes(ymin=.data$lower, ymax=.data$upper, width=0.01)) +
        scale_x_continuous(name="Number of thresholds", breaks=seq(1, nrow(performance_df)),
                           labels=as.character(performance_df$Num_Threshold)) +
        ylab("Absolute Error")

    return(output)

}

#' Visualize the optimal rank of matrix factorization for binary masks
#'
#' @param result DFBM result object
#' @details
#' For each binary masks from each threshold, the optimal rank for matrix factorization is different. Here I plot
#' the thresholds on the x axis and optimal ranks on the y axis
#' @returns a ggplot with threshold on x axis and Rank on y axis
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous scale_y_continuous .data
#'
plot_rank <- function(result){

    thresholds <- result$thresholds
    ranks <- result$optimal_ranks
    min_rank <- min(ranks)
    max_rank <- max(ranks)
    rank_df <- data.frame(Threshold=as.character(thresholds),
                          Rank=ranks)
    rank_df$ID <- seq(1, nrow(rank_df))
    rank_plot <- ggplot(rank_df, aes(x=.data$ID, y=.data$Rank)) + geom_point() + geom_line()+
        scale_x_continuous(name="Threshold", breaks=rank_df$ID, labels=rank_df$Threshold) +
        scale_y_continuous(name="Rank", breaks=seq(min_rank, max_rank))
    rank_plot

}


#' Visualize binary masks and save as png files
#'
#' @param count_mat count matrix
#' @param thresholds vector of thresholds
#' @param folder folder to store the plots
#' @param cmap color map
#' @param include_na Whether to highlight entries not used for matrix factorization as missing
#'
#' @importFrom circlize colorRamp2
#'
plot_binary_mat <- function(count_mat, thresholds, folder,
                            cmap=colorRamp2(c(0, 1), c("white", "#2c7cc7")), include_na=F){

    bin_mat <- 1*(count_mat > thresholds[1])
    filename <- file.path(folder, sprintf("binary_%d.png", thresholds[1]))
    save_heatmap(X=t(bin_mat), entry_name="binary", filename=filename,
                 cmap=cmap, heatmap_ratio=0.98, pad=0.05)

    for (j in 2:length(thresholds)){
        bin_mat <- 1*(count_mat > thresholds[j])
        if (include_na){
            bin_mask <- count_mat <= thresholds[j-1]
            bin_mat[bin_mask] <- NA
        }
        filename <- file.path(folder, sprintf("binary_%d_%d.png", thresholds[j-1], thresholds[j]))
        save_heatmap(X=t(bin_mat), entry_name="binary", filename=filename,
                     cmap=cmap, heatmap_ratio=0.98, pad=0.05)
    }

}



#' Visualize probability matrices and save as png files
#'
#' @param result DFBM result object
#' @param folder folder to store the plots
#' @param cmap color map
#'
#' @importFrom circlize colorRamp2
#'
plot_prob_mat <- function(result, folder, cmap=colorRamp2(c(0, 1), c("white", "#2c7cc7"))){

    thresholds <- result$thresholds
    prob_mats <- result$prob_mats

    filename <- file.path(folder, sprintf("prob_%d.png", thresholds[1]))
    save_heatmap(X=t(prob_mats[[1]]), entry_name="Prob", filename=filename,
                 cmap=cmap, heatmap_ratio=0.98, pad=0.05)
    for (j in 2:length(thresholds)){
        filename <- file.path(folder, sprintf("prob_%d_%d.png", thresholds[j-1], thresholds[j]))
        save_heatmap(X=t(prob_mats[[j]]), entry_name="Prob", filename=filename,
                     cmap=cmap, heatmap_ratio=0.98, pad=0.05)
    }

}


