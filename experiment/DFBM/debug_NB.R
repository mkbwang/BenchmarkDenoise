
rm(list=ls())
library(ComplexHeatmap)
library(circlize)
library(DFBM)


nb_count_05 <- read.csv("inst/extdata/NB/nb_count_05.csv", row.names=1) |> as.matrix()
nb_count_1 <- read.csv("inst/extdata/NB/nb_count_1.csv", row.names=1) |> as.matrix()
nb_count_2 <- read.csv("inst/extdata/NB/nb_count_2.csv", row.names=1) |> as.matrix()
nb_count_5 <- read.csv("inst/extdata/NB/nb_count_5.csv", row.names=1) |> as.matrix()
nb_count_10 <- read.csv("inst/extdata/NB/nb_count_10.csv", row.names=1) |> as.matrix()
nb_mean <- read.csv("inst/extdata/NB/nb_true_mean.csv", row.names=1) |> as.matrix()
sample_types <- scan(file="inst/extdata/NB/sample_types.txt", character(), quote="")
feature_types <- scan(file="inst/extdata/NB/feature_types.txt", character(), quote="")



# visualization setup
color_gray="#808080"
color_red="#db382c"
color_green="#2f7028"
color_brown="#665223"
color_blue="#344885"
color_magenta="#b538b3"
col_fun_count <- colorRamp2(c(0, 70), c("white", "#e30022"))
col_fun_ecdf <- colorRamp2(c(0, 1), c("white", "#00009c"))
col_fun_bin <- colorRamp2(c(0, 1), c("white", "#555555"))

col_annotation <- HeatmapAnnotation(
  Sample = sample_types,
  col = list(Sample = c("Type1" = color_gray,
                        "Type2" = color_red,
                        "Type3"=color_green)),
  show_legend = FALSE,
  show_annotation_name = FALSE,
  annotation_height = unit(0.1, "npc"),
  which="column"
)

row_annotation <- HeatmapAnnotation(
  Feature = feature_types,
  col = list(Feature = c("Group1" = color_brown,
                         "Group2" = color_blue,
                         "Group3"=color_magenta)),
  show_legend = FALSE,
  show_annotation_name = FALSE,
  annotation_width = unit(0.1, "npc"),
  which="row"
)

folder <- "experiment/DFBM/debug_NB"

# source("viz_utils.R")
#
# cutoff_selection <- function(count_mat, begin=0, rate=0.9, gap=0.005, min_prop=0.00){
#
#   unique_values <- unique(as.vector(count_mat)) |> sort()
#   props <- rep(0, length(unique_values))
#   for(j in 1:length(unique_values)){
#     props[j] <- mean(count_mat > unique_values[j])
#   }
#   cutoffs <- c(begin)
#   selected_props <- c(mean(count_mat > begin))
#   current_prop <- mean(count_mat > cutoffs[1])
#   while(1){
#     next_prop <- min(current_prop * rate, current_prop - gap)
#     id <- which.min(props >= next_prop)
#     cutoffs <- c(cutoffs, unique_values[id])
#     current_prop <- props[id]
#     selected_props <- c(selected_props, current_prop)
#     if (current_prop < min_prop){
#       break
#     }
#   }
#   return(list(thresholds=cutoffs, props=selected_props))
#
# }
#
#
# tune_dfbm <- function(X, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93, 0.95)){
#
#   errors <- matrix(0, nrow=3, ncol=length(rates))
#   thresholds_list <- list()
#   cdf_list <- list()
#   for (j in 1:length(rates)){
#     print(j)
#     cutoffs_choice <- cutoff_selection(X, rate=rates[j], gap=0.002, min_prop=0.002)
#     thresholds_list[[j]] <- cutoffs_choice$thresholds
#     cdf_list[[j]] <- cutoffs_choice$props
#     denoise_result <- dfbm(count_mat=X, cutoffs=cutoffs_choice$thresholds,
#                            ncores=4)
#     errors[, j] <- quantile(abs(denoise_result$denoised_counts - nb_mean), c(0.25, 0.5, 0.75))
#   }
#
#   return(list(thresholds_list=thresholds_list,
#               cdf_list=cdf_list,
#               errors=errors))
#
# }
#
#
# plot_error <- function(performance){
#
#   num_thresholds <- rep(0, length(performance$thresholds_list))
#   for(j in 1:length(performance$thresholds_list)){
#     num_thresholds[j] <- length(performance$thresholds_list[[j]])
#   }
#   performance_df <- data.frame(cbind(num_thresholds, t(performance$errors)))
#   colnames(performance_df) <- c("Num_Threshold", "lower", "med", "upper")
#   performance_df$ID <- seq(nrow(performance_df))
#   output <- ggplot(performance_df, aes(x=ID, y=med)) + geom_point() + geom_line() +
#     geom_errorbar(aes(ymin=lower, ymax=upper, width=0.01)) +
#     scale_x_continuous(name="Number of thresholds", breaks=seq(1, nrow(performance_df)),
#                        labels=as.character(performance_df$Num_Threshold)) +
#     ylab("Absolute Error")
#
#   return(output)
#
# }
#
# plot_rank <- function(result){
#
#   thresholds <- result$thresholds
#   ranks <- result$optimal_ranks
#   min_rank <- min(ranks)
#   max_rank <- max(ranks)
#   rank_df <- data.frame(Threshold=as.character(thresholds),
#                         Rank=ranks)
#   rank_df$ID <- seq(1, nrow(rank_df))
#   rank_plot <- ggplot(rank_df, aes(x=ID, y=Rank)) + geom_point() + geom_line()+
#     scale_x_continuous(name="Threshold", breaks=rank_df$ID, labels=rank_df$Threshold) +
#     scale_y_continuous(name="Rank", breaks=seq(min_rank, max_rank))
#   rank_plot
#
# }
#
#
#
#
# plot_prob_mat <- function(result, folder, cmap=colorRamp2(c(0, 1), c("white", "#2c7cc7"))){
#
#   thresholds <- result$thresholds
#   prob_mats <- result$prob_mats
#
#   filename <- file.path(folder, sprintf("prob_%d.png", thresholds[1]))
#   save_heatmap(X=t(prob_mats[[1]]), entry_name="Prob", filename=filename,
#                cmap=cmap, heatmap_ratio=0.98, pad=0.05)
#   for (j in 2:length(thresholds)){
#     filename <- file.path(folder, sprintf("prob_%d_%d.png", thresholds[j-1], thresholds[j]))
#     save_heatmap(X=t(prob_mats[[j]]), entry_name="Prob", filename=filename,
#                  cmap=cmap, heatmap_ratio=0.98, pad=0.05)
#   }
#
# }


# plot the mean expression matrix

save_heatmap(t(nb_mean), entry_name="Count", filename=file.path(folder, "mean_plot_1.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.98, pad=0.05)

save_heatmap(t(nb_mean), entry_name="Count", filename="debug_DFBM/plots/mean_plot_2.png",
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=F)

save_heatmap(t(nb_mean), entry_name="Count", filename="debug_DFBM/plots/mean_plot_3.png",
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_mean), entry_name="Count", filename="debug_DFBM/plots/mean_plot_4.png",
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.98, pad=0.1, legend=T)



# start with the one with negative binomial distribution with size of 10

save_heatmap(t(nb_count_10), entry_name = "Count", filename=file.path(folder, "nb_10", "Count_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_count_10), entry_name = "Count", filename=file.path(folder, "nb_10", "Count_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)

performance_10 <- tune_dfbm(X=nb_count_10, rates=c(0.6, 0.7, 0.8), truth=nb_mean)
saveRDS(performance_10, file="debug_DFBM/performance_10.rds")
plot_10_performance <- plot_error(performance_10)

cutoffs_10 <- cutoff_selection(nb_count_10, rate=0.8, gap=0.002, min_prop=0.002)
denoise_10 <- dfbm(nb_count_10,
                   cutoffs=cutoffs_10$thresholds)
rank_plot_10 <- plot_rank(denoise_10)
plot_binary_mat(count_mat=nb_count_10,
                thresholds=denoise_10$thresholds,
                folder="debug_DFBM/plots/nb_10/binary")
plot_binary_mat(count_mat=nb_count_10,
                thresholds=denoise_10$thresholds,
                folder="debug_DFBM/plots/nb_10/binary_na",
                include_na=T)
plot_prob_mat(result=denoise_10,
              folder="debug_DFBM/plots/nb_10/prob")
save_heatmap(t(denoise_10$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_10", "Denoised_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)
save_heatmap(t(denoise_10$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_10", "Denoised_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)


# size of 5

save_heatmap(t(nb_count_5), entry_name = "Count", filename=file.path(folder, "nb_5", "Count_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_count_5), entry_name = "Count", filename=file.path(folder, "nb_5", "Count_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)


performance_5 <- tune_dfbm(X=nb_count_5, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93), truth=nb_mean)
saveRDS(performance_5, file="debug_DFBM/performance_5.rds")
plot_5_performance <- plot_error(performance_5)
cutoffs_5 <- cutoff_selection(nb_count_5, rate=0.8, gap=0.002, min_prop=0.002)
denoise_5 <- dfbm(nb_count_5,
                   cutoffs=cutoffs_5$thresholds)
rank_plot_5 <- plot_rank(denoise_5)
plot_binary_mat(count_mat=nb_count_5,
                thresholds=denoise_5$thresholds,
                folder="debug_DFBM/plots/nb_5/binary")
plot_binary_mat(count_mat=nb_count_5,
                thresholds=denoise_5$thresholds,
                folder="debug_DFBM/plots/nb_5/binary_na",
                include_na=T)
plot_prob_mat(result=denoise_5,
              folder="debug_DFBM/plots/nb_5/prob")
save_heatmap(t(denoise_5$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_5", "Denoised_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)
save_heatmap(t(denoise_5$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_5", "Denoised_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)



# size of 2

save_heatmap(t(nb_count_2), entry_name = "Count", filename=file.path(folder, "nb_2", "Count_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_count_2), entry_name = "Count", filename=file.path(folder, "nb_2", "Count_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)


performance_2 <- tune_dfbm(X=nb_count_2, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93), truth=nb_mean)

thresholds_plot <- list()
for (j in 1:length(performance_2$thresholds_list)){

  probs <- performance_2$cdf_list[[j]]
  thresholds <- performance_2$thresholds_list[[j]]
  threshold_df <- data.frame(CDF=probs, Threshold=thresholds)
  thresholds_plot[[j]] <- ggplot(threshold_df, aes(x=Threshold, y=CDF)) + geom_point() + geom_line() +
    xlim(0, 250) + xlab("Threshold") + ylab("1-CDF")

}
wrap_plots(thresholds_plot, nrow=2, ncol=3)


saveRDS(performance_2, file="debug_DFBM/performance_2.rds")
plot_2_performance <- plot_error(performance_2)
cutoffs_2 <- cutoff_selection(nb_count_2, rate=0.8, gap=0.002, min_prop=0.002)
denoise_2 <- dfbm(nb_count_2,
                  cutoffs=cutoffs_2$thresholds)
rank_plot_2 <- plot_rank(denoise_2)
plot_binary_mat(count_mat=nb_count_2,
                thresholds=denoise_2$thresholds,
                folder="debug_DFBM/plots/nb_2/binary")
plot_binary_mat(count_mat=nb_count_2,
                thresholds=denoise_2$thresholds,
                folder="debug_DFBM/plots/nb_2/binary_na",
                include_na=T)
plot_prob_mat(result=denoise_2,
              folder="debug_DFBM/plots/nb_2/prob")
save_heatmap(t(denoise_2$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_2", "Denoised_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)
save_heatmap(t(denoise_2$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_2", "Denoised_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)



wrap_plots(rank_plot_10, rank_plot_5, rank_plot_2, rank_plot_1, nrow=4)

# size of 1

save_heatmap(t(nb_count_1), entry_name = "Count", filename=file.path(folder, "nb_1", "Count_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_count_1), entry_name = "Count", filename=file.path(folder, "nb_1", "Count_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)


performance_1 <- tune_dfbm(X=nb_count_1, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93), truth=nb_mean)
saveRDS(performance_1, file="debug_DFBM/performance_1.rds")
plot_1_performance <- plot_error(performance_1)
cutoffs_1 <- cutoff_selection(nb_count_1, rate=0.8, gap=0.002, min_prop=0.002)
denoise_1 <- dfbm(nb_count_1,
                  cutoffs=cutoffs_1$thresholds)
rank_plot_1 <- plot_rank(denoise_1)
plot_binary_mat(count_mat=nb_count_1,
                thresholds=denoise_1$thresholds,
                folder="debug_DFBM/plots/nb_1/binary")
plot_binary_mat(count_mat=nb_count_1,
                thresholds=denoise_1$thresholds,
                folder="debug_DFBM/plots/nb_1/binary_na",
                include_na=T)
plot_prob_mat(result=denoise_1,
              folder="debug_DFBM/plots/nb_1/prob")
save_heatmap(t(denoise_1$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_1", "Denoised_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)
save_heatmap(t(denoise_1$denoised_counts), entry_name = "Count", filename=file.path(folder, "nb_1", "Denoised_1.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)



# size of 05

save_heatmap(t(nb_count_05), entry_name = "Count", filename=file.path(folder, "nb_05", "Count_1.png"),
             rowannot=row_annotation, colannot=col_annotation,
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.95, pad=0.1, legend=T)

save_heatmap(t(nb_count_05), entry_name = "Count", filename=file.path(folder, "nb_05", "Count_2.png"),
             cmap=col_fun_count, width=10, height=12, heatmap_ratio=0.99, pad=0.05, legend=F)


performance_05 <- tune_dfbm(X=nb_count_1, rates=c(0.6, 0.7, 0.8, 0.85, 0.9, 0.93), truth=nb_mean)
saveRDS(performance_05, file="debug_DFBM/performance_05.rds")
plot_05_performance <- plot_error(performance_05)
