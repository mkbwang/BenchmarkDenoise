
rm(list=ls())

library(ComplexHeatmap)
library(circlize)

nb_true_mean <- read.csv("data/nb_true_mean.csv", row.names=1) |> as.matrix()
nb_true_median_05 <- read.csv("data/nb_true_median_05.csv", row.names=1) |> as.matrix()
nb_true_median_1 <- read.csv("data/nb_true_median_1.csv", row.names=1) |> as.matrix()
nb_true_median_2 <- read.csv("data/nb_true_median_2.csv", row.names=1) |> as.matrix()
nb_true_median_5 <- read.csv("data/nb_true_median_5.csv", row.names=1) |> as.matrix()
nb_true_median_10 <- read.csv("data/nb_true_median_10.csv", row.names=1) |> as.matrix()

nb_median_list <- list(nb_true_median_05, nb_true_median_1, nb_true_median_2,
                       nb_true_median_5, nb_true_median_10)


nb_counts_05 <- read.csv("data/nb_count_05.csv", row.names=1) |> as.matrix()
nb_counts_1 <- read.csv("data/nb_count_1.csv", row.names=1) |> as.matrix()
nb_counts_2 <- read.csv("data/nb_count_2.csv", row.names=1) |> as.matrix()
nb_counts_5 <- read.csv("data/nb_count_5.csv", row.names=1) |> as.matrix()
nb_counts_10 <- read.csv("data/nb_count_10.csv", row.names=1) |> as.matrix()

sample_types <- scan(file="data/sample_types.txt", character(), quote="")
feature_types <- scan(file="data/feature_types.txt", character(), quote="")


# visualization utility functions
color_gray="#808080"
color_red="#db382c"
color_green="#2f7028"
color_brown="#665223"
color_blue="#344885"
color_magenta="#b538b3"
col_fun = colorRamp2(c(0, quantile(nb_counts_10, 0.96)), c("white", "red"))

col_annotation <- HeatmapAnnotation(
  Sample = sample_types,
  col = list(Sample = c("Type1" = color_gray,
                        "Type2" = color_red,
                        "Type3"=color_green)),
  show_legend = FALSE,
  show_annotation_name = FALSE
)

row_annotation <- rowAnnotation(
  Feature = feature_types,
  col = list(Feature = c("Group1" = color_brown,
                         "Group2" = color_blue,
                         "Group3"=color_magenta)),
  show_legend = FALSE,
  show_annotation_name = FALSE
)


Heatmap(t(nb_true_mean), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

Heatmap(t(nb_counts_05), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

Heatmap(t(nb_counts_1), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

Heatmap(t(nb_counts_2), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

Heatmap(t(nb_counts_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

Heatmap(t(nb_counts_10), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)



mae_summary <- function(truemean, denoise_list, sizes, subset=NULL, method){
  num_items <- length(denoise_list)
  maes <- rep(0, num_items)
  for (j in 1:num_items){
    if(is.null(subset)){
      maes[j] <- mean(abs(truemean - denoise_list[[j]]))
    } else{
      maes[j] <- mean(abs(truemean[subset, ] - denoise_list[[j]][subset, ]))
    }
  }
  maes_df <- data.frame(MAE=maes, sizes=sizes)
  maes_df$Method <- method
  return(maes_df)
}

mad_summary <- function(median_list, denoise_list, sizes, subset=NULL, method){
  num_items <- length(denoise_list)
  mads <- rep(0, num_items)
  for (j in 1:num_items){
    if(is.null(subset)){
      mads[j] <- median(abs(median_list[[j]] - denoise_list[[j]]))
    } else{
      mads[j] <- median(abs(median_list[[j]][subset, ] - denoise_list[[j]][subset, ]))
    }
  }
  mads_df <- data.frame(MAD=mads, sizes=sizes)
  mads_df$Method <- method
  return(mads_df)
}


# load DFBM data


dfbm_count_05 <- read.csv("denoised/DFBM/nb_denoised_05.csv", row.names=1) |> as.matrix()
dfbm_count_1 <- read.csv("denoised/DFBM/nb_denoised_1.csv", row.names=1) |> as.matrix()
dfbm_count_2 <- read.csv("denoised/DFBM/nb_denoised_2.csv", row.names=1) |> as.matrix()
dfbm_count_5 <- read.csv("denoised/DFBM/nb_denoised_5.csv", row.names=1) |> as.matrix()
dfbm_count_10 <- read.csv("denoised/DFBM/nb_denoised_10.csv", row.names=1) |> as.matrix()
dfbm_denoise_list <- list(dfbm_count_05, dfbm_count_1, dfbm_count_2,
                          dfbm_count_5, dfbm_count_10)

Heatmap(t(dfbm_count_05), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(dfbm_count_1), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(dfbm_count_2), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(dfbm_count_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(dfbm_count_10), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)

mae_dfbm <- mae_summary(nb_true_mean, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="DFBM")
mae_dfbm_1 <- mae_summary(nb_true_mean, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="DFBM")
mae_dfbm_2 <- mae_summary(nb_true_mean, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="DFBM")
mae_dfbm_3 <- mae_summary(nb_true_mean, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="DFBM")

mad_dfbm <- mad_summary(nb_median_list, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="DFBM")
mad_dfbm_1 <- mad_summary(nb_median_list, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="DFBM")
mad_dfbm_2 <- mad_summary(nb_median_list, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="DFBM")
mad_dfbm_3 <- mad_summary(nb_median_list, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="DFBM")


#DREC
drec_count_05 <- read.csv("denoised/DREC/nb_denoised_05.csv", row.names=1) |> as.matrix()
drec_count_1 <- read.csv("denoised/DREC/nb_denoised_1.csv", row.names=1) |> as.matrix()
drec_count_2 <- read.csv("denoised/DREC/nb_denoised_2.csv", row.names=1) |> as.matrix()
drec_count_5 <- read.csv("denoised/DREC/nb_denoised_5.csv", row.names=1) |> as.matrix()
drec_count_10 <- read.csv("denoised/DREC/nb_denoised_10.csv", row.names=1) |> as.matrix()
drec_denoise_list <- list(drec_count_05, drec_count_1, drec_count_2,
                          drec_count_5, drec_count_10)


Heatmap(t(drec_count_05), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(drec_count_1), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(drec_count_2), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(drec_count_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(drec_count_10), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)




mae_drec <- mae_summary(nb_true_mean, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="DREC")
mae_drec_1 <- mae_summary(nb_true_mean, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="DREC")
mae_drec_2 <- mae_summary(nb_true_mean, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="DREC")
mae_drec_3 <- mae_summary(nb_true_mean, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="DREC")

mad_drec <- mad_summary(nb_median_list, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="DREC")
mad_drec_1 <- mad_summary(nb_median_list, dfbm_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="DREC")
mad_drec_2 <- mad_summary(nb_median_list, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="DREC")
mad_drec_3 <- mad_summary(nb_median_list, drec_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="DREC")



magic_count_05 <- read.csv("denoised/MAGIC/nb_denoised_05.csv") |> as.matrix()
magic_count_1 <- read.csv("denoised/MAGIC/nb_denoised_1.csv") |> as.matrix()
magic_count_2 <- read.csv("denoised/MAGIC/nb_denoised_2.csv") |> as.matrix()
magic_count_5 <- read.csv("denoised/MAGIC/nb_denoised_5.csv") |> as.matrix()
magic_count_10 <- read.csv("denoised/MAGIC/nb_denoised_10.csv") |> as.matrix()
magic_denoise_list <- list(magic_count_05, magic_count_1, magic_count_2,
                           magic_count_5, magic_count_10)


mae_magic <- mae_summary(nb_true_mean, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="MAGIC")
mae_magic_1 <- mae_summary(nb_true_mean, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="MAGIC")
mae_magic_2 <- mae_summary(nb_true_mean, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="MAGIC")
mae_magic_3 <- mae_summary(nb_true_mean, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="MAGIC")

mad_magic <- mad_summary(nb_median_list, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10), method="MAGIC")
mad_magic_1 <- mad_summary(nb_median_list, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(1, 80), method="MAGIC")
mad_magic_2 <- mad_summary(nb_median_list, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(81, 140), method="MAGIC")
mad_magic_3 <- mad_summary(nb_median_list, magic_denoise_list, sizes=c(0.5, 1, 2, 5, 10),
                          subset=seq(141, 160), method="MAGIC")


Heatmap(t(magic_count_05), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(magic_count_1), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(magic_count_2), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(magic_count_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


Heatmap(t(magic_count_10), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun)


library(ggplot2)

mad_combined <- rbind(mad_magic, mad_dfbm, mad_drec)
mad_combined$sizes <- as.character(mad_combined$sizes)
mad_combined$sizes <- factor(mad_combined$sizes, levels=c("10", "5", "2", "1", "0.5"))
ggplot(mad_combined, aes(x=sizes, y=MAD, color=Method)) + geom_point(alpha=0.6)+
  xlab("Negative Binomial Size") + ylab("Median Absolute Deviation")


mae_combined <- rbind(mae_magic, mae_dfbm, mae_drec)
mae_combined$sizes <- as.character(mae_combined$sizes)
mae_combined$sizes <- factor(mae_combined$sizes, levels=c("10", "5", "2", "1", "0.5"))
ggplot(mae_combined, aes(x=sizes, y=MAE, color=Method)) + geom_point(alpha=0.6)+
  xlab("Negative Binomial Size") + ylab("Mean Absolute Error")


