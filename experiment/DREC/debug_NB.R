
rm(list=ls())
library(DREC)
library(ComplexHeatmap)
library(circlize)

nb_count_5 <- read.csv("data/nb_count_5.csv", row.names=1) |> as.matrix()
nb_mean <- read.csv("data/nb_true_mean.csv", row.names=1) |> as.matrix()
sample_types <- scan(file="data/sample_types.txt", character(), quote="")
feature_types <- scan(file="data/feature_types.txt", character(), quote="")

# visualization utility functions
color_gray="#808080"
color_red="#db382c"
color_green="#2f7028"
color_brown="#665223"
color_blue="#344885"
color_magenta="#b538b3"
col_fun_count = colorRamp2(c(0, quantile(nb_count_5, 0.96)), c("white", "red"))
col_fun_ecdf = colorRamp2(c(0, 1), c("white", "red"))

nb_count_5 <- read.csv("data/nb_count_5.csv", row.names=1) |> as.matrix()


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


# Open a PNG device
png("debug_drec/heatmap_no_margins.png",
    width=5,
    height=7,
    units="cm",
    res = 700)
# Remove all margins
ht <- Heatmap(t(nb_count_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        show_heatmap_legend = F,
        # top_annotation = col_annotation, # Add column annotation
        # left_annotation = row_annotation, # Add row annotation
        border_gp = gpar(col = "gray", lwd = 0.5),
        # width = unit(4, "cm"),
        # height = unit(6.5, "cm"),
        col=col_fun_count)
# Draw heatmap with minimal margin
draw(ht,newpage = TRUE,
     padding = unit(rep(0, 4), "cm"))
dev.off()



Heatmap(t(nb_count_5), name="Count", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        show_heatmap_legend = F,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        border_gp = gpar(col = "black", lwd = 0.5),
        col=col_fun_count)


mat <- matrix(rnorm(80, 2), 8, 10)

# Open a PNG device
png("heatmap_no_margins.png", width = 800, height = 600, res = 150)

# Remove all margins
par(mar = c(0, 0, 0, 0))

# Draw the heatmap
ht <- Heatmap(mat)
draw(ht, heatmap_legend_side = "bottom")
# Close the device
dev.off()



nb_mean <- read.csv("data/nb_true_mean.csv", row.names=1) |> as.matrix()



ecdf_counts <- count2ecdf(nb_count_5)
Heatmap(t(ecdf_counts), name="ECDF", cluster_rows=FALSE, cluster_columns=FALSE,
        show_column_names = FALSE, show_row_names = FALSE,
        top_annotation = col_annotation, # Add column annotation
        left_annotation = row_annotation, # Add row annotation
        col=col_fun_ecdf)


# ideal situation
mean_quantile_1 <- colMeans(ecdf_counts[1:80, ])
mean_quantile_2 <- colMeans(ecdf_counts[81:140, ])
mean_quantile_3 <- colMeans(ecdf_counts[141:160, ])
ideal_quantiles <- rbind(t(replicate(80, mean_quantile_1)),
                   t(replicate(60, mean_quantile_2)),
                   t(replicate(20, mean_quantile_3)))

ideal_denoised_counts <- ecdf2count(Q_denoised=ideal_quantiles, Q_observed=ecdf_counts,
                                    count_observed=nb_count_5, ncores=4)

mae <- mean(abs(ideal_denoised_counts - nb_mean))

# real situation



