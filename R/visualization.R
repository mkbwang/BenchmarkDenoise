


#' Plot heatmap
#'
#' @param X input matrix
#' @param entry_name title of the colorbar
#' @param filename file name to store the heatmap
#' @param rowannot optional row annotation
#' @param colannot optional column annotation
#' @param cmap a ColorRamp object specifying the color bar
#' @param na_col color representing NA entries
#' @param border_col border color of the heatmap
#' @param width width of the heatmap, default 10cm
#' @param height height of the heatmap, default 12cm
#' @param heatmap_ratio proportion of canvas devoted to the heatmap, by default 0.95
#' @param pad padding when saving the figure, by default 0.1 cm
#' @param legend whether to show the colorbar or not
#' @importFrom grDevices png dev.off
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom grid gpar unit
#'
#' @export
save_heatmap <- function(X, entry_name, filename,  rowannot=NULL, colannot=NULL,
                         cmap, na_col="#555555", border_col="black",
                         width=10, height=12, heatmap_ratio=0.95, pad=0.1,
                         legend=F){

  png(filename,
      width=width,
      height=height,
      units="cm",
      res=700)


  ht <- Heatmap(X, name=entry_name, cluster_rows=FALSE, cluster_columns=FALSE,
                show_column_names = FALSE, show_row_names = FALSE,
                show_heatmap_legend = legend,
                top_annotation = colannot, # Add column annotation
                left_annotation = rowannot, # Add row annotation
                border_gp = gpar(col=border_col, lwd=0.5),
                width = unit(heatmap_ratio, "npc"),
                height = unit(heatmap_ratio, "npc"),
                na_col = na_col,
                col = cmap)
  # Draw heatmap with minimal margin
  draw(ht,newpage = TRUE,
       padding = unit(rep(pad, 4), "cm"))
  dev.off()

}
