#' Create an ordination with spiders based on \code{spider_vec} and coloring determined by \code{col_vec}.
#'
#' @param pca_rda_obj Model object resulting from a call to \code{\link[vegan]{rda}}. The proportion of variance explained by PC1 and PC2 is extracted with \code{\link{summary}}.
#' @param spider_vec Vector of metadata for determining spiders.
#' @param col_vec Vector of metadata for determining how points should be colored.
#' @param col_pal Palette to use for the plot's color scheme. Note that it should be at least as long as the number of unique values in \code{col_vec}.
#' @param mtitle The main title of the plot, defaults to an empty string.
#' @param col_leg_pos Position to place the color legend, defaults to the bottom right corner. This should be adjusted if it overlaps any points.
#' @param col_leg_title Title to be used for the color legend, defaults to an empty string, which results in no legend being shown.
#' @param sig_leg_pos Position to place the significance legend, defaults to the top right corner. This should be adjusted if it overlaps any points.
#' @param sig_leg Text describing the result of significance test, defaults to an empty string, which results in no legend being shown.
#' @return A plot is displayed based on \code{pca_rda_obj} and the specified parameters.
#' @export
#' @examples
#' plot_spider(pca_rda_obj = otu_exp1_pca, spider_vec = otu_exp1$Treatment_group,
#'     col_vec = otu_exp1$Organ, col_pal = my_pal,
#'     mtitle = "PCA of Experiment1 Samples by Treatment Group and Organ",
#'     col_leg_pos = "bottomright", col_leg_title = "Organ", sig_leg_pos = "topleft",
#'     sig_leg = "Difference between Treatment Groups:\n permanova: p < 0.0001")

# work in progress; designed to automate generic combinations for ordinations
plot_spider <- function(pca_rda_obj, spider_vec, col_vec, col_pal, mtitle = "", col_leg_pos = "bottomright", col_leg_title = "", sig_leg_pos = "topright", sig_leg = "") {
  # retrieve proportion explained for PC1 and PC2, to be used for x_lab and y_lab
  pca_summary <- summary(pca_rda_obj)
  pc1 <- pca_summary$cont$importance[2]
  pc2 <- pca_summary$cont$importance[5]
  # format needed to preserve .0 in some cases
  x_lab <- paste0("PC1 (", format(round(pc1 * 100, digits = 1), nsmall = 1), "% Explained)")
  y_lab <- paste0("PC2 (", format(round(pc2 * 100, digits = 1), nsmall = 1), "% Explained)")

  # create ordination layers using specified parameters
  plot(pca_rda_obj, type="n", font=2, font.lab = 2, xlab = x_lab, ylab = y_lab, main = mtitle, display = "sites")
  points(pca_rda_obj, pch = 19, col = col_pal[as.numeric(col_vec)])
  vegan::ordispider(pca_rda_obj, spider_vec, label = TRUE)
  if(col_leg_title == "") { # Allow for displaying color legend without an empty line when no title is provided.
    legend(col_leg_pos, levels(as.factor(col_vec)), pch = 19, col = col_pal, title = NULL)
  } else {
    legend(col_leg_pos, levels(as.factor(col_vec)), pch = 19, col = col_pal, title = col_leg_title)
  }
  if(sig_leg != "") { # Only display significance legend when legend information is provided.
    legend(sig_leg_pos, legend = sig_leg)
  }
}
