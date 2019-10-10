#' Using a list of random forest models, create ordered boxplots of variable importance. The plot title is created dynamically from model attributes. This function is designed to be used in conjunction with \code{\link{many_rf}}.
#'
#' @param rf_list A list containing the random forest models.
#' @param tax Taxonomy table. If \code{NULL}, no taxonomic labels will be added.
#' @param n Number of features to display in the resulting plot.
#' @return A boxplot based on \code{rf_list} and \code{tax}.
#' @export
#' @examples
#' plot_many_rf_varimp(rf_list = rf_vfd_otu_100, tax = otu_good_taxonomy)

plot_many_rf_varimp <- function(rf_list, tax = NULL, n = 50) {
  mod_name <- rlang::ensym(rf_list)

  # extract importance metrics -- use %IncMSE only (gini/node purity can be prone to bias)
  if (rf_list[[1]]$type == "classification") {
    tmp_lst <- purrr::map(rf_list, ~ {.[["importance"]][,3]})
    tmp <- unlist(tmp_lst) %>%
      data.frame(Taxon = names(.), MeanDecreaseAccuracy = .)
  } else if (rf_list[[1]]$type == "regression") {
    tmp_lst <- purrr::map(rf_list, ~ {.[["importance"]][,1]})
    tmp <- unlist(tmp_lst) %>%
      data.frame(Taxon = names(.), IncMSE = .)
  } else {
    stop("Model type not recognized. Use a classification or regression model.")
  }

  # top n taxa by mean IncMSE
  top_tax <- tmp %>% dplyr::group_by(Taxon) %>% dplyr::summarize(mean_err = mean(tmp[,2])) %>% dplyr::arrange(desc(mean_err)) %>% head(n)
  # keep only top n taxa arranged by mean inc_mse
  tmp <- dplyr::filter(tmp, Taxon %in% top_tax$Taxon) %>% droplevels() # remove unused levels

  # reorder levels by mean
  tmp$Taxon <- forcats::fct_reorder(tmp$Taxon, tmp[,2], .fun = mean, .desc = TRUE)

  # use paste_tax() to relabel Taxon levels (designed for use at Genus/OTU  level)
  if(!is.null(tax)) {
    # levels(tmp$Taxon) <- cbmbtools::paste_tax(levels(tmp$Taxon), tax)
    levs <- levels(tmp$Taxon)
    levels(tmp$Taxon)[stringr::str_which(levs, "Otu")] <- levels(cbmbtools::paste_tax(levs[stringr::str_which(levs, "Otu")], tax))
  }

  meas <- names(tmp)[2]

  ggplot(tmp, aes(x = Taxon, y = tmp[,2])) +
    geom_boxplot(outlier.alpha = .4) +
    theme_bw() +
    labs(x = "Feature", y = meas, title = paste0("Feature Importance: ", meas), subtitle = paste0("Model: ", mod_name, "\nRuns: ", length(rf_list), ", mtry: ", rf_list[[1]]$mtry)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(tmp$Taxon)))
}
