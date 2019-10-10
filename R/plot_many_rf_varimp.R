#' Using a list of random forest models, create ordered boxplots of variable importance. The plot title is created dynamically from model attributes. This function is designed to be used in conjunction with \code{\link{many_rf}}.
#'
#' @param rf_list A list containing random forest models.
#' @param tax Taxonomy table. If \code{NULL}, no taxonomic labels will be added.
#' @param n Number of features to display in the resulting boxplot.
#' @return A boxplot based on \code{rf_list} and \code{tax}.
#' @export
#' @examples
#' plot_many_rf_varimp(rf_list = rf_vfd_otu_100, tax = otu_good_taxonomy, n = 40)

plot_many_rf_varimp <- function(rf_list, tax = NULL, n = 50) {
  mod_name <- rlang::ensym(rf_list)

  # extract importance metrics -- use %IncMSE only (gini/node purity can be prone to bias)
  if (rf_list[[1]]$type == "classification") {
    tmp_lst <- purrr::map(rf_list, ~ {.[["importance"]][,3]})
    tmp <- unlist(tmp_lst) %>%
      data.frame(Feature = names(.), MeanDecreaseAccuracy = .)
  } else if (rf_list[[1]]$type == "regression") {
    tmp_lst <- purrr::map(rf_list, ~ {.[["importance"]][,1]})
    tmp <- unlist(tmp_lst) %>%
      data.frame(Feature = names(.), IncMSE = .)
  } else {
    stop("Model type not recognized. Use a classification or regression model.")
  }

  meas <- colnames(tmp)[2]

  # top n features by mean meas
  top_feat <- tmp %>% dplyr::group_by(Feature) %>%
    dplyr::summarize(!!meas := mean(.data[[meas]])) %>%
    dplyr::arrange(desc(.data[[meas]])) %>%
    head(n) %>%
    droplevels()

  # keep only top n features arranged by mean meas
  tmp <- dplyr::filter(tmp, Feature %in% top_feat$Feature) %>%
    droplevels() # remove unused levels

  # reorder levels by mean
  tmp$Feature <- forcats::fct_reorder(tmp$Feature, tmp[,2], .fun = mean, .desc = TRUE)

  # use paste_tax() to relabel Feature levels (designed for use at Genus/OTU level)
  if(!is.null(tax)) {
    # levels(tmp$Feature) <- cbmbtools::paste_tax(levels(tmp$Feature), tax)
    levs <- levels(tmp$Feature)
    levels(tmp$Feature)[stringr::str_which(levs, "Otu")] <- levels(cbmbtools::paste_tax(levs[stringr::str_which(levs, "Otu")], tax))
  }

  ggplot(tmp, aes(x = Feature, y = tmp[,2])) +
    geom_boxplot(outlier.alpha = .4) +
    theme_bw() +
    labs(x = "Feature", y = meas, title = paste0("Feature Importance: ", meas), subtitle = paste0("Model: ", mod_name, "\nRuns: ", length(rf_list), ", mtry: ", rf_list[[1]]$mtry)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(tmp$Feature)))
}
