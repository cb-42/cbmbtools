#' Create hellinger transformed data and PCA model from \code{filt_df}. Currently assumes \code{filt_df} is subset and contains no metadata columns.
#'
#' @param filt_df A dataframe that has been subset such that it contains only the observations of interest and has no metadata columns.
#' @return Although no value is returned, \code{filt_df} will be used to create hellinger transformed data with "_hel" appended to \code{filt_df}.
#' Likewise, a PCA model is created and saved to an object with "_pca" appended to \code{filt_df}.
#' @export
#' @examples
#' otu_df_exp1 <- dplyr::filter(otu_df, Experiment == "Exp1")[,-1:-6]
#' make_hel_pca(otu_df_exp1)

make_hel_pca <- function(filt_df) {
  hel_name <- paste0(substitute(filt_df), as.name("_hel"))
  assign(hel_name, vegan::decostand(filt_df, "hellinger"), pos = 1)
  pca_name <- paste0(substitute(filt_df), as.name("_pca"))
  assign(pca_name, vegan::rda(get(hel_name, pos = 1)), pos = 1)
}
