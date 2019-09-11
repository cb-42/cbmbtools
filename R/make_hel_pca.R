#' Create hellinger transformed data and PCA model from \code{filt_df}. Assumes \code{filt_df} is subset to desired specifications, but \code{make_hel_pca()} will safely handle metadata columns.
#'
#' @param filt_df A dataframe that has been subset such that it contains only the observations of interest. This function will safely handle metadata columns, excluding these in the call to \code{\link[vegan]{decostand}}.
#' @return Although no value is returned, \code{filt_df} will be used to create hellinger transformed data with "_hel" appended to \code{filt_df}.
#' Likewise, a PCA model is created and saved to an object with "_pca" appended to \code{filt_df}.
#' @export
#' @examples
#' otu_df_exp1 <- dplyr::filter(otu_df, Experiment == "Exp1")
#' make_hel_pca(otu_df_exp1)
#' otu_df_exps <- dplyr::filter(otu_df, Experiment %in% c("Exp1", "Exp2"))
#' make_hel_pca(otu_df_exps)

make_hel_pca <- function(filt_df) {
  hel_name <- paste0(substitute(filt_df), as.name("_hel"))
  assign(hel_name, vegan::decostand(dplyr::select(filt_df, contains("Otu")), "hellinger"), pos = 1)
  pca_name <- paste0(substitute(filt_df), as.name("_pca"))
  assign(pca_name, vegan::rda(get(hel_name, pos = 1)), pos = 1)
}
