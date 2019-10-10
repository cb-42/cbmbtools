#' Generate a ranked vector of OTUs or other taxonomic level.
#'
#' @param df A dataframe containing OTU data to be used to determine ranking.
#' @param desc Boolean for determining whether the OTUs should be sorted in descending (default) or ascending order.
#' @param tax_level Taxonomic level in \code{tax_df} to be utilized for ranking. The values within \code{tax_level} should also be column names in \code{df}. Defaults to OTU.
#' @param tax_df Taxonomy table. Defaults to \code{otu_good_taxonomy}.
#' @return A ranked character vector based on the mean abundance corresponding to levels in \code{tax_df[, tax_level]}.
#' @export
#' @examples
#' rank_otus(df = dplyr::filter(otu_df, Experiment == "Amx"))
#' rank_otus(phy_filt, tax_level = "Phylum")

rank_tax <- function(df, desc = TRUE, tax_level = "OTU", tax_df = otu_good_taxonomy) {
  if (tax_level == "OTU") {
    names(sort(colMeans(df[, stringr::str_detect(colnames(df), "Otu")]), decreasing = desc))
  } else if (tax_level %in% colnames(tax_df)) {
    names(sort(colMeans(dplyr::select(df, unique(as.character(tax_df[, tax_level])))), decreasing = desc))
  }
  else {
    stop(paste0("Requested taxonomic level does not exist in ", quote(tax_df), "."))
  }
}
