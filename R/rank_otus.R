#' Generate a ranked vector of OTUs.
#'
#' @param df A dataframe containing OTU data to be used to determine ranking.
#' @param desc Boolean for determining whether the OTUs should be sorted in descending (default) or ascending order.
#' @return A character vector of ranked OTUs.
#' @export
#' @examples
#' rank_otus(df = dplyr::filter(otu_df, Experiment == "Amx"))

rank_otus <- function(df, desc = TRUE) {
  names(sort(colMeans(df[,stringr::str_detect(colnames(df), "Otu")]), decreasing = desc))
}
