#' Join a dataframe with a taxonomy table.
#'
#' @param df Dataframe containing an OTU key column (resulting from a prior call to \code{tidyr::gather}) or alternatively numbered Otu columns, in which case \code{tidyr::gather()} will be called to create an OTU key column. Defaults to \code{otu_df}.
#' @param tax Taxonomy table. \code{join_tax()} assumes that a column called OTU exists in \code{tax}. Defaults to \code{otu_good_taxonomy}.
#' @return A dataframe resulting from using \code{dplyr::inner_join} on \code{df} and \code{tax}.
#' @export
#' @examples
#' merged_meta_tax_df <- join_tax(df = otu_df, tax = otu_good_taxonomy)
#' filt_merged_meta_tax_df <- dplyr::filter(otu_df, Treatment == "A") %>%
#'    join_tax()

join_tax <- function(df = otu_df, tax = otu_good_taxonomy) {
  # gather the data if no key column is detected
  if(!any(colnames(df) == "OTU") & any(stringr::str_detect(colnames(df), "Otu\\d+"))) {
    df <- tidyr::gather(df, key = OTU, value = Percentage, -stringr::str_which(colnames(df), "Otu", negate = TRUE), factor_key = TRUE)
  }

  # Retrieve factor levels to reassign them after join
  otu_levels <- levels(df$OTU)
  # coerce to character to prevent factor/character coercion error
  df$OTU <- as.character(df$OTU)
  df <- dplyr::inner_join(df, tax[,-2], by = "OTU")
  # recreate factor w/ appropriate levels
  df$OTU <- factor(df$OTU, levels = otu_levels)

  return(df)
}
