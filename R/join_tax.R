#' Join a dataframe with a taxonomy table.
#'
#' @param df Dataframe containing an OTU key column (resulting from a prior call to \code{tidyr::gather}) or alternatively numbered Otu columns, in which case \code{tidyr::gather()} will be called to create an OTU key column. Defaults to \code{otu_df}.
#' @param tax_df Taxonomy table. \code{join_tax()} assumes that a column called OTU exists in \code{tax_df}. Defaults to \code{otu_good_taxonomy}.
#' @param tax_levels Vector specifying which taxonomic levels to join from \code{tax_df}.
#' @return A dataframe resulting from using \code{dplyr::inner_join} on \code{df} and \code{tax_df}.
#' @export
#' @examples
#' merged_meta_tax_df <- join_tax(df = otu_df, tax_df = otu_good_taxonomy)
#'
#' filt_merged_meta_tax_df <- dplyr::filter(otu_df, Treatment == "A") %>%
#'    join_tax()
#'
#' merged_meta_phy_fam_df <- join_tax(otu_df, otu_good_taxonomy, tax_levels = c("Phylum", "Family"))

join_tax <- function(df = otu_df, tax_df = otu_good_taxonomy, tax_levels = NULL) {
  # gather the data if no key column is detected
  if(!any(colnames(df) == "OTU") & any(stringr::str_detect(colnames(df), "Otu\\d+"))) {
    df <- tidyr::gather(df, key = OTU, value = Percentage, -stringr::str_which(colnames(df), "Otu", negate = TRUE), factor_key = TRUE)
  }

  # Retrieve factor levels to reassign them after join
  otu_levels <- levels(df$OTU)
  # coerce to character to prevent factor/character coercion error
  df$OTU <- as.character(df$OTU)

  if(!is.null(tax_levels)) {
    df <- dplyr::inner_join(df, dplyr::select(tax_df, OTU, tax_levels), by = "OTU")
  } else {
    df <- dplyr::inner_join(df, tax_df[,-2], by = "OTU")
  }

  # recreate factor w/ appropriate levels
  df$OTU <- factor(df$OTU, levels = otu_levels)

  return(df)
}
