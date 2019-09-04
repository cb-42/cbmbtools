#' Aggregate OTU data by another taxonomic level.
#'
#' @param df A dataframe containing OTU data, presumed to be normalized, although it could be done for raw count data as well. Defaults to \code{otu_df}.
#' @param tax_df Taxonomy table. \code{agg_otus()} assumes that a column called OTU exists in \code{tax_df}. Defaults to \code{otu_good_taxonomy}.
#' @param taxon String which describes the taxonomic level to aggregate by. Default is \code{"Phylum"}.
#' @return An aggregated tibble containing a column of Sample names and columns named according to the levels in \code{taxon}.
#' @export
#' @examples
#' phy_df <- agg_otus(otu_df, "Phylum")

agg_otus <- function(df = otu_df, tax_df = otu_good_taxonomy, taxon = "Phylum") {
  # handle raw counts dataframe, which may not have Sample name data moved into a column
  if(!("Sample_name" %in% colnames(df))) {
    df <- tibble::rownames_to_column(as.data.frame(df), var = "Sample_name")
  }
  # remove any metadata (not included in the eventual aggregation)
  df %>% dplyr::select(Sample_name, contains("Otu")) %>%
    join_tax(tax_df, tax_levels = taxon) %>% # the 2 lines below are roughly equivalent; remove in future update
    # tidyr::gather(key = OTU, value = Perc, -Sample_name) %>% # note factor_key != TRUE
    # dplyr::inner_join(tax_df[,c("OTU", {{taxon}})], by = "OTU") %>%
    dplyr::group_by(Sample_name, .data[[taxon]]) %>% # aggregate otus by level
    dplyr::summarize(Percentage = sum(Percentage)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = .data[[taxon]], value = Percentage)
}
