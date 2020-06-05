#' Aggregate OTU data by another taxonomic level.
#'
#' @param df A dataframe containing OTU data, presumed to be normalized, although it could be done for raw count data as well. Defaults to \code{otu_df}.
#' @param sample_col An optional vector, specifying the column in \code{df} which contains Sample names. If none is provided and a Sample_name column does not exist, rownames will be used.
#' @param tax_df Taxonomy table. \code{agg_otus()} assumes that a column called OTU exists in \code{tax_df}. Defaults to \code{otu_good_taxonomy}.
#' @param taxon String which describes the taxonomic level to aggregate by. Default is \code{"Phylum"}.
#' @return An aggregated tibble containing a column of Sample names and columns named according to the levels in \code{taxon}.
#' @export
#' @examples
#' phy_df <- agg_otus(otu_df, "Phylum")

agg_otus <- function(df = otu_df, sample_col = NULL, tax_df = otu_good_taxonomy, taxon = "Phylum") {
  # Handle raw counts dataframe, which may not have Sample name data moved into a column; Handle custom sample name columns
  samples <- "Sample_name"
  if(is.null(sample_col) & !("Sample_name" %in% colnames(df))) { # Presume that Sample names are the rownames
    df <- tibble::rownames_to_column(as.data.frame(df), var = "Sample_name")
  } else if("Sample_name" %in% colnames(df)){
    # Do nothing - Sample_name column already exists
  } else { # Use the provided column name to identify samples
    samples <- sample_col
  }
  # remove any metadata (not included in the eventual aggregation)
  df %>% dplyr::select({{samples}}, contains("Otu")) %>%
    join_tax(tax_df, tax_levels = taxon) %>% # Transform and add taxonomic levels
    dplyr::group_by(.data[[{{samples}}]], .data[[taxon]]) %>% # aggregate otus by level
    dplyr::summarize(Percentage = sum(Percentage)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = .data[[taxon]], value = Percentage)
}
