#' Reads in a file containing taxonomy data and performs basic processing.
#'
#' @param tax_file A cons.taxonomy file resulting from processing via mothur.
#' @param otu_good An optional matrix containing trimmed OTU count data.
#' @return Returns a dataframe containing taxonomy data, optionally subset by the OTUs within `otu_good`.
#' @export
#' @examples
#' load_tax("miseq_mothur.cons.taxonomy")
#' load_tax("miseq_mothur.cons.taxonomy", otu_good)

load_tax <- function(tax_file, otu_good=NULL) {
  # II. Read in taxonomy data
  # This requires 2 separate calls due to the use of tab and semicolon separators
  tax1 <- read.table(tax_file, sep = "\t", row.names = 1, header = TRUE, colClasses = c("character", "numeric", "character"))
  tax2 <- read.table(tax_file, sep = ";", skip = 1, col.names = c("", "Phylum", "Class", "Order", "Family", "Genus", ""), stringsAsFactors = FALSE) %>%
    dplyr::select(-c(1, 7)) %>%
    purrr::map_df(stringr::str_replace, "\\(.*.\\)", "") %>% # removes (num) at end of Phylum:Genus names, coerces factor columns to chr
    purrr::map_df(as.factor) # recreate factors

  # Note: tibble/data_frame eliminate rownames (an issue if using these to subset later on, such as in factor relabeling)
  # A rework of dependent code could make use of the fact that rownames are included in column1

  otu_taxonomy <- data.frame(OTU = rownames(tax1), Size = tax1[,1], tax2, stringsAsFactors = FALSE) # keeps OTU as chr rather than factor
  rownames(otu_taxonomy) <- rownames(tax1) # insert rownames, for use with vegan functions and plot()

  if(!is.null(otu_good)) {
    # subset to keep only the OTUs over 0.1% of the population
    otu_good_taxonomy <- otu_taxonomy[otu_taxonomy$OTU %in% colnames(otu_good), ] %>%
      droplevels() # Account for factor levels for values that no longer exist.
    rownames(otu_good_taxonomy) <- intersect(rownames(otu_taxonomy), colnames(otu_good))
    return(otu_good_taxonomy)
  }
    return(otu_taxonomy)
}
