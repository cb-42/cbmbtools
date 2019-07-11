#' Reads in a file containing taxonomy data and performs basic processing.
#'
#' @param tax_file A cons.taxonomy file resulting from processing via mothur.
#' @param otu_good An optional matrix containing trimmed OTU count data.
#' @return Returns a dataframe containing taxonomy data.
#' @export
#' @examples
#' load_tax("miseq.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared")
#' load_tax("miseq.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared", otu_good)

load_tax <- function(tax_file, otu_good=NULL) {
  # II. Read in taxonomy data
  # This requires 2 separate calls due to the use of tab and semicolon separators
  tax1 <- read.table(tax_file, sep = "\t", row.names = 1, header = T)
  tax2 <- read.table(tax_file, sep = ";", skip = 1)

  # Note, issues with tibble/data_frame eliminating rownames (an issue if using these to subset later on, such as in factor relabeling)
  # A rework of dependent code could make use of the fact that rownames are included in column1

  otu_taxonomy <- data.frame(OTU = rownames(tax1), Size = tax1[,1], Phylum = tax2[,2], Class = tax2[,3], Order = tax2[,4], Family = tax2[,5], Genus = tax2[,6])  %>%
    purrr::map_df(stringr::str_replace, "\\(.*.\\)", "") %>%  # removes (num) at end of Phylum:Genus names
    as.data.frame() # any operation on a tibble or data_frame will strip the rownames, which we currently use for factor labels
  rownames(otu_taxonomy) <- rownames(tax1) # correct rownames that were stripped out by map_df()

  if(!is.null(otu_good)) {
    # subset to keep only the OTUs over 0.1% of the population
    otu_good_taxonomy <- otu_taxonomy[otu_taxonomy$OTU %in% colnames(otu_good), ]
    rownames(otu_good_taxonomy) <- intersect(rownames(otu_taxonomy), colnames(otu_good))
    # converting to strictly data.frame, due to issues with tibble removing rownames
    otu_good_taxonomy <- as.data.frame(otu_good_taxonomy)
    return(otu_good_taxonomy)
  }
    return(otu_taxonomy)
}
