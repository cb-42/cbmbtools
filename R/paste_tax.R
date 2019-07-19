#' Paste Genus information onto a vector of OTU numbers.
#'
#' @param otu_vec Vector of OTU identifiers, used to select rownames (OTU IDs) in \code{tax}.
#' @param tax Taxonomy table, otu_good_taxonomy by default.
#' @return A factor vector comprised of taxonomy label and OTU identifier, with levels that match the order of OTUs in \code{otu_vec}.
#' @export
#' @examples
#' paste_tax(rownames(otu_good_taxonomy)[1:20])
#' otu_df$OTU <- paste_tax(otu_df$OTU)

paste_tax <- function(otu_vec, tax = otu_good_taxonomy) {
  forcats::fct_inorder(paste0(tax[otu_vec, ]$Genus, " (", otu_vec, ")"))
}
