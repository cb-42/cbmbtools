#' Paste Genus information onto a vector of OTU numbers.
#'
#' @param otu_vec Vector of OTU numbers, used to select rownames (OTUs) in taxonomy table.
#' @param tax Taxonomy table. Default is otu.good.taxonomy (used historically).
#' @return A factor vector comprised of taxonomy label and OTU number, with levels that match the order of OTUs in \code{otu_vec}.
#' @export
#' @examples
#' paste_tax(rownames(otu.good.taxonomy)[1:20])
#' otu.df$OTU <- paste_tax(otu.df$OTU)

paste_tax <- function(otu_vec, tax = otu.good.taxonomy) {
  forcats::fct_inorder(paste0(tax[otu_vec, ]$Genus, " (", otu_vec, ")"))
}
