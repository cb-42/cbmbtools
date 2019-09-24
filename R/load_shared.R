#' Reads in a file containing OTU count data and keeps only OTUs > 0.1 \% of the population.
#'
#' @param shared An opti_mcc.shared file resulting from processing via mothur.
#' @param thresh Threshold used to determine whether any OTUs should be trimmed from the dataset. If an OTU does not reach \code{thresh} at least once in a sample, the OTU will be removed. Default threshold is 0.1.
#' @return Returns a matrix containing OTU counts, trimmed to remove OTUs < 0.1 \% of the population.
#' @export
#' @examples
#' load_shared("miseq.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared", thresh = .05)

load_shared <- function(shared, thresh = 0.1) {
  # Read in OTU counts
  otu_raw <- read.table(shared, row.names = 2, header = T)

  # Label and numOtus columns contain the same info for all rows and should be removed
  otu_trim <- otu_raw[, -c(1:2)]

  # Convert trim to matrix in order to work with which() since data.frame will throw an error
  otu_trim <- as.matrix(otu_trim)
  otu_tmp <- vegan::decostand(otu_trim, "total") * 100
  otu_trim[which(otu_tmp < thresh)] <- 0
  otu_good <- otu_trim[, which(colSums(otu_trim) > 0)]
  return(otu_good)
}
