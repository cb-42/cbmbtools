#' Reads in a file containing OTU count data and keeps OTUs which meet or exceed a specified \% abundance in at least one sample.
#'
#' @param shared An opti_mcc.shared file resulting from processing via mothur.
#' @param otu_vec An optional vector containing OTUs to be excluded, prior to trimming. This could be obtained from extracting a subset of OTUs from the result of \code{\link{load_tax}}.
#' @param thresh Threshold used to determine whether any OTUs should be trimmed from the dataset. If an OTU does not reach \code{thresh} at least once in a sample, the OTU will be removed. Default threshold is 0.1.
#' @return Returns a matrix containing OTU counts, (optionally) without OTUs contained in \code{otu_vec} and also trimmed to remove OTUs that did not cross the specified percentage in at least one sample.
#' @export
#' @examples
#' load_shared("miseq.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared", thresh = .05)

load_shared <- function(shared, otu_vec = NULL, thresh = 0.1) {
  # Read in OTU counts
  otu_raw <- read.table(shared, row.names = 2, header = TRUE)

  # Label and numOtus columns contain the same info for all rows and should be removed
  otu_trim <- otu_raw[, -c(1:2)]

  # Remove any specified OTUs, such as those corresponding to Bacteria_unclassified
  if(!is.null(otu_vec)) {
    otu_trim <- dplyr::select(otu_trim, -otu_vec)
  }

  # Convert trim to matrix in order to work with which() since data.frame will throw an error
  otu_trim <- as.matrix(otu_trim)
  otu_tmp <- vegan::decostand(otu_trim, "total") * 100
  otu_trim[which(otu_tmp < thresh)] <- 0
  otu_good <- otu_trim[, which(colSums(otu_trim) > 0)]
  return(otu_good)
}
