% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_norm_tax.R
\name{export_norm_tax}
\alias{export_norm_tax}
\title{Write metadata and normalized OTU data (trimmed) to a file. Additionally, transpose and write taxonomy data (trimmed) to a second file.}
\usage{
export_norm_tax(
  norm_df = otu_df,
  tax = otu_good_taxonomy,
  ctrl_vec = NULL,
  norm_file = "sample_table.csv",
  tax_file = "transposed_tax_table.csv",
  digits = NULL
)
}
\arguments{
\item{norm_df}{Dataframe containing normalized OTU data (trimmed).}

\item{tax}{Taxonomy table. Default is otu_good_taxonomy.}

\item{ctrl_vec}{Vector of control types, used to exclude control samples from output to \code{norm_file}. This assumes there is an 'Experiment' column in the data. Alternatively, filter the data beforehand.}

\item{norm_file}{Path for outputting \code{norm_df}.}

\item{tax_file}{Path for outputting \code{tax}.}

\item{digits}{Number of digits to use for rounding normalized OTU data in \code{norm_df}.}
}
\value{
No value is returned.
}
\description{
Write metadata and normalized OTU data (trimmed) to a file. Additionally, transpose and write taxonomy data (trimmed) to a second file.
}
\examples{
export_norm_tax()
export_norm_tax(ctrl_vec = c("AE", "Empty", "IsoCtrl", "Water_Neg", "Zymo_Mock"))
export_norm_tax(filt_df, digits = 2)
}
