#' Write metadata and normalized OTU data (trimmed) to a file. Additionally, transpose and write taxonomy data (trimmed) to a second file.
#'
#' @param norm_df Dataframe containing normalized OTU data (trimmed).
#' @param tax Taxonomy table. Default is otu_good_taxonomy.
#' @param ctrl_vec Vector of control types, used to exclude control samples from output to \code{norm_file}.
#' @param norm_file Path for outputting \code{norm_df}.
#' @param tax_file Path for outputting \code{tax}.
#' @return No value is returned.
#' @export
#' @examples
#' export_norm_tax()
#' export_norm_tax(ctrl_vec = c("AE", "Empty", "IsoCtrl", "Water_Neg", "Zymo_Mock"))

export_norm_tax <- function(norm_df = otu_df, tax = otu_good_taxonomy, ctrl_vec = NULL, norm_file = "sample_table.csv", tax_file = "transposed_tax_table.csv") {
  if(!is.null(ctrl_vec)) {
    norm_df <- dplyr::filter(norm_df, !(Experiment %in% ctrl_vec))
  }

  readr::write_csv(norm_df, path = norm_file)

  # write.csv() is used to maintain rownames
  write.csv(data.frame(t(tax[-1:-2])), file = tax_file)
}
