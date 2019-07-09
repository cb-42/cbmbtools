#' Generate a list of paired fastq files from given directory.
#'
#' @param dir_in Directory containing paired fastq files. Files that do not contain "_R1_" or "_R2_" in the filename will be ignored (e.g. config.xml).
#' @param f_out Output file path. A .csv with two columns containing the fastq pairs listed in \code{dir_in} will be written to \code{f_out}.
#' @return No value is returned.
#' @export
#' @examples
#' list_paired_fastq(dir_in = "fastq_files", f_out = "exp1_files.csv")

# function to create 2 columns for the pairs of reads (also ignores other files, e.g., config.xml)
list_paired_fastq <- function(dir_in, f_out) {
  tmp <- list.files(dir_in)
  df <- data.frame(filename1 = grep("_R1_", tmp, value = TRUE),
             filename2 = grep("_R2_", tmp, value = TRUE))
  write_csv(df, path = f_out)
}
