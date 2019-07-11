#' Generate a list of paired fastq files from given directory.
#'
#' @param dir_in Directory containing paired fastq files. Files that do not contain "_R1_" or "_R2_" in the filename will be ignored (e.g. config.xml).
#' @param f_out Output file path. A .csv with two columns containing the fastq pairs listed in \code{dir_in} will be written to \code{f_out}.
#' @param mothur Boolean for whether a fastq list is created for mothur processing (TRUE) or SRA upload preparation (FALSE). TRUE by default.
#' @return No value is returned.
#' @export
#' @examples
#' list_paired_fastq(dir_in = "unzipped_fastq_files_mothur")
#' list_paired_fastq(dir_in = "fastq_files_sra", f_out = "exp1_files.csv", mothur = FALSE)

# function to create 2 columns for pairs of reads (also ignores other files, e.g., config.xml)
# used for creating fastq list for mothur (TRUE) or for SRA upload (FALSE)
# Note: these can likely be combined into a single workflow in a future update
list_paired_fastq <- function(dir_in, f_out, mothur = TRUE) {
  file_list <- list.files(dir_in)

  if(mothur) {
    forContigs <- cbind(grep("_R1_", file_list, value = TRUE),
                        grep("_R2_", file_list, value = TRUE))
    rownames(forContigs) <- sub("_L001_R1_001.fastq", "", forContigs[,1])
    write.table(forContigs, "miseq.txt", row.names = TRUE, sep = "\t", quote = FALSE)
  }
  else {
    df <- data.frame(filename1 = grep("_R1_", file_list, value = TRUE),
                     filename2 = grep("_R2_", file_list, value = TRUE))
    write_csv(df, path = f_out)
  }
}
