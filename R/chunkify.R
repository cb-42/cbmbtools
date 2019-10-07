#' Generate R Markdown code chunks.
#'
#' @param stem Sequence of unchanging characters in the code chunk label.
#' @param vals A vector containing the levels or values to paste together with \code{stem}.
#' @param chunk_opts An optional string containing any code chunk options to include in the code chunk header.
#' @return No value is returned. A sequence of code chunks equal to the length of \code{vals} is output to the console. These can then be pasted into the .Rmd.
#' @export
#' @examples
#' chunkify(stem = "chunk_name", vals = letters[1:6], chunk_opts = "fig.height=12, fig.width=12")

chunkify <- function(stem, vals, chunk_opts = NULL) {
  if (!is.null(chunk_opts)) {
    chunk_names <- paste0("```{r ", stem, "_", vals, ", ", chunk_opts, "}\n\n```\n")
  } else {
    chunk_names <- paste0("```{r ", stem, "_", vals, "}\n\n```\n")
  }
  cat(chunk_names, sep="\n")
}
