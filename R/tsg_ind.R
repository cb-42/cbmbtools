#' Transform data from wide to long, without aggregating individual samples.
#'
#' @param df Dataframe containing metadata and taxon (Phylum, Otu, etc) abundance data in wide format.
#' @param n Number of OTUs to return, 50 by default.
#' @param ar_mean Boolean to indicate whether the arithmetic mean should be appended to the data.
#' @param geo_mean Boolean to indicate whether the geometric mean should be appended to the data.
#' @param otu_vec Character or factor vector of OTU identifiers. These are OTUs resulting from a dataframe output by \code{\link{taxon_sort_gather}}.
#' @return An unaggregated dataframe that has been reshaped into long form.
#' @seealso \code{\link{taxon_sort_gather}}
#' @export
#' @examples
#' mock_gg <- dplyr::filter(otu_df, Experiment == "Zymo_Mock") %>%
#'     tsg_ind(n = 20, ar_mean = T, geo_mean = T)
#'
#' filt_df_ind <- dplyr::filter(otu_df, Experiment %in% c("A", "B"), Treatment_group == "TG1", Organ == "Feces", Day == 11) %>%
#'     tsg_ind()

# work in progress
tsg_ind <- function(df, n = 50, ar_mean = FALSE, geo_mean = FALSE, otu_vec = NULL) {
  # Geometric mean helper: https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  tsg_geo <- function(x, na.rm = TRUE) {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }

  otu_order <- rank_tax(df)

  if (ar_mean | geo_mean) {
    sample_names <- df$Sample_name
    temp <- dplyr::select(df, otu_order[1:n])

    if (ar_mean) {
      temp <- rbind(temp, apply(dplyr::select(df, otu_order[1:n]), 2, mean))
      sample_names <- append(sample_names, "Arith_Mean")
    }

    if (geo_mean) {
      temp <- rbind(temp, Geo_Mean = apply(dplyr::select(df, otu_order[1:n]), 2, tsg_geo))
      temp[temp==1] <- 0
      sample_names <- append(sample_names, "Geo_Mean")
    }
    # temp <- temp %>% tibble::rownames_to_column("Sample")

    # if (ar_mean & geo_mean) { # this will enable structuring the dataframe so that the means plot on top
    #   sample_names <- forcats::fct_relevel(sample_names, sample_names[c(length(sample_names) - 1, length(sample_names))])
    # } else {
    #   sample_names <- forcats::fct_relevel(sample_names, sample_names[length(sample_names)])
    # }

    temp <- temp[colMeans(temp) > 0] # remove 0-valued OTUs - could be determined by limiting rank_tax to colMeans() > 0
    df <- cbind(Sample_name = forcats::fct_inorder(sample_names), temp) # maintains rows in the original order, with means appended to end
  } else { # non-aggregation case
    if(!is.null(otu_vec)) {
      if(class(otu_vec)=="factor") {
        otu_vec <- levels(otu_vec)
      } # otherwise a character vector is expected
      df <- dplyr::select(df, stringr::str_which(colnames(df), "Otu", negate = TRUE), stringr::str_extract(otu_vec, "Otu\\d+")) # Retains OTUs in order from otu_vec
    } else { # retain all OTUs, ordered by in-data means
      df <- dplyr::select(df, stringr::str_which(colnames(df), "Otu", negate = TRUE), otu_order[1:n]) # Currently does not trim 0-valued OTUs
    }
  }

  # reformats the otu data from wide to long, must include factor_key for factor relabeling
  df <- tidyr::gather(df, key = OTU, value = Percentage, -stringr::str_which(colnames(df), "Otu", negate = TRUE), factor_key = TRUE)
  # df_gg <- dplyr::filter(df_gg, OTU %in% dplyr::filter(df_gg, Sample_name == "Arith_Mean", Percentage > 0)$OTU) # this trim should occur before gathering
  levels(df$OTU) <- paste_tax(levels(df$OTU))

  return(df)
}
