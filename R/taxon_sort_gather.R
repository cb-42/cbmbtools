#' Transform data from wide to long, join a taxonomic level, aggregate and create summary statistics, and return n OTUs.
#'
#' @param df Dataframe containing metadata and taxon (Phylum, Otu, etc) abundance data in wide format.
#' @param n Number of OTUs to return, 50 by default. Fewer results will be returned if there are not enough entities with non-zero abundances.
#' @param facet_var Variable to be used for grouping and eventual faceting, possibly in \code{plot_ra}.
#' @param ord_val Level with the \code{facet_var} to be used for ranking OTUs.
#' @param tax_df Taxonomy table. Passed to \code{\link{join_tax}}. Defaults to \code{otu_good_taxonomy}.
#' @param tax_level Taxonomic level to join from \code{tax_df}, the levels within which will be used for grouping. Defaults to Phylum.
#' @return A dataframe that has been aggregated and reshaped into long form.
#' @export
#' @examples
#' filt_ord_facet_df <- dplyr::filter(otu_good_14d, Day == 0, Organ == "Feces") %>%
#'   taxon_sort_gather(facet_var = "Treatment_group", ord_val = "untreated", tax_level = "Phylum")

taxon_sort_gather <- function(df, n = 50, facet_var = NULL, ord_val = NULL, tax_df = otu_good_taxonomy, tax_level = "Phylum") {
  # stderr helper - should fail when NAs are present
  tsg_sem <- function(x) {
    sqrt(var(x)/length(x))
  }

  if(!is.null(facet_var)) {
    fv <- rlang::ensym(facet_var)
  }
  if(!is.null(tax_level)) {
    tl <- rlang::ensym(tax_level)
  }

  ## Check for faceting, and ordering conditions
  if(!is.null(facet_var)) {
    if(!is.null(ord_val)) { # OTUs should be ordered according to rank within specified facet level
      ord_df <- dplyr::filter(df, !!fv == ord_val)
      otu_order <- rank_tax(ord_df)
      df <- data.frame(dplyr::select_if(df, !(stringr::str_detect(colnames(otu_df), "Otu"))), df[,otu_order[1:n]]) %>%
        join_tax(tax_levels = tax_level, tax_df = tax_df) %>%
        dplyr::group_by(OTU, !!tl, !!fv)
    } else { # Facets, ordered by overall mean
      otu_order <- rank_tax(df)
      df <- data.frame(dplyr::select_if(df, !(stringr::str_detect(colnames(otu_df), "Otu"))), df[,otu_order[1:n]]) %>%
        join_tax(tax_levels = tax_level, tax_df = tax_df) %>%
        dplyr::group_by(OTU, !!tl, !!fv)
    }
  } else { # No faceting, ordered by overall mean
    otu_order <- rank_tax(df)
    df <- data.frame(dplyr::select_if(df, !(stringr::str_detect(colnames(otu_df), "Otu"))), df[,otu_order[1:n]]) %>%
      join_tax(tax_levels = tax_level, tax_df = tax_df) %>%
      dplyr::group_by(OTU, !!tl)
    # df <- dplyr::top_n(df, n, Mean_Perc)
  }

  # Summary statistics
  df <- dplyr::summarize(df, Mean_Perc = mean(Percentage), SEM = tsg_sem(Percentage)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Mean_Perc > 0) %>%
    droplevels() %>% # remove factor levels corresponding to entities that have been removed
    dplyr::arrange(desc(Mean_Perc))

  # label OTU
  levels(df$OTU) <- paste_tax(levels(df$OTU), tax_df)

  return(df)
}
