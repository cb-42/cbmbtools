#' Create an ordered relative abundance plot
#'
#' @param df Dataframe containing metadata and taxon (Phylum, Otu, etc) abundance data in 'long' format, e.g. using \code{\link[tidyr]{gather}}.
#'     This dataframe is typically the result of processing via \code{\link{taxon_sort_gather}}.
#' @param df_obs An optional dataframe containing individual observations, when \code{df} contains summarized samples. \code{df_obs} will be used to add a points layer with \code{\link[ggplot2]{geom_jitter}}.
#' @param taxon Which taxonomic level within \code{df} (and optionally, \code{df_obs}) should be plotted? OTU by default.
#' @param yvar Variable to use on the y-axis. Defaults to \code{Mean_Perc}.
#' @param title String to use as plot title. Defaults to an empty string.
#' @param error_bar Boolean for whether to include an errorbar. If so, \code{df} should contain a column named SEM with this information.
#' @param facet_var Variable to use for faceted plots. By default, no faceting will be used.
#' @param fill Optional variable to use for fill color. Common use cases include filling by Phylum, Sample_name, or Sample_type.
#' @param phy_vec Optional vector containing the unique Phyla resulting from \code{\link{load_tax}}, likely following trimming. Combined with \code{fill_pal} this allows for a consistent color scheme across RA plots.
#' @param fill_pal An optional user-defined palette for maintaining consistent fill colors across RA plots. Best when combined with \code{phy_vec}.
#' @param gtxt Boolean for whether to include a \code{\link[ggplot2]{geom_text}} layer in the plot which will display actual percentages. Not included by default.
#' @param seed Integer to be used as a random seed to ensure reproducibility. For example, this would come into play if using \code{df_obs} which will create a \code{\link[ggplot2]{geom_jitter}} layer that adds random noise to points.
#' @return A ggplot created based on \code{df} and the specified parameters. It can be further modified with additional layers.
#' @export
#' @examples
#' plot_ra(mock_df, title = "Cross-Comparison of Mock Control Replicates", yvar = "Percentage", facet_var = "Sample",
#'    fill = "Phylum", gtxt = T)
#'
#' plot_ra(samp_df, title = "Exp1 & 2 Stomachs by Treatment, Ordered by Untreated",
#'    facet_var = "Group", fill = "Phylum", error_bar = T)
#'
#' plot_ra(df = otu_agg, df_obs = otu_obs, title = "Feces Samples, Ordered by Day 0",
#'    facet_var = "Day", fill = "Phylum")
#'
#' plot_ra(df = phy_agg, df_obs = phy_obs, taxon = "Phylum",
#'    title = "Feces Samples, Aggregated by Phylum, Ordered by Day 0",
#'    facet_var = "Day", fill = "Phylum", phy_vec = phy_rank$Phylum, fill_pal = phy_pal)

plot_ra <- function(df, df_obs = NULL, taxon = "OTU", yvar = "Mean_Perc", title = "", error_bar = FALSE, facet_var = NULL, fill = NULL, phy_vec = NULL, fill_pal = NULL, gtxt = FALSE, seed = 123) {
  set.seed(seed)

  # account for fill, either by a taxonomy condition, or grey
  if(!is.null(fill)) { # controls all tax cases, and Sample (controls) (or presumably other columns that exist in data)
    p <- ggplot(data = df, aes(y = .data[[yvar]], x = .data[[taxon]])) +
      geom_col(aes(fill = .data[[fill]])) +
      labs(fill = fill)
  } # should fill be set to facet_var in cases where fill (e.g. taxonomy data) is null?
  else { # This is primarily aimed at cases where there is no fill or faceting
    p <- ggplot(data = df, aes(y = .data[[yvar]], x = .data[[taxon]])) +
      geom_col(fill = "grey")
  }

  # handle faceting (always by rows given nature of RA plots)
  if(!is.null(facet_var)) {
    #fv <- rlang::ensym(facet_var)        # remove in future version
    #p <- p + facet_grid(rows = vars(!!fv))
    p <- p + facet_grid(rows = vars(.data[[facet_var]]))
  }

  # add error bar if requested
  if(error_bar) {
    p <- p + geom_errorbar(aes(ymin = Mean_Perc - SEM, ymax = Mean_Perc + SEM, width = 0.3))
  }

  if (!is.null(df_obs)) { # If a dataframe of observations is provided, assume it should be added as a points layer
    p <- p + geom_jitter(data = df_obs, aes(y = Percentage, x = .data[[taxon]]), alpha = 0.3, width = .15)
  }

  # Determine x-axis label
  xlabel <- dplyr::case_when(stringr::str_detect(taxon, "OTU") ~ "OTUs",
                      stringr::str_detect(taxon, "Phylum") ~ "Phyla",
                      stringr::str_detect(taxon, "Class") ~ "Classes",
                      stringr::str_detect(taxon, "Order") ~ "Orders",
                      stringr::str_detect(taxon, "Family") ~ "Families",
                      stringr::str_detect(taxon, "Genus") ~ "Genera")

  p <- p + theme_bw() + # added due to pdf knitting losing global ggtheme
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), legend.position = "top") + # angles axis text; repositions legend
    labs(title = title, x = xlabel, y = "% Relative Abundance")

  # Keep Phylum fill color consistent with other plots
  if(!is.null(phy_vec) & !is.null(fill_pal)) {
    fill_ord <- match(levels(df$Phylum), phy_vec)
    if(taxon!="Phylum") { # This is a temporary solution for x=OTU and needs further optimization
      fill_lev = levels(df$Phylum)[na.omit(match(phy_vec, levels(df$Phylum)))[1:length(levels(df$Phylum))]]
      p <- p + scale_fill_manual(values = fill_pal[fill_ord], breaks = fill_lev, labels = fill_lev)
    } else {
      p <- p + scale_fill_manual(values = fill_pal[fill_ord])
    }
  }

  if(gtxt == TRUE) {
    # this helps show OTUs have a value of 0; related to a knit issue that shows sliver of color when OTUs actually have a true value of 0
    p <- p + geom_text(aes(label = paste0(round(Mean_Perc, 2), "%"), vjust = 0), size = 2)
  }

  # return the plot
  p
}
