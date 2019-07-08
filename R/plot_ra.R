#' Create an ordered relative abundance plot
#'
#' @param df Dataframe containing metadata and OTU abundance data in 'long' format, e.g. using tidyr::gather().
#' @param title String to use as plot title. Defaults to an empty string.
#' @param error_bar Boolean for whether to include an errorbar. If so, \code{df} should contain a column named SEM with this information.
#' @param facet_var Variable to use for faceted plots. By default, no faceting will be used.
#' @param tax_fill Variable to use for fill color based on taxonomy table. By default, fill color is not based on taxonomy.
#' @param gtxt Boolean for whether to include a geom_text() layer in the plot which will display actual percentages. Not included by default.
#' @return A plot created based on \code{df} and the specified parameters.
#' @export
#' @examples
#' plot_ra(mock.df, title = "Cross-Comparison of Mock Control Replicates", facet_var = "Sample", tax_fill = "Phylum", txt = T)
#' plot_ra(samp.df, title = "Exp1 & 2 Stomachs by Treatment, Ordered by Untreated", facet_var = "Group", tax_fill = "Phylum", error_bar = T)

# work in progress; now supports NULL case for tax_fill and as well as NULL/non-NULL faceting
plot_ra <- function(df, title = "", error_bar = FALSE, facet_var = NULL, tax_fill = NULL, gtxt = FALSE) {

  # account for fill, either by a taxonomy condition, or grey
  if(!is.null(tax_fill)) { # controls all tax cases, and Sample (controls) (or presumably other columns that exist in data)
    p <- ggplot(data = data, aes(y = Mean_Perc, x = OTU, fill = .data[[tax_fill]])) +
      geom_col() +
      labs(fill = tax_fill)
  } # should fill be set to facet_var in cases where tax_fill (e.g. taxonomy data) is null?
  else { # This is primarily aimed at cases where there is no tax_fill or faceting
    p <- ggplot(data = data, aes(y = Mean_Perc, x = OTU)) +
      geom_col(fill="grey")
  }

  # handle faceting (always by rows given nature of RA plots)
  if(!is.null(facet_var)) {
    #fv <- rlang::ensym(facet_var)        # remove in future version
    #p <- p + facet_grid(rows = vars(!!fv))
    p <- p + facet_grid(rows = vars(.data[[facet_var]]))
  }

  # add error bar if requested (or if column exists?)
  if(error_bar) {
    p <- p + geom_errorbar(aes(ymin = Mean_Perc - SEM, ymax = Mean_Perc + SEM, width = 0.3))
  }

  p <- p + theme_bw() + # added due to pdf knitting losing global ggtheme
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), legend.position = "top") + # angles axis text; repositions legend
    labs(title = title, x = "OTUs", y = "% Relative Abundance")

  if(txt == TRUE) {
    # this helps show OTUs have a value of 0; related to a knit issue that shows sliver of color when OTUs actually have a true value of 0
    p <- p + geom_text(aes(label = paste0(round(Mean_Perc, 2), "%"), vjust = 0), size = 2)
  }

  # return the plot
  p
}
