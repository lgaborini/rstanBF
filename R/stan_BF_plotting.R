

# Plotting methods -------------------------------

#' Plot posterior distributions of a stanBF object
#'
#' Plot posterior distributions of a stanBF object.
#'
#' @param stanBF a `stanBF` object
#' @param ... other arguments
#' @export
plot_posteriors <- function(stanBF, ...) {
   UseMethod('plot_posteriors')
}


plot_posteriors.default <- function(...) {
   message('Not implemented.')
   invisible(NULL)
}

#' Make boxplots for turn-point posteriors
#'
#' Make boxplots for turn-point posteriors.
#'
#' @param stanBF a supported `stanBF` object
#' @param variable `'theta'` or `'rho'` (normalized theta)
#' @param type type of plot (default: `'boxplots'``)
#' @export
#' @return a ggplot plot
#' @import dplyr tidyr ggplot2
plot_posteriors.stanBF_turn <- function(stanBF, variable=NULL, type='boxplots') {

   if (type != 'boxplots') stop('plot not implemented.')

   default.variables <- c('rho', 'theta')

   if (is.null(variable)) {
      variable <- default.variables[1]
      message(paste0('Missing plotting variable: using ', variable))
   }
   assertthat::assert_that(variable %in% default.variables,
                           msg = paste0('Variable must be in: ', paste0(default.variables, collapse = ', ')))

   n.chains <- length(stanBF$stanfit$H1@stan_args)
   n.iter <- stanBF$stanfit$H1@stan_args[[1]]$iter

   # Make plot-friendly columns
   # convert to plotmath notation
   hyp_to_plotmath <- list()
   hyp_to_plotmath[['Hp']] <- 'H[1]'
   hyp_to_plotmath[['Hd']] <- 'H[2]'

   # Create the Grouping column (boxplot fill, shows which term is evaluated)
   df_samples_plot <- stanBF$df_samples %>%
      dplyr::group_by(.data$Hypothesis) %>%
      tidyr::gather('Variable', 'Value', dplyr::starts_with(paste0(variable, '['))) %>%
      dplyr::mutate(Grouping = paste0(hyp_to_plotmath[.data$Hypothesis], ':~"', tolower(.data$Source), '"'))

   # To suppress CRAN check warnings
   Variable <- Value <- Grouping <- NULL

   ggplot(df_samples_plot) +
      geom_boxplot(aes(x = Variable, y = Value, fill = Grouping) ) +
      labs(
         x = NULL,
         y = label_parse(variable),
         title = bquote(paste(.(stanBF$model_name), ' model: posterior samples for ', .(variable))),
         subtitle = bquote(paste(.(n.chains), ' chains, ', .(n.iter), ' Stan iterations'))
      ) +
      scale_y_continuous(limits = c(0,NA), expand = ggplot2::expansion(mult = c(0, .1))) +
      scale_x_discrete(labels = label_parse) +
      scale_fill_discrete('Grouping during fit', labels = scales::parse_format())
}
