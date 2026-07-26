#' Fit a linear model with cluster-robust standard errors
#'
#' Fits an ordinary least squares model and returns its coefficient table with
#' standard errors clustered on a specified variable.
#'
#' @param formula A model formula.
#' @param data.df A data frame containing the model variables.
#' @param cluster.var Character scalar naming the clustering variable.
#'
#' @return An object of class `coeftest`.
getClusteredLm <- function(formula, data.df, cluster.var) {
  model <- stats::lm(formula = formula, data = data.df)
  cluster.vec <- data.df[[cluster.var]]
  vcov.mat <- sandwich::vcovCL(model, cluster = cluster.vec)

  lmtest::coeftest(model, vcov. = vcov.mat)
}

#' Format conjoint estimates for plotting
#'
#' Joins output from `cregg::cj()` to the project label table, orders conjoint
#' attributes, and creates plotting indicators and row labels.
#'
#' @param cj.out.df A data frame returned by `cregg::cj()`.
#' @param cj.label.df A label data frame with attribute names and labels.
#' @param treat.vars Character vector giving the desired attribute order.
#'
#' @return A formatted data frame for conjoint plots.
formatCjResults <- function(cj.out.df, cj.label.df, treat.vars) {
  cj.out.df |>
    dplyr::right_join(
      cj.label.df,
      by = c(
        "feature" = "att_vname",
        "level" = "att_vlab_full"
      )
    ) |>
    dplyr::mutate(feature = factor(feature, levels = treat.vars)) |>
    dplyr::arrange(feature, mod_att_num) |>
    dplyr::mutate(
      title = mod_att_num == -1,
      refcat = mod_att_num == 0
    ) |>
    dplyr::mutate(
      rn = factor(dplyr::row_number(), labels = att_vlab)
    ) |>
    tidyr::fill(statistic, outcome, .direction = "updown") |>
    dplyr::mutate(statistic = toupper(statistic))
}

#' Create a conjoint label scale with bold section headings
#'
#' Builds a discrete y-axis scale and renders selected labels in bold.
#'
#' @param label.vec Character vector of axis labels.
#' @param bold.idx Integer vector identifying labels to render in bold.
#'
#' @return A `ggplot2` discrete position scale.
getConjointLabelScale <- function(label.vec, bold.idx) {
  formatted.labels.ls <- lapply(seq_along(label.vec), function(index) {
    if (index %in% bold.idx) {
      bquote(bold(.(label.vec[[index]])))
    } else {
      label.vec[[index]]
    }
  })

  ggplot2::scale_y_discrete(
    breaks = label.vec,
    labels = formatted.labels.ls
  )
}

#' Plot main conjoint estimates
#'
#' Creates a faceted plot of AMCE and marginal-mean estimates, including
#' reference lines and hollow markers for AMCE reference categories.
#'
#' @param results.df Formatted conjoint results from `formatCjResults()`.
#' @param label.vec Character vector of y-axis labels in display order.
#' @param bold.idx Integer vector identifying section-heading labels.
#' @param mm.reference Numeric reference value for marginal means.
#'
#' @return A `ggplot2` object.
plotCjMainResults <- function(
    results.df,
    label.vec,
    bold.idx,
    mm.reference = 0.5
) {
  ggplot2::ggplot(
    results.df,
    ggplot2::aes(
      x = estimate,
      y = forcats::fct_rev(rn),
      xmin = lower,
      xmax = upper,
      colour = feature
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(linewidth = 0.2) +
    ggplot2::facet_grid(
      feature ~ statistic,
      scales = "free",
      space = "free"
    ) +
    ggplot2::geom_vline(
      data = dplyr::filter(results.df, statistic == "AMCE"),
      ggplot2::aes(xintercept = 0),
      colour = "grey40",
      linetype = 2
    ) +
    ggplot2::geom_vline(
      data = dplyr::filter(results.df, statistic == "MM"),
      ggplot2::aes(xintercept = mm.reference),
      colour = "grey40",
      linetype = 2
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(
        results.df,
        statistic == "AMCE",
        refcat
      ),
      shape = 21,
      fill = "white"
    ) +
    ggplot2::scale_color_viridis_d(option = "H") +
    getConjointLabelScale(label.vec = label.vec, bold.idx = bold.idx) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      strip.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.border = ggplot2::element_rect(fill = NA),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Plot subgroup conjoint estimates
#'
#' Creates a faceted marginal-means plot comparing levels of a subgroup
#' variable stored in the `BY` column returned by `cregg::cj()`.
#'
#' @param results.df Formatted conjoint results from `formatCjResults()`.
#' @param legend.title Character scalar used as the colour legend title.
#' @param label.vec Character vector of y-axis labels in display order.
#' @param bold.idx Integer vector identifying section-heading labels.
#' @param mm.reference Numeric reference value for marginal means.
#'
#' @return A `ggplot2` object.
plotCjSubgroupResults <- function(
    results.df,
    legend.title,
    label.vec,
    bold.idx,
    mm.reference = 0.5
) {
  ggplot2::ggplot(
    results.df,
    ggplot2::aes(
      x = estimate,
      y = forcats::fct_rev(rn),
      xmin = lower,
      xmax = upper,
      colour = BY
    )
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::geom_linerange(
      linewidth = 0.2,
      position = ggplot2::position_dodge2(width = 0.4)
    ) +
    ggplot2::geom_vline(
      data = dplyr::filter(results.df, statistic == "MM"),
      ggplot2::aes(xintercept = mm.reference),
      colour = "grey40",
      linetype = 2
    ) +
    ggplot2::scale_color_viridis_d(
      option = "C",
      end = 0.6,
      direction = -1,
      name = legend.title,
      na.translate = FALSE
    ) +
    ggplot2::facet_grid(
      feature ~ statistic,
      scales = "free",
      space = "free"
    ) +
    getConjointLabelScale(label.vec = label.vec, bold.idx = bold.idx) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      strip.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.border = ggplot2::element_rect(fill = NA),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Plot model coefficients and confidence intervals
#'
#' Produces a simple coefficient plot from a tidy model-output data frame.
#'
#' @param coef.df A data frame with `term`, `estimate`, `conf.low`, and
#'   `conf.high` columns.
#' @param x.label Character scalar used as the x-axis label.
#'
#' @return A `ggplot2` object.
plotCoefficients <- function(coef.df, x.label) {
  ggplot2::ggplot(
    coef.df,
    ggplot2::aes(
      x = estimate,
      y = term,
      xmin = conf.low,
      xmax = conf.high
    )
  ) +
    ggplot2::geom_pointrange(linewidth = 0.4) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 2,
      colour = "grey"
    ) +
    ggplot2::scale_x_continuous(name = x.label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Allocate a fixed relative width to a plot legend
#'
#' Extracts the legend from a plot and recombines it with the legend-free plot
#' using an explicitly specified relative width.
#'
#' @param plot.obj A `ggplot2` object containing a legend.
#' @param legend.rel.width Numeric relative width allocated to the legend.
#'
#' @return A combined plot object produced by `cowplot::plot_grid()`.
fixLegendWidth <- function(plot.obj, legend.rel.width) {
  plot.no.legend <- plot.obj +
    ggplot2::theme(legend.position = "none")
  plot.legend <- cowplot::get_legend(plot.obj)

  cowplot::plot_grid(
    plot.no.legend,
    plot.legend,
    rel_widths = c(1, legend.rel.width)
  )
}

#' Compare main-effects and interaction models
#'
#' Fits nested linear models with and without interactions between one conjoint
#' attribute and all other conjoint attributes, then compares them by ANOVA.
#'
#' @param data.df A data frame containing the model variables.
#' @param outcome.var Character scalar naming the outcome variable.
#' @param treat.vars Character vector naming conjoint attributes.
#' @param interaction.var Character scalar naming the focal attribute.
#'
#' @return An ANOVA table comparing the two nested models.
compareInteractionModels <- function(
    data.df,
    outcome.var,
    treat.vars,
    interaction.var
) {
  other.treat.vars <- treat.vars[treat.vars != interaction.var]
  interaction.terms <- paste0(other.treat.vars, ":", interaction.var)

  main.formula <- stats::reformulate(
    termlabels = treat.vars,
    response = outcome.var
  )
  interaction.formula <- stats::reformulate(
    termlabels = c(treat.vars, interaction.terms),
    response = outcome.var
  )

  main.model <- stats::lm(formula = main.formula, data = data.df)
  interaction.model <- stats::lm(
    formula = interaction.formula,
    data = data.df
  )

  stats::anova(main.model, interaction.model)
}

#' Prepare descriptive plot data
#'
#' Converts a survey variable into state-level percentages when it is a factor,
#' or retains respondent-level observations when it is numeric.
#'
#' @param var.df A data frame containing `STATE` and the requested variable.
#' @param var.name Character scalar naming the requested variable.
#'
#' @return A data frame with a `type` column identifying factor or numeric data.
getPlotData <- function(var.df, var.name) {
  var.df <- data.frame(var.df)
  var.sym <- rlang::sym(var.name)

  if (is.factor(var.df[[var.name]]) || is.character(var.df[[var.name]])) {
    plot.df <- var.df |>
      dplyr::count(STATE, !!var.sym, name = "count") |>
      dplyr::group_by(STATE) |>
      dplyr::mutate(
        pct = count / sum(count),
        type = "factor"
      ) |>
      dplyr::ungroup()
  } else {
    plot.df <- var.df |>
      dplyr::select(dplyr::all_of(c("STATE", var.name))) |>
      dplyr::mutate(type = "numeric")
  }

  plot.df
}

#' Plot a categorical survey variable by state
#'
#' Produces a stacked percentage bar chart for a categorical survey variable.
#'
#' @param plot.df Data prepared by `getPlotData()`.
#' @param var.name Character scalar naming the plotted variable.
#' @param color.end Numeric end point for the viridis colour scale.
#' @param viridis.option Character viridis palette option.
#' @param plot.title Character subtitle for the plot.
#' @param brewer.palette Optional ColorBrewer palette name. When supplied, the
#'   Brewer palette replaces the viridis scale.
#' @param legend.reverse Logical indicating whether to reverse legend entries.
#'
#' @return A `ggplot2` object.
getFactorPlot <- function(
    plot.df,
    var.name,
    color.end,
    viridis.option = "G",
    plot.title,
    brewer.palette = NULL,
    legend.reverse = FALSE
) {
  var.sym <- rlang::sym(var.name)

  if (is.null(brewer.palette)) {
    fill.scale <- ggplot2::scale_fill_viridis_d(
      option = viridis.option,
      end = color.end,
      name = "Response:",
      guide = ggplot2::guide_legend(
        nrow = 2,
        reverse = legend.reverse
      )
    )
  } else {
    fill.scale <- ggplot2::scale_fill_brewer(
      palette = brewer.palette,
      name = "Response:",
      guide = ggplot2::guide_legend(
        nrow = 2,
        reverse = legend.reverse
      )
    )
  }

  ggplot2::ggplot(
    plot.df,
    ggplot2::aes(
      y = forcats::fct_rev(STATE),
      x = pct,
      fill = !!var.sym
    )
  ) +
    ggplot2::geom_col(colour = "white") +
    fill.scale +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::labs(subtitle = plot.title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.size = grid::unit(0.4, "cm"),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}

#' Plot a numeric survey variable by state
#'
#' Produces horizontal boxplots for a numeric survey variable by state.
#'
#' @param plot.df Data prepared by `getPlotData()`.
#' @param var.name Character scalar naming the plotted variable.
#' @param viridis.option Character viridis palette option.
#' @param color.end Numeric end point for the viridis colour scale.
#' @param plot.title Character subtitle for the plot.
#'
#' @return A `ggplot2` object.
getNumericPlot <- function(
    plot.df,
    var.name,
    viridis.option = "H",
    color.end = 0.8,
    plot.title
) {
  var.sym <- rlang::sym(var.name)

  ggplot2::ggplot(
    plot.df,
    ggplot2::aes(
      y = forcats::fct_rev(STATE),
      x = !!var.sym,
      fill = STATE
    )
  ) +
    ggplot2::geom_boxplot(alpha = 0.8) +
    ggplot2::scale_fill_viridis_d(
      option = viridis.option,
      direction = -1,
      end = color.end,
      name = "Response:",
      guide = ggplot2::guide_legend(nrow = 2, reverse = TRUE)
    ) +
    ggplot2::labs(subtitle = plot.title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.key.size = grid::unit(0.4, "cm"),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}

#' Retrieve a survey-question label from the codebook
#'
#' Looks up the wrapped question label associated with one variable name.
#'
#' @param codebook.df A codebook data frame containing `Variable` and `Label`.
#' @param var.name Character scalar naming the requested variable.
#'
#' @return A character scalar containing the question label.
getQuestionLabel <- function(codebook.df, var.name) {
  label.vec <- codebook.df$Label[codebook.df$Variable == var.name]

  if (length(label.vec) != 1L) {
    stop(
      "Expected exactly one codebook label for variable '",
      var.name,
      "', but found ",
      length(label.vec),
      "."
    )
  }

  label.vec[[1]]
}

#' Create the appropriate descriptive plot for one variable
#'
#' Selects either a categorical percentage plot or a numeric boxplot based on
#' the `type` indicator created by `getPlotData()`.
#'
#' @param var.name Character scalar naming the plotted variable.
#' @param plot.data.ls Named list of data frames from `getPlotData()`.
#' @param codebook.df A codebook data frame containing question labels.
#' @param color.end Numeric end point for the viridis colour scale.
#' @param brewer.palette Optional ColorBrewer palette name for factor plots.
#' @param legend.reverse Logical indicating whether to reverse legend entries.
#'
#' @return A `ggplot2` object.
getVariablePlot <- function(
    var.name,
    plot.data.ls,
    codebook.df,
    color.end = 0.8,
    brewer.palette = NULL,
    legend.reverse = FALSE
) {
  plot.df <- plot.data.ls[[var.name]]
  plot.type <- unique(plot.df$type)

  if (length(plot.type) != 1L) {
    stop("Plot data must contain exactly one value in the 'type' column.")
  }

  plot.title <- getQuestionLabel(
    codebook.df = codebook.df,
    var.name = var.name
  )

  if (identical(plot.type, "numeric")) {
    getNumericPlot(
      plot.df = plot.df,
      var.name = var.name,
      color.end = color.end,
      plot.title = plot.title
    )
  } else {
    getFactorPlot(
      plot.df = plot.df,
      var.name = var.name,
      color.end = color.end,
      plot.title = plot.title,
      brewer.palette = brewer.palette,
      legend.reverse = legend.reverse
    )
  }
}

#' Get scale bar coordinates for a longitude-latitude map
#'
#' Creates a simple scale bar positioned relative to the map boundaries.
#'
#' @param x.limits Numeric vector of length 2 giving longitude limits.
#' @param y.limits Numeric vector of length 2 giving latitude limits.
#' @param bar.length.km Numeric. Scale-bar length in kilometres.
#' @param x.offset.prop Numeric. Horizontal offset from the right boundary as
#'   a proportion of map width.
#' @param y.offset.prop Numeric. Vertical offset from the bottom boundary as
#'   a proportion of map height.
#'
#' @return A list containing line and label data frames.
getScaleBarData <- function(
  x.limits,
  y.limits,
  bar.length.km = 200,
  x.offset.prop = 0.05,
  y.offset.prop = 0.06
) {
  x.range <- diff(x.limits)
  y.range <- diff(y.limits)
  
  mean.lat <- mean(y.limits)
  km.per.degree.lon <- 111.32 * cos(mean.lat * pi / 180)
  bar.length.deg <- bar.length.km / km.per.degree.lon
  
  x.end <- x.limits[2] - x.offset.prop * x.range
  x.start <- x.end - bar.length.deg
  y.position <- y.limits[1] + y.offset.prop * y.range
  
  tick.height <- 0.012 * y.range
  label.y <- y.position + 0.018 * y.range
  
  line.df <- data.frame(
    x = c(x.start, x.start, x.end),
    y = c(
      y.position,
      y.position - tick.height,
      y.position - tick.height
    ),
    xend = c(x.end, x.start, x.end),
    yend = c(
      y.position,
      y.position + tick.height,
      y.position + tick.height
    )
  )
  
  label.df <- data.frame(
    x = c(x.start, x.end),
    y = c(label.y, label.y),
    label = c("0", paste0(bar.length.km, " km")),
    hjust = c(0, 1)
  )
  
  list(
    line.df = line.df,
    label.df = label.df
  )
}
