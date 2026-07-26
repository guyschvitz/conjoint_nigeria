# Descriptive analysis ------------------------------------------------------
# Run this script from the repository root.

# Load reusable project functions.
source("R/functions/00_analysis_functions.R")

# Output paths --------------------------------------------------------------
# Define the figure output directory.
out.dir <- "results"
fig.dir <- file.path(out.dir, "figures")
plot.obj.dir <- file.path(out.dir, "plot_objects")

# Create the figure directory if it does not already exist.
dir.create(fig.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot.obj.dir, recursive = TRUE, showWarnings = FALSE)

# Load data -----------------------------------------------------------------
# Load the preprocessed conjoint survey data.
sp.main.df <- readRDS("data/conjoint_data_prepped.Rds")
# Load the survey codebook used for figure subtitles.
sp.codebook.df <- readRDS("data/conjoint_data_codebook.Rds")

# Exclude interviews flagged as invalid.
sp.main.cj.df <- sp.main.df |>
  dplyr::filter(!grepl("^Invalid", DataValidity))

# Wrap long question labels for plot subtitles.
sp.codebook.df <- sp.codebook.df |>
  dplyr::mutate(
    Label = stringr::str_wrap(
      Definition,
      width = 60,
      indent = 0,
      exdent = 0
    )
  )

# Demographic variables -----------------------------------------------------
# Define the demographic variables included in the descriptive figures.
demo.vars <- sprintf("Q%s", setdiff(6:13, 7))
# Create one observation per respondent and prepare demographic factors.
sp.demo.df <- sp.main.cj.df |>
  dplyr::select(
    INTNR,
    STATE,
    dplyr::all_of(demo.vars)
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    Q8 = forcats::fct_lump(Q8, prop = 0.05),
    Q9 = forcats::fct_lump(Q9, prop = 0.05),
    Q10 = forcats::fct_lump(Q10, prop = 0.05),
    Q12 = forcats::fct_rev(Q12)
  )

# Prepare plot data for the first set of demographic variables.
demo.vars.first <- demo.vars[1:3]
demo.data.first.ls <- lapply(
  demo.vars.first,
  function(var.name) {
    getPlotData(sp.demo.df, var.name)
  }
)
names(demo.data.first.ls) <- demo.vars.first

# Build the first set of demographic plots.
demo.plot.first.ls <- lapply(
  demo.vars.first,
  function(var.name) {
    getVariablePlot(
      var.name = var.name,
      plot.data.ls = demo.data.first.ls,
      codebook.df = sp.codebook.df
    )
  }
)

# Combine the first demographic plots into one figure.
demo.plot.first <- patchwork::wrap_plots(
  demo.plot.first.ls,
  ncol = 1
)

# Export the first demographic-variable figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "demo_vars1_plot.png"),
  plot = demo.plot.first,
  width = 13,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Prepare plot data for the second set of demographic variables.
demo.vars.second <- demo.vars[4:6]
demo.data.second.ls <- lapply(
  demo.vars.second,
  function(var.name) {
    getPlotData(sp.demo.df, var.name)
  }
)
names(demo.data.second.ls) <- demo.vars.second

# Build the second set of demographic plots.
demo.plot.second.ls <- lapply(
  demo.vars.second,
  function(var.name) {
    getVariablePlot(
      var.name = var.name,
      plot.data.ls = demo.data.second.ls,
      codebook.df = sp.codebook.df
    )
  }
)

# Combine the second demographic plots into one figure.
demo.plot.second <- patchwork::wrap_plots(
  demo.plot.second.ls,
  ncol = 1
)

# Export the second demographic-variable figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "demo_vars2_plot.png"),
  plot = demo.plot.second,
  width = 13,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Education -----------------------------------------------------------------
# Define readable labels for education categories.
education.labels <- c(
  "No education",
  "Incomplete primary",
  "Complete primary",
  "Incomplete secondary (vocational)",
  "Complete secondary (vocational)",
  "Incomplete secondary (university prep)",
  "Complete secondary (university prep)",
  "Incomplete tertiary (university)",
  "Complete tertiary (university)"
)

# Calculate education-category shares by state.
education.df <- sp.demo.df |>
  dplyr::mutate(
    Q13 = factor(
      Q13,
      levels = levels(Q13),
      labels = education.labels
    )
  ) |>
  dplyr::count(STATE, Q13, name = "count") |>
  dplyr::group_by(STATE) |>
  dplyr::mutate(pct = count / sum(count)) |>
  dplyr::ungroup()

# Retrieve and wrap the education question text.
education.subtitle <- getQuestionLabel(
  codebook.df = sp.codebook.df,
  var.name = "Q13"
) |>
  stringr::str_wrap(width = 60)

# Build the education distribution figure.
education.plot <- ggplot2::ggplot(
  education.df,
  ggplot2::aes(
    y = Q13,
    x = pct,
    fill = STATE
  )
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::label_percent()) +
  ggplot2::scale_fill_viridis_d(
    option = "G",
    end = 0.8,
    direction = -1,
    name = "State"
  ) +
  ggplot2::labs(subtitle = education.subtitle) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.key.size = grid::unit(0.4, "cm"),
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_text(size = 10)
  )

# Export the education distribution figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "edu_plot.png"),
  plot = education.plot,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Attitude variables --------------------------------------------------------
# Define the respondent-attitude variables.
attitude.vars <- c("Q16", "Q17", "Q018")
missing.response.label <- "Refused/Don't know"

# Create respondent-level attitude data and consolidate rare responses.
sp.attitude.df <- sp.main.cj.df |>
  dplyr::select(
    INTNR,
    STATE,
    dplyr::all_of(attitude.vars)
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    Q16 = forcats::fct_lump(
      Q16,
      prop = 0.02,
      other_level = missing.response.label
    ),
    Q17 = forcats::fct_lump(
      Q17,
      prop = 0.01,
      other_level = missing.response.label
    ),
    Q018 = forcats::fct_lump(
      Q018,
      prop = 0.01,
      other_level = missing.response.label
    ),
    Q17 = forcats::fct_collapse(
      Q17,
      "A lot" = c("A great deal", "A lot"),
      "Somewhat / a little" = c("Somewhat", "A little"),
      "Not at all" = "Not at all"
    )
  )

# Prepare plotting data for each attitude variable.
attitude.data.ls <- lapply(
  attitude.vars,
  function(var.name) {
    getPlotData(sp.attitude.df, var.name)
  }
)
names(attitude.data.ls) <- attitude.vars

# Build the individual attitude-variable plots.
attitude.plot.ls <- lapply(
  attitude.vars,
  function(var.name) {
    getVariablePlot(
      var.name = var.name,
      plot.data.ls = attitude.data.ls,
      codebook.df = sp.codebook.df,
      brewer.palette = "RdBu",
      legend.reverse = TRUE
    )
  }
)

# Combine the attitude plots into one figure.
attitude.plot <- patchwork::wrap_plots(
  attitude.plot.ls,
  ncol = 1
)

# Export the attitude-variable figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "attitude_var_plot.png"),
  plot = attitude.plot,
  width = 13,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Victimhood responses ------------------------------------------------------
# Extract and wrap the victimhood response labels.
victim.data.df <- attitude.data.ls[["Q17"]]
victim.data.df$Q17 <- forcats::fct_relabel(
  victim.data.df$Q17,
  function(label) {
    stringr::str_wrap(label, width = 10)
  }
)

# Retrieve and wrap the victimhood question text.
victim.subtitle <- getQuestionLabel(
  codebook.df = sp.codebook.df,
  var.name = "Q17"
) |>
  stringr::str_wrap(width = 50)

# Build the victimhood response figure.
victim.plot <- ggplot2::ggplot(
  victim.data.df,
  ggplot2::aes(
    x = Q17,
    y = pct,
    fill = STATE
  )
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1)
  ) +
  ggplot2::scale_fill_viridis_d(
    option = "G",
    end = 0.8,
    direction = -1,
    name = "State"
  ) +
  ggplot2::labs(subtitle = victim.subtitle) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.key.size = grid::unit(0.4, "cm"),
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_text(size = 10)
  )

# Export the victimhood response figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "vict_plot.png"),
  plot = victim.plot,
  width = 10,
  height = 8,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Interviewer assessments --------------------------------------------------
# Define the interviewer-assessment variables.
interviewer.vars <- c("EQ2", "EQ3", "EQ4_1", "EQ4_2")
# Create respondent-level interviewer-assessment data.
sp.interviewer.df <- sp.main.cj.df |>
  dplyr::select(
    INTNR,
    STATE,
    dplyr::all_of(interviewer.vars)
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    EQ2 = forcats::fct_lump(
      EQ2,
      prop = 0.02,
      other_level = "Other"
    )
  )

# Prepare plotting data for each interviewer assessment.
interviewer.data.ls <- lapply(
  interviewer.vars,
  function(var.name) {
    getPlotData(sp.interviewer.df, var.name)
  }
)
names(interviewer.data.ls) <- interviewer.vars

# Build the individual interviewer-assessment plots.
interviewer.plot.ls <- lapply(
  interviewer.vars,
  function(var.name) {
    getVariablePlot(
      var.name = var.name,
      plot.data.ls = interviewer.data.ls,
      codebook.df = sp.codebook.df
    )
  }
)

# Combine the interviewer-assessment plots into one figure.
interviewer.plot <- patchwork::wrap_plots(
  interviewer.plot.ls,
  ncol = 1
)

# Export the interviewer-assessment figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "interview_var_plot1.png"),
  plot = interviewer.plot,
  width = 13,
  height = 20,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Task duration --------------------------------------------------------
task.duration.df <- sp.main.cj.df |>
  dplyr::mutate(task_no = as.numeric(stringr::str_extract(nTASK, "[0-9]{1,}")),
                task_no = factor(task_no, levels = sort(unique(task_no))))

task.duration.plot <- ggplot2::ggplot(task.duration.df, 
                                      ggplot2::aes(x = task_no, y = log1p(CBC1_Duration_Task))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_x_discrete(name = "Task") +
  ggplot2::scale_y_continuous(name = "Duration (log seconds)")

# Export the interview-duration figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "task_duration_log_plot.png"),
  plot = task.duration.plot,
  width = 13,
  height = 7,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Map of study locations -------------------------------------------------------
# Download Nigeria's admin-1 boundaries.
# Define a local directory for downloaded boundary files.
spatial.dir <- file.path("data", "spatial")

dir.create(
  spatial.dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Download Nigeria's admin-1 boundaries.
nigeria.adm1.vect <- geodata::gadm(
  country = "NGA",
  level = 1,
  resolution = 2,
  path = spatial.dir
)

# Convert the spatial boundaries to ggplot-compatible data frames.
nigeria.adm1.sf <- sf::st_as_sf(nigeria.adm1.vect)

# Define the locations of Abuja and Maiduguri.
city.df <- data.frame(
  city = c("Abuja", "Maiduguri"),
  longitude = c(7.3986, 13.1510),
  latitude = c(9.0765, 11.8311),
  label.longitude = c(8.2, 12.2),
  label.latitude = c(8.65, 12.25)
)

# Use Nigeria's bounding box as the displayed map extent.
nigeria.ext <- terra::ext(nigeria.adm1.sf)

x.limits <- c(nigeria.ext[1], nigeria.ext[2])
y.limits <- c(nigeria.ext[3], nigeria.ext[4])

# Create scale-bar coordinates.
scale.bar.ls <- getScaleBarData(
  x.limits = x.limits,
  y.limits = y.limits,
  bar.length.km = 300,
  x.offset.prop = 0.05,
  y.offset.prop = 0.055
)

# Create the Nigeria admin-1 map.
nigeria.map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = nigeria.adm1.sf,
    fill = "grey97",
    colour = "grey45",
    linewidth = 0.3
  ) +
  ggplot2::geom_point(
    data = city.df,
    ggplot2::aes(
      x = longitude,
      y = latitude
    ),
    shape = 21,
    size = 2.8,
    stroke = 0.5,
    fill = "black",
    colour = "white",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_label(
    data = city.df,
    ggplot2::aes(
      x = label.longitude,
      y = label.latitude,
      label = city
    ),
    size = 3.5,
    fill = "white",
    label.size = 0.2,
    label.padding = grid::unit(0.15, "lines"),
    inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    data = scale.bar.ls$line.df,
    ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    linewidth = 0.45,
    colour = "black",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = scale.bar.ls$label.df,
    ggplot2::aes(
      x = x,
      y = y,
      label = label,
      hjust = hjust
    ),
    size = 3,
    vjust = 0,
    inherit.aes = FALSE
  ) +
  ggplot2::coord_sf(
    xlim = x.limits,
    ylim = y.limits,
    expand = FALSE
  ) +
  ggplot2::labs(
    title = "Study locations in Nigeria"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = ggplot2::margin(b = 6)
    ),
    plot.margin = ggplot2::margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  )

# Export the map as a high-resolution PNG.
ggplot2::ggsave(
  filename = file.path(
    "results",
    "figures",
    "nigeria_map.png"
  ),
  plot = nigeria.map,
  width = 6,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

