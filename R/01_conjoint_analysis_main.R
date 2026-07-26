# Main conjoint analysis ----------------------------------------------------
# Run this script from the repository root.

# Load reusable project functions.
source("R/functions/00_analysis_functions.R")

# Output paths --------------------------------------------------------------
out.dir <- "results"
fig.dir <- file.path(out.dir, "figures")
tab.dir <- file.path(out.dir, "tables")
plot.obj.dir <- file.path(out.dir, "plot_objects")

# Create output directories if they do not already exist.
dir.create(fig.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot.obj.dir, recursive = TRUE, showWarnings = FALSE)

# Load data -----------------------------------------------------------------
# Load the preprocessed conjoint survey data.
sp.main.df <- readRDS("data/conjoint_data_prepped.Rds")

# Load attribute and level labels used in tables and figures.
sp.label.df <- readRDS("data/conjoint_labels_df.Rds")

# Load the survey codebook used to retrieve question wording.
sp.codebook.df <- readRDS("data/conjoint_data_codebook.Rds")

# Exclude interviews flagged as invalid.
sp.main.cj.df <- sp.main.df |>
  dplyr::filter(!grepl("^Invalid", DataValidity)) |>
  droplevels()

# Create one observation per interview for respondent-level analyses.
demo.vars <- paste0("Q", c(6, 7, 9, 10, 15, 13, 17))
sp.main.int.df <- sp.main.cj.df |>
  dplyr::select(
    VIGNETTE,
    CQ1,
    STATE,
    dplyr::all_of(demo.vars),
    INTNR,
    ALLOWRETURN
  ) |>
  dplyr::distinct()

# Main conjoint estimates ---------------------------------------------------
# Define the conjoint attributes and forced-choice outcome formula.
treat.vars <- paste0("CJ_ATTRIBUTE", c(3:1, 4:8))
cj.choice.formula <- stats::reformulate(
  termlabels = treat.vars,
  response = "CJCHOICE"
)

# Estimate average marginal component effects.
cjc.main.am.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  estimate = "amce"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Estimate marginal means.
cjc.main.mm.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Combine AMCE and marginal-mean estimates for plotting.
cjc.main.both.df <- dplyr::bind_rows(
  cjc.main.am.df,
  cjc.main.mm.df
)

# Identify axis labels and attribute headings for the combined figure.
cjc.label.vec <- rev(levels(cjc.main.both.df$rn))
cjc.bold.idx <- which(rev(cjc.main.both.df$title))

# Build the main conjoint-results figure.
cjc.main.plot <- plotCjMainResults(
  results.df = cjc.main.both.df,
  label.vec = cjc.label.vec,
  bold.idx = cjc.bold.idx,
  mm.reference = 0.5
)

# Export the main conjoint-results figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_choice_main_combined_new.png"),
  plot = cjc.main.plot,
  width = 13,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Estimate conjoint effects using the 0-10 rating outcome.
# Define the conjoint attributes and rating-outcome formula.
treat.vars <- paste0("CJ_ATTRIBUTE", c(3:1, 4:8))
cj.rating.formula <- stats::reformulate(
  termlabels = treat.vars,
  response = "CJRATING"
)

# Estimate average marginal component effects for the rating outcome.
cjr.main.am.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.rating.formula,
  id = ~ INTNR,
  estimate = "amce"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Estimate marginal means for the rating outcome.
cjr.main.mm.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.rating.formula,
  id = ~ INTNR,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Combine AMCE and marginal-mean estimates for plotting.
cjr.main.both.df <- dplyr::bind_rows(
  cjr.main.am.df,
  cjr.main.mm.df
)

# Identify axis labels and attribute headings.
cjr.label.vec <- rev(levels(cjr.main.both.df$rn))
cjr.bold.idx <- which(rev(cjr.main.both.df$title))

# Build the rating-outcome robustness figure.
cjr.main.plot <- plotCjMainResults(
  results.df = cjr.main.both.df,
  label.vec = cjr.label.vec,
  bold.idx = cjr.bold.idx,
  mm.reference = 5
)

# Export the rating-outcome robustness figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_ratings_main_combined_new.png"),
  plot = cjr.main.plot,
  width = 13,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)



# Interaction tests ---------------------------------------------------------
# Fit nested models for the four interaction tests.
interaction.model.ls <- list(
  compareInteractionModels(
    data.df = sp.main.cj.df,
    outcome.var = "CJCHOICE",
    treat.vars = sprintf("CJ_ATTRIBUTE%s", 1:8),
    interaction.var = "CJ_ATTRIBUTE2"
  ),
  compareInteractionModels(
    data.df = sp.main.cj.df,
    outcome.var = "CJRATING",
    treat.vars = sprintf("CJ_ATTRIBUTE%s", 1:8),
    interaction.var = "CJ_ATTRIBUTE2"
  ),
  compareInteractionModels(
    data.df = sp.main.cj.df,
    outcome.var = "CJCHOICE",
    treat.vars = sprintf("CJ_ATTRIBUTE%s", 1:8),
    interaction.var = "CJ_ATTRIBUTE3"
  ),
  compareInteractionModels(
    data.df = sp.main.cj.df,
    outcome.var = "CJRATING",
    treat.vars = sprintf("CJ_ATTRIBUTE%s", 1:8),
    interaction.var = "CJ_ATTRIBUTE3"
  )
)

# Define readable labels for the tested outcomes and interactions.
interaction.outcome.vec <- rep(c("Forced Choice", "Rating"), 2)
interaction.term.vec <- rep(
  c(
    "Economic support: BH Victims",
    "Economic support: Community"
  ),
  each = 2
)

# Extract the nested-model test statistics into one table.
interaction.anova.df <- lapply(
  seq_along(interaction.model.ls),
  function(index) {
    anova.df <- interaction.model.ls[[index]]

    data.frame(
      outcome = interaction.outcome.vec[[index]],
      interaction = interaction.term.vec[[index]],
      df = as.character(round(anova.df$Df[[2]])),
      f = round(anova.df$F[[2]], 5),
      p = round(anova.df$`Pr(>F)`[[2]], 5)
    )
  }
) |>
  dplyr::bind_rows()

# Apply publication-ready column names.
names(interaction.anova.df) <- c(
  "Outcome",
  "Interaction Term",
  "DF",
  "F-Statistic",
  "P-Value"
)

# Convert the interaction-test results to a LaTeX table.
interaction.xtable <- xtable::xtable(
  interaction.anova.df,
  caption = paste(
    "Summary of ANOVA results: interaction effects and trade-offs",
    "between reintegration policies"
  ),
  label = "tab:int_anova",
  align = c("l", "l", "l", "r", "r", "r"),
  digits = 3
)

# Export the interaction-test table as LaTeX.
capture.output(
  print(
    interaction.xtable,
    type = "latex",
    comment = FALSE,
    hline.after = c(-1, 0, nrow(interaction.anova.df)),
    include.rownames = FALSE
  ),
  file = file.path(tab.dir, "interaction_anova.tex")
)

# Vignette effects ----------------------------------------------------------
# Estimate post-treatment vignette effects on reintegration support.
vignette.post.formula <- stats::reformulate(
  termlabels = "VIGNETTE",
  response = "CQ1"
)
vignette.post.model <- getClusteredLm(
  formula = vignette.post.formula,
  data.df = sp.main.int.df,
  cluster.var = "INTNR"
)
vignette.post.df <- broom::tidy(
  vignette.post.model,
  conf.int = TRUE
) |>
  dplyr::mutate(dv = "Support (post-treatment)")

# Estimate placebo effects on pre-treatment reintegration support.
vignette.pre.formula <- stats::reformulate(
  termlabels = "VIGNETTE",
  response = "ALLOWRETURN"
)
vignette.pre.model <- getClusteredLm(
  formula = vignette.pre.formula,
  data.df = sp.main.int.df,
  cluster.var = "INTNR"
)
vignette.pre.df <- broom::tidy(
  vignette.pre.model,
  conf.int = TRUE
) |>
  dplyr::mutate(dv = "Baseline support (pre-treatment)")

# Combine and label the post-treatment and placebo coefficients.
vignette.coef.df <- dplyr::bind_rows(
  vignette.post.df,
  vignette.pre.df
) |>
  dplyr::filter(!grepl("Intercept", term)) |>
  tibble::add_row(
    term = "Unknown",
    estimate = 0,
    conf.low = 0,
    conf.high = 0,
    dv = unique(c(vignette.post.df$dv, vignette.pre.df$dv))
  ) |>
  dplyr::mutate(
    term = factor(
      term,
      levels = c(
        "VIGNETTEHigh Risk",
        "VIGNETTELow Risk",
        "Unknown"
      ),
      labels = c(
        "C: High risk",
        "B: Low risk",
        "A: Unknown risk"
      )
    )
  )

# Build the vignette coefficient figure.
vignette.coef.plot <- plotCoefficients(
  coef.df = vignette.coef.df,
  x.label = "Estimated change in support for reintegration"
) +
  ggplot2::facet_wrap(ggplot2::vars(dv)) +
  ggplot2::scale_y_discrete(name = "Vignette") +
  ggplot2::geom_point(
    data = dplyr::filter(
      vignette.coef.df,
      grepl("Unknown", term)
    ),
    ggplot2::aes(x = estimate, y = term),
    shape = 21,
    size = 2.5,
    fill = "white"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey20"),
    strip.text = ggplot2::element_text(
      face = "bold",
      colour = "grey95"
    )
  )

# Export the vignette coefficient figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "bl_support_vignette.png"),
  plot = vignette.coef.plot,
  width = 15,
  height = 6,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Test whether conjoint effects differ across vignette conditions.
vignette.anova.df <- cregg::cj_anova(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ VIGNETTE
)

# Export the vignette subgroup test results.
utils::write.csv(
  as.data.frame(vignette.anova.df),
  file = file.path(tab.dir, "conjoint_anova_vignette.csv"),
  row.names = FALSE
)

# Estimate marginal means by vignette condition.
cjc.vignette.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ VIGNETTE,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Build the vignette subgroup figure.
vignette.plot <- plotCjSubgroupResults(
  results.df = cjc.vignette.df,
  legend.title = "Vignette",
  label.vec = cjc.label.vec,
  bold.idx = cjc.bold.idx
) |>
  fixLegendWidth(legend.rel.width = 0.3)

# Export the vignette subgroup figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_choice_vign_mm.png"),
  plot = vignette.plot,
  width = 16,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# State subgroup ------------------------------------------------------------
# Estimate state differences in post-treatment reintegration support.
state.post.formula <- stats::reformulate(
  termlabels = "STATE",
  response = "CQ1"
)
state.post.model <- getClusteredLm(
  formula = state.post.formula,
  data.df = sp.main.int.df,
  cluster.var = "INTNR"
)
state.post.df <- broom::tidy(
  state.post.model,
  conf.int = TRUE
) |>
  dplyr::mutate(dv = "Support reintegration (Q2)")

# Estimate state differences in pre-treatment reintegration support.
state.pre.formula <- stats::reformulate(
  termlabels = "STATE",
  response = "ALLOWRETURN"
)
state.pre.model <- getClusteredLm(
  formula = state.pre.formula,
  data.df = sp.main.int.df,
  cluster.var = "INTNR"
)
state.pre.df <- broom::tidy(
  state.pre.model,
  conf.int = TRUE
) |>
  dplyr::mutate(dv = "Support reintegration (Q1)")

# Combine the pre-treatment and post-treatment state coefficients.
state.support.df <- dplyr::bind_rows(state.post.df, state.pre.df)

# Export the state-level reintegration-support coefficients.
utils::write.csv(
  state.support.df,
  file = file.path(tab.dir, "state_support_coefficients.csv"),
  row.names = FALSE
)

# Test whether conjoint effects differ between states.
state.anova.df <- cregg::cj_anova(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ STATE
)

# Export the state subgroup test results.
utils::write.csv(
  as.data.frame(state.anova.df),
  file = file.path(tab.dir, "conjoint_anova_state.csv"),
  row.names = FALSE
)

# Estimate marginal means by state.
cjc.state.df <- cregg::cj(
  data = sp.main.cj.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ STATE,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Build the state subgroup figure.
state.plot <- plotCjSubgroupResults(
  results.df = cjc.state.df,
  legend.title = "State",
  label.vec = cjc.label.vec,
  bold.idx = cjc.bold.idx
) |>
  fixLegendWidth(legend.rel.width = 0.3)

# Export the state subgroup figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_choice_state_mm.png"),
  plot = state.plot,
  width = 16,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Victimhood subgroup -------------------------------------------------------
# Recode victimhood responses and remove non-substantive categories.
sp.main.cj.victim.df <- sp.main.cj.df |>
  dplyr::mutate(
    Qvictim = dplyr::case_when(
      as.character(Q17) %in% c("A great deal", "A lot") ~ "A lot",
      as.character(Q17) %in% c("Somewhat", "A little") ~
        "Somewhat / a little",
      as.character(Q17) == "Not at all" ~ "Not at all",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(Qvictim)) |>
  dplyr::mutate(
    Qvictim = factor(
      Qvictim,
      levels = c(
        "A lot",
        "Somewhat / a little",
        "Not at all"
      )
    )
  ) |>
  droplevels()

# Add state controls to the victimhood conjoint specification.
cj.choice.state.formula <- stats::reformulate(
  termlabels = c(treat.vars, "STATE"),
  response = "CJCHOICE"
)

# Test whether conjoint effects differ by victimhood.
victim.anova.df <- cregg::cj_anova(
  data = sp.main.cj.victim.df,
  formula = cj.choice.state.formula,
  id = ~ INTNR,
  by = ~ Qvictim
)

# Export the victimhood subgroup test results.
utils::write.csv(
  as.data.frame(victim.anova.df),
  file = file.path(tab.dir, "conjoint_anova_victimhood.csv"),
  row.names = FALSE
)

# Estimate marginal means by victimhood group.
cjc.victim.df <- cregg::cj(
  data = sp.main.cj.victim.df,
  formula = cj.choice.state.formula,
  id = ~ INTNR,
  by = ~ Qvictim,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  )

# Build the victimhood subgroup figure.
victim.plot <- plotCjSubgroupResults(
  results.df = cjc.victim.df,
  legend.title = "Victimhood",
  label.vec = cjc.label.vec,
  bold.idx = cjc.bold.idx
) |>
  fixLegendWidth(legend.rel.width = 0.3)

# Export the victimhood subgroup figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_choice_victim_mm.png"),
  plot = victim.plot,
  width = 16,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Survey-method subgroup ----------------------------------------------------
# Test whether conjoint effects differ by survey method.
sp.main.cj.method.df <- sp.main.cj.df |>
  dplyr::mutate(method = dplyr::case_when(
    grepl("^Respondent", EQ3) ~ "Independent",
    grepl("^Enumerator", EQ3) ~ "Assisted", 
    TRUE ~ NA),
    method = factor(method, levels = c("Assisted", "Independent"))) |>
  dplyr::filter(!is.na(method))

survey.method.anova.df <- cregg::cj_anova(
  data = sp.main.cj.method.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ method
)

# Export the survey-method subgroup test results.
utils::write.csv(
  as.data.frame(survey.method.anova.df),
  file = file.path(tab.dir, "conjoint_anova_survey_method.csv"),
  row.names = FALSE
)

# Estimate marginal means by survey method.
cjc.survey.method.df <- cregg::cj(
  data = sp.main.cj.method.df,
  formula = cj.choice.formula,
  id = ~ INTNR,
  by = ~ method,
  estimate = "mm"
) |>
  formatCjResults(
    cj.label.df = sp.label.df,
    treat.vars = treat.vars
  ) 

# Build the survey-method subgroup figure.
survey.method.plot <- plotCjSubgroupResults(
  results.df = cjc.survey.method.df,
  legend.title = "Survey method",
  label.vec = cjc.label.vec,
  bold.idx = cjc.bold.idx
) 

# Export the survey-method subgroup figure as PNG.
ggplot2::ggsave(
  filename = file.path(fig.dir, "cj_choice_survey_method_mm.png"),
  plot = survey.method.plot,
  width = 16,
  height = 17,
  units = "cm",
  dpi = 600,
  bg = "white"
)

