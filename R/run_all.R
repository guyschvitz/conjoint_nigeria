# Run all replication analyses from the repository root.
analysis.script.vec <- file.path("R", c(
  "01_conjoint_analysis_main.R",
  "02_conjoint_analysis_descriptives.R"
))

for (analysis.script in analysis.script.vec) {
  message("Running ", analysis.script, "...")
  sys.source(
    analysis.script,
    envir = new.env(parent = globalenv())
  )
}

dir.create("results", recursive = TRUE, showWarnings = FALSE)
capture.output(
  utils::sessionInfo(),
  file = file.path("results", "session_info.txt")
)
