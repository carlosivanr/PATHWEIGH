# #############################################################################
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Render reports in parallel
# This script was designed to render multiple .qmd reports in parallel.

# #############################################################################
library(future)
library(future.apply)
library(quarto)

# Plan for parallelization
plan(multisession, workers = 4) 

# List the Quarto files
qmd_files <- c(
  "D:\\PATHWEIGH\\delivery_20240917\\scripts\\ee_vs_ene\\ee_vs_ene_models\\ee_vs_ene_logistic_regression_model.qmd",
  "D:\\PATHWEIGH\\delivery_20240917\\scripts\\ee_vs_ene\\ee_vs_ene_models\\ee_vs_ene_linear_model_4a.qmd",
  "D:\\PATHWEIGH\\delivery_20240917\\scripts\\ee_vs_ene\\ee_vs_ene_models\\ee_vs_ene_linear_model_5.qmd"
  # "D:\\PATHWEIGH\\delivery_20240917\\scripts\\ee_vs_ene\\ee_vs_ene_models\\ee_vs_ene_linear_model_6.qmd",
  # "D:\\PATHWEIGH\\delivery_20240917\\scripts\\ee_vs_ene\\ee_vs_ene_models\\ee_vs_ene_linear_model_7.qmd"
)


# Could also try this to avoid UNRELIABLE VALUE warnings.
options(future.rng.onMisuse = "ignore")

# Benchmark the time it takes to render
tictoc::tic()

# lambda function to render the reports
future_lapply(qmd_files, function(file) {
  renv::load("D:\\PATHWEIGH")  # Explicitly activate renv
  quarto::quarto_render(file)
}, future.seed = TRUE)

tictoc::toc()
beepr::beep(sound = 2)