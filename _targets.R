library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(tidyverse))

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

tar_option_set(
  packages = c("tibble"),
  format = "rds",
  workspace_on_error = TRUE
)

# Convert relative paths to absolute paths
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Load functions for the pipeline
source("R/tar_slides.R")
source("R/tar_projects.R")
source("R/tar_data.R")

# THE MAIN PIPELINE ----
list(
  ## xaringan slides ----
  tar_files(xaringan_files, list.files(here_rel("slides"),
                                       pattern = "\\.Rmd",
                                       full.names = TRUE)),
  tar_target(xaringan_slides,
             render_xaringan(xaringan_files),
             pattern = map(xaringan_files),
             format = "file"),

  ## Convert slides to PDF ----
  tar_files(xaringan_html_files, {
    xaringan_slides
    list.files(here_rel("slides"),
               pattern = "\\.html",
               full.names = TRUE)
  }),
  tar_target(xaringan_pdfs,
             xaringan_to_pdf(xaringan_html_files),
             pattern = map(xaringan_html_files),
             format = "file"),

  ## Project folders ----
  tar_force(project_paths,
            list.dirs(here_rel("projects"),
                      full.names = FALSE, recursive = FALSE),
            force = TRUE),
  tar_target(project_files, project_paths, pattern = map(project_paths)),
  tar_target(project_zips, {
    zippy(project_files, "projects")
  },
  pattern = map(project_files),
  format = "file"),

  ## Build site ----
  tar_quarto(site, path = ".")
)
