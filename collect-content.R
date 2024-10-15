#!/usr/bin/env Rscript

library(yaml)
library(purrr)
library(readr)
library(stringr)


# Parts
source(file.path(this.path::this.dir(), "r-lib/utilities.R"))
source(file.path(this.path::this.dir(), "r-lib/process-slides.R"))
source(file.path(this.path::this.dir(), "r-lib/process-assignments.R"))


# Variables
dir_in_base <- here::here()
dir_out_base <- file.path(getwd(), "c")
yaml_content <- read_yaml("content.yml")
subdirs <- c("00-bilder", "01-daten", "bilder", "daten", "skripte")
copy_exclude_patterns <- c(".*.afdesign", ".*~")


# Clear and make output directory
if (dir.exists(dir_out_base)) unlink(dir_out_base, recursive = TRUE)
dir.create(dir_out_base, recursive = TRUE, showWarnings = FALSE)


# Slides
if (!is.null(yaml_content$slides)) {
    process_slides(yaml_content$slides, dir_in_base, dir_out_base, subdirs)
}


# Assignments
if (!is.null(yaml_content$assignments)) {
    process_assignments(
        yaml_content$assignments, dir_in_base, dir_out_base, subdirs
    )
}
