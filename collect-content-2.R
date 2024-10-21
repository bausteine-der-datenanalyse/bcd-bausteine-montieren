#!/usr/bin/env Rscript

library(yaml)
library(purrr)
library(readr)
library(tools)
library(stringr)

# Read YAML file
y_assembly <- read_yaml("content-2.yml")$`assembly`

target_base_folder <- y_assembly$target$folder

handling <- list(
    name_pattern = y_assembly$target$handling$`name-pattern`,
    folder_name = y_assembly$target$handling$folder,
    file_name = y_assembly$target$handling$file,
    enumerate = y_assembly$target$handling$enumerate
)

tasks <- list(
    copy_files = y_assembly$tasks$`copy-files`,
    copy_folders = y_assembly$tasks$`copy-folders`
)

# Clean up and make target folder
if (dir.exists(target_base_folder)) unlink(target_base_folder, recursive = TRUE)
dir.create(target_base_folder, recursive = TRUE, showWarnings = FALSE)


make_target_name <- function(base, name, sidx) {
    base |>
        str_replace("\\$\\{name\\}", name) |>
        str_replace("\\$\\{idx\\}", sidx)
}

file_copy_safe <- function(from, to) {
    if (file.exists(to)) stop("Target file exists: ", to)
    file.copy(from, to)
}

# Process entries to collect
idx <- 1
for (part in y_assembly$parts) {
    # Basic things
    part_foldername <- part$folder
    sidx <- str_pad(idx, 2, side = "left", pad = "0")

    # Source folder
    source_folder <- here::here(part_foldername)

    if (!dir.exists(source_folder)) stop("Source folder does not exist: ", source_folder)

    # Target folder and file
    basename <- str_match(part_foldername, handling$name_pattern)[2]
    target_foldername <- make_target_name(handling$folder, basename, sidx)
    target_filename <- make_target_name(handling$file, basename, sidx)

    target_folder <- file.path(target_base_folder, target_foldername)
    target_file_base <- file.path(target_folder, target_filename)

    # Create target folder
    if (!dir.exists(target_folder)) dir.create(target_folder, recursive = TRUE)

    # Copy files task
    if (!is.null(tasks$copy_files)) {
        for (p in tasks$copy_files) {
            for (file in list.files(path = source_folder, pattern = tasks$copy_files, full.names = FALSE)) {
                ext <- file_ext(file)
                source_file <- file.path(source_folder, file)
                target_file <- paste(target_file_base, ext, sep = ".")
                file_copy_safe(source_file, target_file)
            }
        }
    }


    idx <- idx + 1
}

# Copy

# Pack
