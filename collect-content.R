#!/usr/bin/env Rscript

# library(zip)
# library(here)
library(yaml)
library(purrr)
library(readr)
library(tools)
library(stringr)


# -------------------------------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------------------------------

test_include <- function(path, exclude_patterns) {
    for (p in exclude_patterns) {
        if (str_detect(path, p)) {
            return(FALSE)
        }
    }
    file_test("-f", path)
}

make_name <- function(base, part_folder, name, idx) {
    sidx <- str_pad(idx, 2, side = "left", pad = "0")
    base |>
        str_replace_all("\\$\\{target\\-folder\\}", target_folder) |>
        str_replace_all("\\$\\{deploy\\-folder\\}", deploy_folder) |>
        str_replace_all("\\$\\{part\\-folder\\}", part_folder) |>
        str_replace_all("\\$\\{name\\}", name) |>
        str_replace_all("\\$\\{idx\\}", sidx)
}

prepare_copy_to <- function(to, overwrite) {
    do_overwrite <- !is.null(overwrite) && overwrite
    if (file.exists(to) && !do_overwrite) stop("File exists: ", to)
    folder <- dirname(to)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
}

file_copy_safe <- function(from, to, overwrite) {
    prepare_copy_to(to, overwrite)
    file.copy(from, to)
}

copy_job <- function(from, to, exclude_patterns, overwrite) {
    if (file_test("-f", from)) { # Copy one file
        file_copy_safe(from, to, overwrite)
    } else if (file_test("-d", from)) { # Copy folder
        for (f in list.files(from)) {
            from_path <- file.path(from, f)
            if (test_include(from_path, exclude_patterns)) file_copy_safe(from_path, file.path(to, f), overwrite)
        }
    } else { # Don't know
        stop("Don't know what to do with: ", from)
    }
}

zip_job <- function(zipfile, folder, root) zip::zip(zipfile, folder, root = root)

delete_job <- function(folder) {
    unlink(folder, recursive = TRUE)
}

# -------------------------------------------------------------------------------------------------


# Read YAML file
yaml <- read_yaml("content.yml")
target_folder <- yaml$`target-folder`
deploy_folder <- here::here(yaml$`deploy-folder`)

# Clean up and make target folder
if (dir.exists(target_folder)) unlink(target_folder, recursive = TRUE)
dir.create(target_folder, recursive = TRUE)


# Jobs
for (job in yaml$jobs) {
    keys <- names(job)
    if (keys[[1]] == "copy") {
        copy_job(
            make_name(job$from, "xxx", "yyy", 99),
            make_name(job$to, "xxx", "yyy", 99),
            job$`exclude-patterns`,
            job$overwrite
        )
    } else {
        stop("Unknown job: ", keys[[1]])
    }
}


# Parts
idx <- 1
for (part in yaml$parts) {
    part_folder <- part$folder
    part_folder_path <- here::here(part_folder)
    if (!dir.exists(part_folder_path)) stop("Source folder does not exist: ", part_folder_path)

    for (job in yaml$`jobs-on-parts`) {
        keys <- names(job)
        name <- str_match(part$folder, yaml$`part-name-pattern`)[2]

        if (keys[[1]] == "copy") {
            copy_job(
                make_name(job$from, here::here(part_folder), name, idx),
                make_name(job$to, part_folder, name, idx),
                job$`exclude-patterns`,
                job$overwrite
            )
        } else if (keys[[1]] == "zip") {
            zip_job(
                make_name(job$zipfile, part_folder, name, idx),
                make_name(job$folder, part_folder, name, idx),
                make_name(job$root, part_folder, name, idx)
            )
        } else if (keys[[1]] == "delete") {
            delete_job(
                make_name(job$folder, here::here(part_folder), name, idx)
            )
        } else {
            stop("Unknown job-on-parts: ", keys[[1]])
        }
    }
    idx <- idx + 1
}
