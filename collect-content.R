#!/usr/bin/env Rscript

library(yaml)
library(purrr)
library(readr)
library(tools)
library(stringr)

# Used but not importet because of naming conflicts
# library(zip)
# library(here)


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


# -------------------------------------------------------------------------------------------------
# Job functions
# -------------------------------------------------------------------------------------------------

copy_job <- function(job) {
    from <- substitute_definitions(job$from)
    to <- substitute_definitions(job$to)
    exclude_patterns <- job$`exclude-patterns`
    overwrite <- job$overwrite

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

zip_job <- function(job) {
    zipfile <- substitute_definitions(job$zipfile)
    folder <- substitute_definitions(job$folder)
    root <- substitute_definitions(job$root)
    zip::zip(zipfile, folder, root = root)
}

delete_job <- function(job) {
    folder <- substitute_definitions(job$folder)
    unlink(folder, recursive = TRUE)
}

# pwp: Path with pattern
do_assignments <- function(pwps, with_solution) {
    #
    # Process files
    for (pwp in pwps) {
        path <- substitute_definitions(pwp)
        folder <- dirname(path)
        pattern <- basename(path)

        for (file in list.files(path = folder, pattern = pattern)) {
            file_sol <- str_replace(file, fixed(".qmd"), ".sol.qmd")
            is_solution <- str_detect(file, fixed(".sol."))
            have_solution <- file.exists(file.path(folder, file_sol))

            if (!is_solution) {
                #
                # Cat assignment
                cat(read_lines(file.path(folder, file)), sep = "\n")
                cat("\n")

                # Announce solution
                if (with_solution) {
                    cat("### Lösung {-}\n\n")
                    if (!have_solution) cat("Keine Lösung\n\n")
                }
            } else if (with_solution) {
                #
                # Cat solution
                cat(read_lines(file.path(folder, file)), sep = "\n")
                cat("\n")
            }
        }
    }
}

assignment_paper_job <- function(job) {
    define("sol", job$sol)
    define("subtitle", job$subtitle)

    sink(substitute_definitions("${target-folder}/aufgabenblatt-${idx}.qmd"))
    cat(substitute_definitions("${assignment-header}"))
    cat("\n")
    do_assignments(job$files, with_solution = FALSE)
    sink()

    sink(substitute_definitions("${target-folder}/aufgabenblatt-${idx}-loesung-${sol}.qmd"))
    cat(substitute_definitions("${assignment-solution-header}"))
    cat("\n")
    do_assignments(job$files, with_solution = TRUE)
    sink()
}

increment_index_job <- function(job) increment_index()

jobs <- list(
    "copy" = copy_job,
    "zip" = zip_job,
    "delete" = delete_job,
    "assignment-paper" = assignment_paper_job,
    "increment-index" = increment_index_job
)

handle_job <- function(job) jobs[[names(job)[[1]]]](job)


# -------------------------------------------------------------------------------------------------
# Definitions and index
# -------------------------------------------------------------------------------------------------

# Definitions
definitions <- list()

define <- function(var, val) definitions[[var]] <<- val

substitute_definitions <- function(s) {
    for (key in names(definitions)) {
        value <- definitions[[key]]
        var <- paste0("${", key, "}")
        if (length(value)) s <- str_replace_all(s, fixed(var), value)
    }
    s
}

# Index
idx <- 0

reset_index <- function() {
    idx <<- 0
    increment_index()
}

increment_index <- function() {
    idx <<- idx + 1
    define("sidx", idx)
    define("idx", str_pad(idx, 2, side = "left", pad = "0"))
}


# -------------------------------------------------------------------------------------------------
# Process input
# -------------------------------------------------------------------------------------------------


# Message
cat("Collecting content...")

# Read YAML file
yaml <- read_yaml("content.yml")

target_folder <- yaml$`target-folder`
deploy_folder <- here::here(yaml$`deploy-folder`)


define("target-folder", target_folder)
define("deploy-folder", deploy_folder)
define("project-folder", here::here())


for (def in yaml$definitions) {
    var <- names(def)[1]
    val <- def[[var]]
    define(var, val)
}


# Clean up and make target folder
target_folder <- definitions[["target-folder"]]
if (dir.exists(target_folder)) unlink(target_folder, recursive = TRUE)
dir.create(target_folder, recursive = TRUE)


# Process jobs
reset_index()
walk(yaml$jobs, handle_job)


# Process parts
reset_index()
for (part in yaml$parts) {
    #
    # Define name and part folder
    define("name", str_match(part$folder, yaml$`part-name-pattern`)[2])
    define("part-folder", here::here(part$folder))

    # Check if part folder exists
    if (!dir.exists(definitions[["part-folder"]])) stop("Part folder does not exist: ", definitions[["part-folder"]])

    # Process jobs
    walk(yaml$`jobs-on-parts`, handle_job)

    # Increment counter
    increment_index()
}
