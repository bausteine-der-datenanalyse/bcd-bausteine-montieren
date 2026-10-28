#!/usr/bin/env Rscript

library(yaml)
library(purrr)
library(readr)
library(tools)
library(stringr)

# Used but not imported
# - rprojroot
# - zip

# -------------------------------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------------------------------

is_true <- function(b) !is.null(b) && b

from_project_root <- function(file_or_folder = "") rprojroot::is_r_package$find_file(file_or_folder)

test_include <- function(path, exclude_patterns) {
  for (p in exclude_patterns) {
    if (str_detect(path, p)) {
      return(FALSE)
    }
  }
  file_test("-f", path)
}

prepare_copy_to <- function(to, overwrite) {
  if (file.exists(to) && !is_true(overwrite)) {
    stop("File exists: ", to)
  }
  folder <- dirname(to)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
}

file_copy_safe <- function(from, to, overwrite) {
  prepare_copy_to(to, overwrite)
  file.copy(from, to)
}

do_transform_replace <- function(file, pattern, replacement) {
  in_chunk <- FALSE
  lines <- read_lines(file)
  for (i in seq_along(lines)) {
    l <- lines[i]

    # State
    if (startsWith(l, "```{r}")) {
      in_chunk <- TRUE
    } else if (in_chunk && startsWith(l, "```")) {
      in_chunk <- FALSE
    }

    # Transform if not in chunk
    if (!in_chunk) {
      lines[i] <- str_replace_all(l, regex(pattern), replacement)
    } else {
      lines[i] <- l
    }
  }
  write_lines(lines, file)
}

# Insert text after the YAML header (or at the beginning if there is none)
do_transform_insert_after_header <- function(file, text) {
  lines <- read_lines(file)
  end_header <- 0
  if (length(lines) && lines[1] == "---") {
    end_header <- which(lines[-1] == "---")[1] + 1
  }
  lines <- append(lines, c("", str_split_1(substitute_definitions(text), "\n")), after = end_header)
  write_lines(lines, file)
}

do_transform <- function(files, transform) {
  for (file in files) {
    if (transform$type == "replace") {
      do_transform_replace(file, transform$pattern, transform$replacement)
    } else if (transform$type == "insert-after-header") {
      do_transform_insert_after_header(file, transform$text)
    } else {
      stop("Unknown transform: ", transform$type)
    }
  }
}


# -------------------------------------------------------------------------------------------------
# Job functions
# -------------------------------------------------------------------------------------------------

copy_job_do <- function(
  from,
  to,
  exclude_patterns = NULL,
  overwrite = FALSE,
  optional = FALSE,
  transform = NULL
) {
  # Copy and create list of copied files
  files <- list()
  if (file_test("-f", from)) {
    # Copy one file
    file_copy_safe(from, to, overwrite)
    files <- append(files, to)
  } else if (file_test("-d", from)) {
    # Copy folder
    for (f in list.files(from)) {
      from_path <- file.path(from, f)
      to_path <- file.path(to, f)
      if (test_include(from_path, exclude_patterns)) {
        file_copy_safe(from_path, to_path, overwrite)
        files <- append(files, to_path)
      }
    }
  } else if (!is_true(optional)) {
    # Don't know
    stop("No such file or directory: ", from)
  }

  # Transform files if requested
  if (!is.null(transform)) {
    do_transform(files, transform)
  }
}

copy_job <- function(job) {
  copy_job_do(
    substitute_definitions(job$from),
    substitute_definitions(job$to),
    job$`exclude-patterns`,
    job$overwrite,
    job$optional,
    job$transform
  )
}

zip_job <- function(job) {
  #
  # Definitions
  zipfile <- substitute_definitions(job$zipfile)
  folder <- substitute_definitions(job$folder)
  root <- substitute_definitions(job$root)

  # Create path to zipfile if needed
  zipfile_path <- dirname(zipfile)
  if (!dir.exists(zipfile_path)) {
    dir.create(zipfile_path, recursive = TRUE)
  }

  # Zip
  zip::zip(zipfile, folder, root = root)
}

symlink_job <- function(job) {
  link <- substitute_definitions(job$link)
  if (!dir.exists(dirname(link))) dir.create(dirname(link), recursive = TRUE)
  # Remove existing link (or file) as ln -sfn does, unlink does not follow links
  if (nzchar(Sys.readlink(link)) || file.exists(link)) unlink(link)
  file.symlink(substitute_definitions(job$target), link)
}

delete_job <- function(job) {
  folder <- substitute_definitions(job$folder)
  unlink(folder, recursive = TRUE)
}

# Value of definition `zipfile` for the part with the given folder: index and name are those of the part, not of
# the assignment paper (an assignment paper can contain several parts). NULL if there is no such part.
zipfile_for_folder <- function(folder) {
  if (is.null(definitions[["zipfile"]])) {
    return(NULL)
  }
  for (i in seq_along(yaml$parts)) {
    part_folder <- yaml$parts[[i]]$folder
    if (part_folder != "skip" && normalizePath(from_project_root(part_folder)) == normalizePath(folder)) {
      zipfile <- definitions[["zipfile"]] |>
        str_replace_all(fixed("${idx}"), str_pad(i, 2, side = "left", pad = "0")) |>
        str_replace_all(fixed("${name}"), str_match(part_folder, yaml$`part-name-pattern`)[2])
      return(substitute_definitions(zipfile))
    }
  }
  NULL
}

# Read file, replace placeholder ${zipfile} if there is a value. R chunks in solution and work blocks
# (::: {.tinh-solution} or ::: {.tinh-work} up to the next :::) are only shown, not run (`{r}` becomes `{.r}`):
# knitr would run them on both sheets before the filter of the extension tinh removes or keeps the blocks.
read_assignment_lines <- function(file, zipfile) {
  lines <- read_lines(file)
  if (!is.null(zipfile)) {
    lines <- str_replace_all(lines, fixed("${zipfile}"), zipfile)
  }
  in_solution <- FALSE
  for (i in seq_along(lines)) {
    if (str_detect(lines[i], "^:::+\\s*\\{\\.tinh-(solution|work)\\}")) {
      in_solution <- TRUE
    } else if (in_solution && str_detect(lines[i], "^:::+\\s*$")) {
      in_solution <- FALSE
    } else if (in_solution) {
      lines[i] <- str_replace(lines[i], "^```\\{r\\}", "```{.r}")
    }
  }
  lines
}

assignment_paper_job_do <- function(pwps) {
  #
  # Process files
  # pwp stands for path with pattern
  for (pwp in pwps) {
    pwp <- substitute_definitions(pwp)
    folder <- dirname(pwp)
    pattern <- basename(pwp)
    zipfile <- zipfile_for_folder(folder)

    for (file in list.files(path = folder, pattern = pattern)) {
      cat(read_assignment_lines(file.path(folder, file), zipfile), sep = "\n")
      cat("\n")
    }
  }
}

assignment_paper_job <- function(job) {
  define("sol", job$sol)
  define("subtitle", job$subtitle)

  # Assignment papers
  sink(substitute_definitions("${target-folder}/aufgabenblatt-${idx}.qmd"))
  cat(substitute_definitions("${assignment-header}"))
  cat("\n")
  assignment_paper_job_do(job$files)
  sink()

  # Solutions
  sink(substitute_definitions("${target-folder}/aufgabenblatt-${idx}-loesung-${sol}.qmd"))
  cat(substitute_definitions("${assignment-solution-header}"))
  cat("\n")
  assignment_paper_job_do(job$files)
  sink()

  # Folders
  for (pwp in job$folders) {
    pwp <- substitute_definitions(pwp)
    folder <- dirname(pwp)
    pattern <- basename(pwp)

    for (e in list.files(path = folder, pattern = pattern)) {
      from <- file.path(folder, e)
      to <- file.path(substitute_definitions("${target-folder}"), e)
      if (file_test("-d", from)) copy_job_do(from, to)
    }
  }
}

increment_index_job <- function(job) increment_index()

jobs <- list(
  "copy" = copy_job,
  "zip" = zip_job,
  "delete" = delete_job,
  "symlink" = symlink_job,
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
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (key in names(definitions)) {
      value <- definitions[[key]]
      if (length(value)) {
        s_old <- s
        var <- paste0("${", key, "}")
        s <- str_replace_all(s, fixed(var), value)
        if (s != s_old) changed <- TRUE
      }
    }
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
cat("Collecting content...\n")


# Read YAML file
yaml <- read_yaml("content.yml")


# Folders
target_folder <- yaml$`target-folder`
deploy_folder <- from_project_root(yaml$`deploy-folder`)


# Definitions
define("target-folder", target_folder)
define("deploy-folder", deploy_folder)
define("project-folder", from_project_root())


# Definitions from YAML
for (def in yaml$definitions) {
  var <- names(def)[1]
  val <- def[[var]]
  define(var, val)
}


# Clean up and make target folder. The deploy folder is not cleaned up since it contains rendered files when this
# script runs as pre-render.
if (dir.exists(target_folder)) {
  unlink(target_folder, recursive = TRUE)
}
dir.create(target_folder, recursive = TRUE)


# Process jobs
reset_index()
walk(yaml$jobs, handle_job)


# Process parts
reset_index()
for (part in yaml$parts) {
  if (part$folder != "skip") {
    #
    # Define name and part folder
    define("name", str_match(part$folder, yaml$`part-name-pattern`)[2])
    define("part-folder", from_project_root(part$folder))

    # Check if part folder exists
    if (!dir.exists(definitions[["part-folder"]])) {
      stop("Part folder does not exist: ", definitions[["part-folder"]])
    }

    # Process jobs
    walk(yaml$`jobs-on-parts`, handle_job)
  }

  # Increment counter
  increment_index()
}
