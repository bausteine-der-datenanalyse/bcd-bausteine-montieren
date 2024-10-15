library(purrr)

collect_one_slidedeck <- function(root, idx, common_files, to_subdir) {
    #
    # Input directory and existence check
    dir_root <- file.path(dir_in_base, root)
    dir_in <- file.path(dir_root, "folien")
    if (!dir.exists(dir_in)) stop("Folder ", dir_in, " does not exist")

    # Output
    name <- str_sub(basename(dir_root), 3)
    if (to_subdir) {
        name_out <- fname(paste0("${idx}-", name), idx, extension = NULL)
        dir_out <- file.path(dir_out_base, name_out)
    } else {
        name <- fname(paste0("${idx}-", name), idx, extension = NULL)
        dir_out <- dir_out_base
    }

    # Make output directory and copy subdirectories
    if (!dir.exists(dir_out)) dir.create(dir_out)
    walk(subdirs, \(dir) copy_dir(dir, dir_in, dir_out))

    # Copy common files
    for (f in common_files) {
        name_to <- basename(f)
        if (name_to == "quarto-template.yml") {
            if (to_subdir) {
                name_to <- "_quarto.yml"
            } else {
                name_to <- "_metadata.yml"
            }
        }
        file.copy(f, file.path(dir_out, name_to))
    }

    # Copy slides.qmd and optionally slides.Rproj
    exts_to_copy <- c(".qmd")
    if (to_subdir) exts_to_copy <- append(exts_to_copy, ".Rproj")
    for (ext in exts_to_copy) {
        from <- file.path(dir_in, paste0("folien", ext))
        to <- file.path(dir_out, paste0(name, ext))
        if (!file.exists(from)) stop("File ", from, " does not exist")
        file.copy(from, to)
    }

    # Zip and delete - path to _output is a hack
    if (to_subdir) {
        zipfile <- paste0(name_out, ".zip")
        zipfile_path <- file.path(dir_out_base, zipfile)
        output_dir <- file.path(here::here(), "_output/folien/c")
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        zip::zip(zipfile, name_out, root = dir_out_base)
        file.copy(zipfile_path, file.path(output_dir, basename(zipfile)))
        unlink(dir_out, recursive = TRUE)
    }
}

process_slides <- function(slides, dir_in_base, dir_out_base, subdirs) {
    #
    # Common files copied to each directory
    bd <- file.path(here::here(), slides$root)
    common_files <- c(
        file.path(bd, "_quarto.yml"),
        file.path(bd, "style.scss")
    )

    # Loop over content
    idx <- 1
    for (entry in slides$content) {
        common_files <- c(
            file.path(bd, "quarto-template.yml"),
            file.path(bd, "style.scss")
        )

        collect_one_slidedeck(entry$root, idx, common_files, TRUE)
        collect_one_slidedeck(entry$root, idx, common_files, FALSE)

        # Loop
        idx <- idx + 1
    }
}
