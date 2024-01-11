#!/usr/bin/env Rscript

# Subset targets --------------------------------------------------------------

project <- "actuarial_challenge"

tbd_targets <- c(
  NULL
)

# Run targets plan (analyse data & build reports) -----------------------------

Sys.setenv(TAR_PROJECT = project)

outdated_targets <- targets::tar_outdated()
targets_metadata <- targets::tar_meta()
rerun_time <- dplyr::filter(targets_metadata, name %in% outdated_targets)$seconds

if(length(rerun_time) > 0) {
  cat(
    "\n\nPredicted computation time:"
    , lubridate::seconds_to_period(sum(rerun_time, na.rm = TRUE))
    , "\n"
  )
}

untimed_targets <- dplyr::filter(
  targets_metadata, name %in% outdated_targets & is.na(seconds)
)$name

if(length(rerun_time) > 0) {
  cat(
    "\nTargets with nonestimable computation time estimate:"
    , if(length(untimed_targets) == 0) "None" else paste(untimed_targets, collapse = ", ")
    , "\n"
  )
}

cat(paste0("\nIt is now:  ", Sys.time()), "\n\n")

targets::tar_make(names = !!tbd_targets) 
