#!/usr/local/bin/Rscript

task <- dyncli::main()

library(dyncli, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(dyndimred, warn.conflicts = FALSE)
library(dynwrap, warn.conflicts = FALSE)

#####################################
###           LOAD DATA           ###
#####################################

parameters <- task$parameters
expression <- task$expression

# TIMING: done with preproc
timings <- list(method_afterpreproc = Sys.time())

#####################################
###        INFER TRAJECTORY       ###
#####################################

# perform PCA dimred
dimred <- dyndimred::dimred(expression, method = parameters$dimred, ndim = parameters$ndim)

# transform to pseudotime using atan2
pseudotime <- dimred[,parameters$component] %>% set_names(rownames(expression))

# TIMING: done with method
timings$method_aftermethod <- Sys.time()

#####################################
###     SAVE OUTPUT TRAJECTORY    ###
#####################################
output <-
  wrap_data(
    cell_ids = rownames(expression)
  ) %>%
  add_linear_trajectory(
    pseudotime = pseudotime
  ) %>%
  add_dimred(
    dimred = dimred
  ) %>%
  add_timings(
    timings = timings
  )

dyncli::write_output(output, task$output)
