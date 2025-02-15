library(purrr)
library(dplyr)
library(dynwrap)

setwd("/home/huang/RCode/scrna_tools/dynverse_reproduce/ti_comp1/")

task <- dynutils::read_h5("../ti_slingshot/tmp/args.h5")

# ====================后续从run.R中复制过来, 添加额外注释============================
#####################################
###           LOAD DATA           ###
#####################################

parameters <- task$parameters
expression <- task$expression
parameters$dimred <- "pca" # 手动指定使用pca降维
parameters$component <- "comp_1"

# TIMING: done with preproc
timings <- list(method_afterpreproc = Sys.time())

#####################################
###        INFER TRAJECTORY       ###
#####################################

# perform PCA dimred
dimred <- dyndimred::dimred(expression, method = parameters$dimred, ndim = parameters$ndim)

# transform to pseudotime using atan2
pseudotime <- dimred[, parameters$component] %>% set_names(rownames(expression))

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
