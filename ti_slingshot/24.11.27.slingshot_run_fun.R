setwd("/home/huang/RCode/scrna_tools/dynverse_reproduce/ti_slingshot/")

library(dplyr)
library(purrr)
library(slingshot)
source("package/R_Annotated/ti_slingshot.R") # 直接调用该包

# 读取quickstart调试过程保存的参数
# 在/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/R_Annotated/method_create_ti_method_r.R的.method_execution_execute_function函数中调用
args <- dynutils::read_h5("tmp/args.h5")

# 对应step2, 选择方法
# /home/huang/RCode/scrna_tools/dynverse_reproduce/dynmethods/R_Annotated/method_choose_backend.R中getFromNamespace调用
getFromNamespace("ti_slingshot", "tislingshot")
method <- dynwrap::create_ti_method_r(
  definition = definition,
  run_fun = run_fun,
  return_function = TRUE
)(
  cluster_method = "pam",
  ndim = 20L,
  shrink = 1L,
  reweight = TRUE,
  reassign = TRUE,
  thresh = 0.001,
  maxit = 10L,
  stretch = 2L,
  smoother = "smooth.spline",
  shrink.method = "cosine"
)

# 对应step3. 应用方法, (提前到R_Annotated/ti_slingshot.R的run_fun函数输出部位打断点)
# /home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/R_Annotated/method_create_ti_method_r.R中.method_execution_execute_function调用
output <- method$run$run_fun(
  expression = args$expression,
  priors = args$priors,
  parameters = args$parameters,
  seed = args$seed,
  verbose = args$verbose
)

dyncli::write_output(output, "tmp/output.h5")

output$before_add # 方法的直接输出, 没有wrapper_add, 可以用作后续调试