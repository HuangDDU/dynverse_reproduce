setwd("/home/huang/RCode/scrna_tools/dynverse_reproduce/ti_paga")

library(dynwrap)

# 从output.h5文件里提取在PAGA运行脚本调用R时的输入
output <- dynutils::read_h5("tmp/output.h5")
before_add <- output$before_add
cell_ids <- before_add$cell_ids
grouping <- before_add$grouping
branch_progressions <- before_add$branch_progressions
branches <- before_add$branches
branch_network <- before_add$branch_network
dimred <- before_add$dimred
checkpoints <- before_add$checkpoints


dataset <- wrap_data(cell_ids=cell_ids)
# 主要看这里的添加结果时的调试, 内部调用了add_trajectory
dataset <- add_branch_trajectory(
  dataset = dataset,
  grouping = grouping,
  branch_progressions = branch_progressions,
  branches = branches,
  branch_network = branch_network,
)
dataset <- add_dimred(
  dataset = dataset,
  dimred = dimred,
)
dataset <- add_timings(
  dataset = dataset,
  timings = checkpoints
)