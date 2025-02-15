library(dplyr)
library(mclust)

setwd("/home/huang/RCode/scrna_tools/dynverse_reproduce/ti_mst/")

task <- dynutils::read_h5("../ti_slingshot/tmp/args.h5")

# ====================后续从run.R中复制过来, 添加额外注释============================
#   ____________________________________________________________________________
#   Load data                                                               ####

expression <- task$expression
parameters <- task$parameters
parameters$dimred <- "pca" # 手动指定使用pca降维

#   ____________________________________________________________________________
#   Infer trajectory                                                        ####


# TIMING: done with preproc
checkpoints <- list(method_afterpreproc = as.numeric(Sys.time()))

# infer dimred
# 计算降维
space <- dyndimred::dimred(expression, method = parameters$dimred, ndim = parameters$ndim)

# cluster cells
# 使用Mcluster聚类细胞, 本质是高斯混合模型，可以指定候选数量范围
# 在Python的sklearn中可以使用GMM替代
clust <- mclust::Mclust(space, modelNames = "EEV", G = 5:15)
# 提取聚类中心作为milestone
centers <- t(clust$parameters$mean)
milestone_ids <- paste0("M", seq_len(nrow(centers)))
rownames(centers) <- milestone_ids

# convert distance to similarity
# 两两里程碑间的距离
dis <- as.matrix(dist(centers))
rownames(dis) <- colnames(dis) <- milestone_ids
disdf <- dis %>%
  reshape2::melt(varnames = c("from", "to"), value.name = "weight") %>%
  na.omit() # 宽数据转化为长数据，相当于全联通图

# calculate mst
# 计算最小生成树，树的边作为milestone_network
gr <- igraph::graph_from_data_frame(disdf, directed = FALSE, vertices = milestone_ids)
mst <- igraph::minimum.spanning.tree(gr, weights = igraph::E(gr)$weight)
milestone_network <-
  igraph::as_data_frame(mst) %>%
  transmute(from, to, length = weight, directed = FALSE)

# TIMING: done with method
checkpoints$method_aftermethod <- as.numeric(Sys.time())

# return output
output <- lst(
  cell_ids = rownames(expression),
  milestone_ids,
  milestone_network,
  dimred_milestones = centers,
  dimred = space,
  timings = checkpoints
)

#   ____________________________________________________________________________
#   Save output                                                             ####

output <- dynwrap::wrap_data(cell_ids = rownames(expression)) %>%
  dynwrap::add_dimred_projection(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    dimred = space,
    dimred_milestones = centers
  ) %>%
  dynwrap::add_timings(checkpoints)