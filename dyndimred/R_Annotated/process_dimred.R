.process_dimred <- function(space, rn = rownames(space)) {
  space <- as.matrix(space)
  dimnames(space) <- list(rn, paste0("comp_", seq_len(ncol(space)))) # 添加降维后的维度名称
  space
}
