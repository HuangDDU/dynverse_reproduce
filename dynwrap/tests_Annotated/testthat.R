library(testthat)
library(dynwrap)

# 设置测试路径，待测试代码放在testhad下面
setwd("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated")

# 默认测试报告格式为check
test_check("dynwrap")

# 生成其他类型的报告，报告类型可选：https://testthat.r-lib.org/reference/Reporter.html
# test_check("dynwrap", "summary") # 简单报告
# test_check("dynwrap", "progress") # 显示进度的报告
# test_check("dynwrap", "junit") # 测试单元的报告

# 运行本文件，生成测试日志：Rscript testthat.R > testthat.log


