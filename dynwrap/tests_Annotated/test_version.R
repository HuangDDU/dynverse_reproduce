# 包含dyn字符的包的版本查看
pattern <- "dyn"
installed_packages <- installed.packages()
installed_packages[grepl(pattern, installed_packages[, "Package"]), c("Package", "Version")]