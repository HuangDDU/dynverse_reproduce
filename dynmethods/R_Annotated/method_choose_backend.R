#' @importFrom dynwrap create_ti_method_container
method_choose_backend <- function(
  package_repository,
  package_name,
  function_name,
  package_version,
  container_id,
  backend = getOption("dynwrap_backend")
) {
  # 候选使用wrapper脚本或者Docker容器
  correct_backends <- c("r_wrapper", "container")

  if (!is.null(function_name) && is.null(container_id)) {
    # 提供了方法名, 而没有提供docker镜像名
    backend <- "r_wrapper"
  } else if (is.null(function_name) && !is.null(container_id)) {
    # 没有提供了方法名, 而提供docker镜像名
    backend <- "container"
  } else if (is.null(backend)) {
    # 交互式命令行, 手动选择
    # first choose backend: either with an option, using a selection menu (if interactive) or defaults to r_wrapper
    if (interactive()) {

      # prompt user for container vs r_wrapper
      title <- paste(
        paste0("You can run this method as an ", crayon::bold("R wrapper"), " (1, default) or as a ", crayon::bold("container"), " (2)"),
        "Which do you want to use? This option will be saved in options(dynwrap_backend = c('r_wrapper', 'container'))",
        "1: R wrapper [default]",
        "2: Container",
        sep = "\n"
      )

      message(title)
      answer <- readline("")

      if (answer == "2") {
        backend <- "container"
      } else {
        backend <- "r_wrapper"
      }

      # set option
      options(dynwrap_backend = backend)
    } else {
      # 否则, 默认选择wrapper脚本执行
      message("dynwrap_backend option not set, assuming that you want to run this method as an R wrapper")

      backend <- "r_wrapper"
    }
  }

  if (is.null(backend) || !backend %in% correct_backends) {
    stop("Invalid dynwrap_backend option: ", backend)
  }

  # now return method
  if (backend == "r_wrapper") {
    # 从Github下载执行脚本与包环境, 注意这里传入的
    install_github_tagged_version(paste0(package_name, "=", package_repository), package_version)
    getFromNamespace(function_name, package_name)

  } else if (backend == "container") {
    # 创建docker容器, 这里检查和拉取docker镜像
    dynwrap::create_ti_method_container(container_id = container_id)
  }
}
