

install_if_ <- function(x, ip = NULL) {
  if (is.null(ip)) {
    ip <- installed.packages()
  }
  i <- !x %in% ip
  if (i) {
    install_pkg_verbose(x)
  }
  i
}

#' Install packages if not already
#'
#' Wrapper around install.packages
#'
#' @param pkg Character; one or more packages to install.
#' @return Returns data frame with pkg (chr) and required_install (lgl) columns.
#' @export
install_if <- function(pkg) {
  x <- vapply(pkg, install_if_,
    ip = installed.packages(),
    FUN.VALUE = logical(1))
  data.frame(pkg = names(x), required_install = x,
    row.names = NULL)
}



task_progress_bar <- function(task, msg) {
  w <- getOption("width", 100)
  ## b/c only monsters set width > 100
  if (w > 100)
    w <- 100
  r <- floor((w - nchar(msg) - 9) / 2)
  cat(paste0(msg, " +"))
  for (j in seq_len(r)) {
    Sys.sleep(.025)
    cat("+")
  }
  eval(task)
  for (k in seq_len(r)) {
    Sys.sleep(.025)
    if (k == r) {
      cat(" 100%", fill = TRUE)
    } else {
      cat("+")
    }
  }
  invisible(TRUE)
}

install_pkg_verbose <- function(pkg) {
  if (.Platform$OS.type == "unix") {
    type <- "source"
  } else {
    type <- getOption("pkgType")
  }
  repos <- getOption("repos", "https://cran.rstudio.com/")
  sh <- tryCatch(utils::install.packages(pkg, quiet = TRUE,
    repos = repos,
    verbose = FALSE, type = type),
    error = function(e) NULL)
  msg <- sprintf("Installing {%s}", pkg)
  msg <- paste0(msg, paste(rep(" ", 15 - (nchar(pkg) %/% 2) * 2), collapse = ""))
  task_progress_bar(invisible(), msg)
}
