% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_if.R
\name{install_if}
\alias{install_if}
\title{Install packages if not already}
\usage{
install_if(pkg)
}
\arguments{
\item{pkg}{Character; one or more packages to install.}
}
\value{
Returns data frame with pkg (chr) and required_install (lgl) columns.
}
\description{
Wrapper around install.packages
}
