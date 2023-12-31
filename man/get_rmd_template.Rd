% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rmd_template.R
\name{get_rmd_template}
\alias{get_rmd_template}
\title{Get .Rmd template}
\usage{
get_rmd_template(dir = "./out", name)
}
\arguments{
\item{dir}{A character string for the path to the directory.}

\item{name}{A character value for the name of the template.}
}
\value{
The path to the template.
}
\description{
The function copies an .Rmd template file from the APACo package to put it in a new directory
called 'out' created  at the root of the current directory when running the analytical
pipeline.
}
