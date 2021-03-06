% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainFunctions.R
\name{ideaSubmitJobs}
\alias{ideaSubmitJobs}
\title{Submit Jobs}
\usage{
ideaSubmitJobs(reg, ...)
}
\arguments{
\item{reg}{An IDE+A Registry}

\item{...}{Additional parameters passed to batchtools::submitJobs}
}
\description{
A wrapper around batchtools::submitJobs.
The wrapper is necessary to implement rsync usage to synchronize local files with the cluster.
}
