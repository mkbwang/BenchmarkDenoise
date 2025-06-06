% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DFBM_utils.R
\name{cutoff_selection}
\alias{cutoff_selection}
\title{Select Cutoffs}
\usage{
cutoff_selection(
  count_mat,
  begin = 0,
  rate = 0.9,
  gap = 0.005,
  min_prop = 0.005
)
}
\arguments{
\item{count_mat}{count matrix}

\item{begin}{smallest threshold}

\item{rate}{geometric sequence rate for selecting thresholds}

\item{gap}{minimum proportion of entries that change from1 to zero between adjacent thresholds}

\item{min_prop}{the minimum of the proportion of entries larger than the largest threshold}
}
\value{
a list with the sequence of thresholds and the proportion of entries larger than each entry
}
\description{
Select Cutoffs
}
