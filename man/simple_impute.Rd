% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/composition_utils.R
\name{simple_impute}
\alias{simple_impute}
\title{Impute the zeros with a pseudocount}
\usage{
simple_impute(count_mat, scale = 0.5)
}
\arguments{
\item{count_mat}{count matrix (nsample*nfeature)}

\item{scale}{for an entry with zeros, impute the zeros with scale* smallest nonzero value in each row (sample)
@return imputed count matrix}
}
\description{
Impute the zeros with a pseudocount
}
