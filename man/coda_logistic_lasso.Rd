% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coda_lasso.R
\name{coda_logistic_lasso}
\alias{coda_logistic_lasso}
\title{Log Contrast Model}
\usage{
coda_logistic_lasso(
  y,
  X,
  lambda,
  maxiter = 400,
  maxiter2 = 50,
  r = 10,
  tol = 1e-04,
  tol2 = 1e-06,
  printTime = NULL
)
}
\arguments{
\item{y}{binary response variable}

\item{X}{predictor matrix}

\item{lambda}{penalization parameter}

\item{maxiter}{maximum number of iterations for the optimization loop}

\item{maxiter2}{maximum number of iterations for the line search condition.}

\item{r}{fixed parameter used for the update step.}

\item{tol}{tolerance for the difference explained deviance in two consecutive steps.}

\item{tol2}{tolerance to fulfill the constraint:  sum of betas being 1}

\item{printTime}{value to print computing time (works any value)}
}
\description{
Log Contrast Model
}
