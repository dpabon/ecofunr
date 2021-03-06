% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cue.R
\name{cue}
\alias{cue}
\title{Carbon Use Efficiency}
\usage{
cue(
  GPP,
  NEP,
  NPP,
  Rb,
  method = "plants",
  dates,
  aggregation.time = NULL,
  aggregation.metric = "median",
  overlapping = F,
  prob = 0.9
)
}
\arguments{
\item{GPP}{Gross Primary Production (units)}

\item{NEP}{Net Ecosystem Productivity (units)}

\item{NPP}{Net Primary Productivity}

\item{Rb}{Basal Respiration (See Reddyproc function XXX)}

\item{method}{Can be "plants", "ecosystem", "apparent", "all". See details.}

\item{dates}{a vector of class "Date" with the same length of GPP.}

\item{aggregation.time}{Can be "NULL", "day", "month", "year", growing season "gs". if it's
a number it's considered as a time window in number of days. If "optim" the optimum number of days will be estimate to reduce the sd of each chunck. See \code{\link[ecofunr]{aggreg}}.}

\item{aggregation.metric}{Can be "mean", "max", "min", "median", and "quant". if "quant" a number between 0 and 1 need to be provided for prob parameter.}

\item{overlapping}{Can be "NULL" or a number. If it's a number equivale to the parameter by of the \cite{\link[zoo]{rollapply}} function.}

\item{prob}{Only used if aggregation.metric is "quant", a number between 0 and 1. By default 0.9.}
}
\value{
An object of type "data.frame" if method = "all". Otherwise a numeric vector.
}
\description{
This function estimate different metrics of Carbon Use Efficiency
}
\section{Methods}{


\strong{plants}
Carbon Use efficiency (CUE) provides a measure of what fraction of total carbon assimilation becomes
incorported into new tissues. \insertCite{chambers_respiration_2004}{ecofunr}

\deqn{CUE = \frac{NPP}{GPP}}{CUE = NPP / GPP}

\strong{ecosystem} \insertCite{fernandez-martinez_nutrient_2014}{ecofunr}:

\deqn{CUE = \frac{NEP}{GPP}}{CUE = NEP / GPP}

\strong{apparent} (Reference is missing)

\deqn{CUE = 1 - \frac{Rb}{GPP}}{CUE = 1 - (Rb / GPP)}

For a complete review about all CUE metrics at different spatial and temporal scales check \insertCite{manzoni_reviews_2018}{ecofunr}
}

\references{
\insertAllCited{}
}
