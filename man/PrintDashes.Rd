% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{PrintDashes}
\alias{PrintDashes}
\title{Draw a line of dashes, or something else with a certain width.}
\usage{
PrintDashes(symbol = "=", width = 48, ...)
}
\arguments{
\item{symbol}{Symbol to repeat \code{width} times.}

\item{width}{The desired width (number of symbols)}

\item{...}{Further arguments passed to 'strwrap()'}
}
\value{
a character string of nchar \code{width}
}
\description{
Draw a line of dashes, or something else with a certain
width. Is used in package in combination with PrintWrap.
}
\author{
Gilles Dutilh
}
