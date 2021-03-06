% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{sxsmat}
\alias{sxsmat}
\title{Create Species x Site matrix}
\usage{
sxsmat(data, cols)
}
\arguments{
\item{data}{A data.frame containing species and site info}

\item{cols}{A vector of the form c("SPECIESNAME_COLUMN", "SITENAME_COLUMN") or
c("SPECIESNAME_COLUMN", "SITENAME_COLUMN", "ABUNDANCE_COLUMN")}
}
\value{
An object of class "matrix"
}
\description{
Creates a Species x Site abundance or occurrence matrix from a dataframe
}
\details{
Creates a Species x Site matrix from data tabulated in a csv
(i.e. like DarwinCore download from GBIF). If three column names are
specified in the cols argument, then sxsmat() creates an abundance
matrix where cell values are taken from the inputed column name
"ABUNDANCE_COLUMN". If only two columns are specified sxsmat()
creates a presence absence matrix
}
