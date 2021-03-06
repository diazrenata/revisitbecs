% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_distributions.R
\name{make_bsd}
\alias{make_bsd}
\title{Construct BSD from df with community data}
\usage{
make_bsd(community_df, ln_units = 0.2, decimals = NULL)
}
\arguments{
\item{community_table}{df of species ids, individual sizes, energy, and size class}
}
\value{
bsd of # species with mean mass in each size class
}
\description{
species level body size distributions
}
