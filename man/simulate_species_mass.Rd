% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_community_data.R
\name{simulate_species_mass}
\alias{simulate_species_mass}
\title{Simulate species' masses}
\usage{
simulate_species_mass(nspecies, min_mass, max_mass)
}
\arguments{
\item{nspecies}{S}

\item{min_mass}{Min average mass}

\item{max_mass}{Max average mass}
}
\value{
vector of body sizes for a community of S species
}
\description{
Randomly draw mean masses for S species with minimum Min and maximum Max, from a uniform distribution.
}
