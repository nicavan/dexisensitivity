% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monte_carlo.R
\name{monte_carlo}
\alias{monte_carlo}
\title{Monte Carlo Simulation on a Decision Tree}
\usage{
monte_carlo(tree, num_runs, write_to_file = NULL)
}
\arguments{
\item{tree}{\code{Tree} object to be used in the simulation.}

\item{num_runs}{\code{numeric} indicating the number of Monte Carlo
simulations to be executed.}

\item{write_to_file}{\code{character} Name of the file created to save the
Monte Carlo's results. If \code{NULL}, don't write any file. Default is
\code{NULL}}
}
\value{
A \code{matrix} containing the results of the Monte Carlo simulation.
}
\description{
Conducts a Monte Carlo simulation over a provided decision tree for a
specified number of runs. Optionally, the function can save the random
options selected for the analysis in a .csv file named "MC options.csv".
}
\examples{
tree <- dexisensitivity::masc2
MC <- monte_carlo(tree, 100)

}
