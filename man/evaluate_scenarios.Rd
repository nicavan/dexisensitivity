% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/option.R
\name{evaluate_scenarios}
\alias{evaluate_scenarios}
\title{Evaluate Multiple Scenarios for a Given Tree}
\usage{
evaluate_scenarios(tree, options_matrix)
}
\arguments{
\item{tree}{A \code{Tree} object for evaluation.}

\item{options_matrix}{A \code{matrix} where each column represents a scenario.}
}
\value{
A \code{list} of \code{numeric} vectors with evaluation results for each scenario.
}
\description{
Evaluates multiple scenarios simultaneously using the \code{evaluate_scenario} function.
Each scenario is represented as a column in the \code{options_matrix}.
}
\examples{
tree <- dexisensitivity::masc2
option <- create_options(tree, num_options=3, seed = 42)
scenarios <- evaluate_scenarios(tree, option)
scenarios

}
