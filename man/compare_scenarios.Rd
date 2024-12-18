% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/option.R
\name{compare_scenarios}
\alias{compare_scenarios}
\title{Compare Scenarios Using a Radial Plot}
\usage{
compare_scenarios(tree, scenarios_results, nodes_list)
}
\arguments{
\item{tree}{A \code{Tree} object.}

\item{scenarios_results}{List of \code{numeric} vectors with scenario
evaluation results.}

\item{nodes_list}{List of \code{character} node names to be compared in the
plot.}
}
\value{
No return value, called for side effects
}
\description{
Visualizes the comparison of node values across multiple scenarios with a
radial plot. This representation provides an intuitive view of how different
scenarios compare for the selected nodes.
}
\examples{
tree <- dexisensitivity::masc2
option <- create_options(tree, num_options=3, seed = 42)
scenarios <- evaluate_scenarios(tree, option)
compare_scenarios(tree, scenarios,
 c("Dimension economique", "Dimension sociale", "Dimension environnementale"))

}
\seealso{
\code{\link{evaluate_scenarios}}
}
