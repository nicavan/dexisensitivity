% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/option.R
\name{evaluate_scenario}
\alias{evaluate_scenario}
\title{Evaluate Node Values in a Tree}
\usage{
evaluate_scenario(tree, option)
}
\arguments{
\item{tree}{A \code{Tree} object.}

\item{option}{A matrix representation of a scenario, providing values for the
tree's leaves. Each column in the matrix corresponds to a tree leaf, and the
rows provide different values for the scenario analysis.}
}
\value{
A named \code{numeric} vector representing the evaluated values for all
nodes, progressing from the leaves to the root.
}
\description{
Calculates the values of the tree nodes using a bottom-up approach. The function
starts by assigning values to the leaves, based on the provided option, and then
aggregates these values up the tree to determine each node's value. This ensures
each node's value considers the values of its child nodes.
}
\details{
Begins by assigning values to the leaves of the tree based on the
provided option. If the tree structure indicates leaf-aggregated scenarios,
these values are aggregated accordingly. The function then continues to aggregate
values up the tree, considering the tree's structure, to determine each node's value.

It's essential for the input option matrix to have columns that correspond to the
leaves of the tree and for the tree object to have the appropriate attributes set.
}
\examples{
tree <- dexisensitivity::masc2
option <- create_options(tree, num_options=1, seed = 42)
scenario <- evaluate_scenario(tree, option)
scenario

}
