% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_subtree.R
\name{create_sub_tree}
\alias{create_sub_tree}
\title{Create a Subtree Starting from a Specific Node}
\usage{
create_sub_tree(tree, node_name, avoid_repetition = FALSE)
}
\arguments{
\item{tree}{a \code{Tree} object from which the subtree is derived.}

\item{node_name}{A \code{character} indicating the name of the starting
\code{Node} for the subtree.}

\item{avoid_repetition}{A \code{logical} indicating whether to avoid node
repetitions. Default is \code{FALSE}.}
}
\value{
A new \code{Tree} object representing the subtree.
}
\description{
Constructs a subtree from a specified \code{Node} within a given \code{Tree}.
This subtree encompasses all descendant attributes of the \code{Node}.
}
\examples{
tree <- dexisensitivity::masc2
subtree <- create_sub_tree(masc2, masc2@Nodes[[2]]@Name)
subtree

# Equivalent to :
tree <- dexisensitivity::masc2
subtree <- create_sub_tree(masc2, "Dimension economique")
subtree

}
\seealso{
Relevant functions and classes that provide more context or might be
of interest:
\itemize{
\item \code{\link{Tree-class}}: For an in-depth understanding of the
\code{Tree} class.
\item \code{\link{Node-class}}: To get more details about the structure of
a \code{Node}.
}
}
