#' Title
#'
#' @param aTree A Tree
#' @param nbOptions Number option to create
#' @param aSeed seed for random number generation
#'
#' @return
#' @export
#'
#' @examples
createOptions <- function(aTree, nbOptions = 1, aSeed = -1) {
    if (aSeed>0) {
        set.seed(aSeed)
    }

    option <- matrix(nrow = aTree@nbLeaves, ncol = nbOptions)
    rownames(option) <- aTree@Leaves

    for(k in aTree@Leaves) {
        option[k, ] <- sample(aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@rangeScale,
                              size = nbOptions,
                              prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                              replace = TRUE)
    }

    return(option)
}


#' Title
#'
#' @param aTree a Tree
#' @param option 1 Option
#'
#' @return
#' @export
#'
#' @examples
EvaluateScenario <- function(aTree, option) {

    # Create the return array
    if (!is.matrix(option)) {
        stop(cat("\nl'option n'est pas une matrice"), call)
    }

    results <- numeric(aTree@nbAttributes)
    names(results) <- aTree@Attributes
    results[] <- -1

    # Give value for all leaves --- Deal with multiple leaves.
    # Leaf-Aggretated leaves should not be in option
    for(i in 1:length(option)) {
        results[which(names(results) == dimnames(option)[[1]][i])] <- option[i]
    }

    # If leaf aggregated leaves, deal first with the subtree
    if (aTree@isLeafAggregated) {
        for(i in 1:length(aTree@EvalOrder)) {
            subTree <- createSubTree(aTree,
                                     aTree@Attributes[aTree@EvalOrder[i]])

            #Get the proper option
            subOption <- as.matrix(results[c(subTree@Leaves)])
            for(j in rev(subTree@Aggregated)) {

                # Compute the value thanks to the attribution table
                if(results[j] < 0) {
                    id <- getID(subTree@Nodes, j)

                    if(length(id) > 1) {
                        id <- id %>%
                            sapply(function(x) {
                                if (!subTree@Nodes[[x]]@isLeaf) {x}
                                }) %>%
                            unlist()
                    }

                    # It may happen that the tree structure is repeated !!!!
                    for(ii in id) {
                        nbc <- length(subTree@Nodes[[ii]]@children)
                        value <- subTree@Nodes[[ii]]@aggregation

                        for(k in 1:nbc) {
                            value <- value[value[, k] == results[subTree@Nodes[[ii]]@children[k]], ]
                        }

                        results[j]<-value[nbc+1]
                    }
                }
            }
        }
    }

    # Thanks to the hierchical structure of the tree... use rev()
    for(k in aTree@Attributes) {
        results[getID(aTree@Nodes, k)] <- max(results[getID(aTree@Nodes, k)])
    }

    for(i in rev(aTree@Aggregated)) {
        #Compute the value thanks to the attribution table
        if (results[i] < 0) {
            id <- getID(aTree@Nodes, i)

            if(length(id) > 1) {
                id <- id %>%
                    sapply(function(x) {if (!aTree@Nodes[[x]]@isLeaf) {x}})
            }

            nbc <- length(aTree@Nodes[[id]]@children)
            value <- aTree@Nodes[[id]]@aggregation

            for(j in 1:nbc) {
                value <- value[value[, j] == results[aTree@Nodes[[id]]@children[j]], ]
            }

            results[i] <- value[nbc + 1]
        }
    }

    for(k in aTree@Attributes) {
        results[getID(aTree@Nodes, k)] <- max(results[getID(aTree@Nodes, k)])
    }

    return(results)
}

#' Title
#'
#' @param aTree a tree
#' @param options Several options
#'
#' @return
#' @export
#'
#' @examples
EvaluateScenarios <- function(aTree, options) {
    1:dim(options)[2] %>%
        sapply(function(x) {
            EvaluateScenario(aTree, as.matrix(options[, x]))
        })
}


#' Title
#'
#' @param anOptionsTable anOptionsTable
#' @param aFileName aFileName
#'
#' @return
#' @export
#'
#' @examples
saveOptions <- function(anOptionsTable,
                        aFileName) {
    utils::write.table(anOptionsTable,
                       file = aFileName,
                       sep = "\t",
                       row.names = T,
                       col.names = NA,
                       quote = FALSE)
}



#' Title
#'
#' @param filename filename
#'
#' @return
#' @export
#'
#' @examples
loadOptions <- function(filename) {
    return(as.matrix(utils::read.table(file = filename,
                                       header = T,
                                       sep = "\t",
                                       row.names = 1)))
}


#' Title
#'
#' @param theScenarios theScenarios
#' @param file file name
#'
#' @return
#' @export
#'
#' @examples
saveScenarios <- function(theScenarios,
                          file) {
    utils::write.table(theScenarios,
                       file = file,
                       sep = "\t",
                       row.names = T,
                       col.names = NA)
}



#' Title
#'
#' A function to create a graph of a scenario as horizontal bar
#'
#' @param aScenario the scenario to graph
#' @param aTree the tree
#' @param isLabelY do we want the label of the option
#' @param isPar do we want the function to modify the par
#'
#' @return
#'
#' @importFrom withr defer
#'
#' @export
#'
#' @examples
showScenario <- function(aScenario,
                         aTree,
                         isLabelY = TRUE,
                         isPar = T) {
    if (isPar) {
        oldpar <- par(mgp = c(7,1,0), oma = c(0,20,0,0), cex = 0.5)
        withr::defer(par(oldpar))
    }

    # Determine the gray scale, we use grey.scale
    myGreyValue <- lapply(1:7, function(x) {grDevices::gray.colors(x, 0, 1) })
    myCol <- aTree@Leaves %>%
        sapply(function(x) {
            myGreyValue[[aTree@Nodes[[getID(aTree@Nodes, x)[1]]]@rangeScale]][aScenario[x, ]]
        }) %>%
        unlist()

    theMax <- aTree@Attributes %>%
        lapply(function(x) {
            aTree@Nodes[[getID(aTree@Nodes, x)[1]]]@rangeScale
        }) %>%
        unlist() %>%
        matrix(ncol = 1)

    mc <- graphics::barplot(as.vector(rev(aScenario)),
                            xlim = c(0, max(theMax[]) + 0.5),
                            ylab = "Indicators",
                            xlab = "Mark",
                            horiz = T,
                            col = rev(myCol))

    if (isLabelY) {
        graphics::axis(side = 2,
                       at = mc,
                       labels = rev(rownames(aScenario)),
                       las = 2,
                       cex = 0.5)
    }

    graphics::points(as.vector(rev(theMax)),
                     mc,
                     col = "black",
                     pch = "<")

    graphics::abline(v = c(1:max(theMax)), untf = FALSE, lty = 3)
}
