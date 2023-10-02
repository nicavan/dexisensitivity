#' Estimates execution time for Monte Carlo simulations
#'
#' Estimates the time required to run a set number of Monte Carlo simulations
#' based on the time taken to run a smaller test set.
#'
#' @param tree Decision tree for simulations.
#' @param num_runs Total number of simulations desired.
#' @param num_test Number of test simulations for time estimation (default is
#'   50).
#'
#' @return No explicit return. The function prints the estimated execution time.
#' @export
estimate_mc_time <- function(tree, num_runs, num_test = 50) {

  # Generate sample options for test simulations using create_options function
  options_matrix <- create_options(tree, num_options = num_test, seed = NULL)

  # Run test simulations
  start_time <- Sys.time()
  sapply(1:num_test, function(x) {
    evaluate_scenario(tree, as.matrix(options_matrix[, x]))
  })
  end_time <- Sys.time()

  # Estimate and print the required time for desired simulations
  estimated_minutes <- as.numeric(difftime(end_time, start_time, units = "mins")) * num_runs / num_test
  cat("\n Estimated time for", num_runs,
      "simulations:", round(estimated_minutes, 2), "minutes\n")
}


#' Performs a Monte Carlo simulation
#'
#' Conducts a Monte Carlo simulation on a given decision tree with a specified
#' number of runs. If `isFile` is set to TRUE, a .csv file named "MC
#' options.csv" is saved, which contains all the random options selected for the
#' Monte Carlo analysis.
#'
#' @param aTree Decision tree to run the simulation on
#' @param nbRuns Number of Monte Carlo simulations to perform
#' @param isFile Logical value to decide whether to write a file or not
#' @param verbose Logical value for printing additional information
#'
#' @return Returns a matrix representing the Monte Carlo simulations
#'
#' @export
MonteCarlo <- function(aTree,
                       nbRuns,
                       isFile = F,
                       verbose = T) {
  if (verbose) {
    cat("\n Time in: ", date())
  }

  MC <- matrix(ncol = nbRuns,
               nrow = aTree@NumberOfAttributes,
               dimnames = list(c(aTree@Attributes), c(seq(1:nbRuns))))
  option <- matrix(nrow = aTree@NumberOfLeaves,
                   ncol = nbRuns)
  rownames(option) <- aTree@Leaves

  for(k in aTree@Leaves) {
    option[k, ] <- aTree@Nodes[[get_id(aTree@Nodes, k)[1]]]@RangeScale %>%
      sample(size = nbRuns,
             prob = aTree@Nodes[[get_id(aTree@Nodes, k)[1]]]@Probability,
             replace = TRUE)
  }

  MC <- 1:nbRuns %>%
    sapply(function(x) {evaluate_scenario(aTree,
                                          as.matrix(option[, x]))})

  if (isFile) {
    # Write a file that contain all the random options selected for the MC analysis
    write.table(option,
                file = "MC options.csv",
                sep = ",", row.names = T, col.names = NA)
  }

  if (verbose) {
    cat("\n Time out: ",date())
  }
  return(MC)
}



#' Displays the Monte Carlo simulation results
#'
#' This function generates a bar chart of the frequency of each outcome in the
#' Monte Carlo simulation results for a given node. It also writes the bar
#' lengths to a .csv file.
#'
#' @param Node Node of interest
#' @param MC Monte Carlo simulation results
#' @param nbRuns Number of Monte Carlo simulations performed
#'
#' @return Returns the bar chart data
#'
#' @importFrom graphics legend text
#'
#' @export
ShowMC <- function(Node,
                   MC,
                   nbRuns) {

  #  Node<-theTree@Nodes[[nodeName]]
  typ <- "A"
  if (Node@IsLeaf) { typ <- "L" }

  bar <- MC[Node@Name, ] %>%
    tapply(MC[Node@Name, ], sum)

  if (length(bar) < Node@RangeScale) {
    newbar <- array(data = 0,
                    dim = Node@RangeScale,
                    dimnames = list(c(seq(1:Node@RangeScale))))
    for(i in 1:dim(bar)) {
      newbar[names(bar)[i]] <- bar[i]
    }
    bar <- newbar
  }

  bar[] <- (bar[] / c(1:Node@RangeScale)) / nbRuns
  mc <- barplot(bar,
                main = paste(Node@Name, " [", typ, "]", sep = ""),
                xlab = "Modalities",
                ylab = "Frequencies",
                ylim = c(0, (max(bar) + 0.1)),
                las = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 0.9)
  #,names.arg=Node@ScaleLabel

  text(mc, bar,
       format(round(bar, digits = 2)),
       xpd = T, cex = 0.8, pos = 3)

  #  ref_points<-as.matrix(read.table(file="MC bar lengths_ref.csv",sep=",",row.names=1))
  #  points(mc,as.vector(na.omit((ref_points))),pch=18,col=2)

  legend("topright",
         legend = paste(names(bar), abbreviate(Node@ScaleLabel)),
         box.lty = 0, cex =0.8)
  text(length(bar))

  #write.table(bar,file="MC bar lengths.csv",sep=",",row.names=T,col.names=NA)
  cat(row.names = Node@Name,
      bar,
      file = "MC bar lengths.csv",
      sep = ",", fill = T, append = T)
  return(bar)
}
