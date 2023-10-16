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
  cat(
    "\n Estimated time for", num_runs,
    "simulations:", round(estimated_minutes, 2), "minutes\n"
  )
}


#' Performs a Monte Carlo simulation
#'
#' Conducts a Monte Carlo simulation on a given decision tree with a specified
#' number of runs. If `write_to_file` is set to TRUE, a .csv file named "MC
#' options.csv" is saved, containing the random options selected for the Monte Carlo analysis.
#'
#' @param tree Decision tree for the simulation.
#' @param num_runs Number of Monte Carlo simulations to conduct.
#' @param write_to_file Logical determining if results should be saved to a file.
#' @param verbose Logical determining if additional information should be printed.
#'
#' @return A matrix representing the Monte Carlo simulation results.
#'
#' @export
monte_carlo <- function(tree, num_runs, write_to_file = FALSE, verbose = TRUE) {
  if (verbose) {
    cat("\n Starting simulation at: ", date())
  }

  # Use create_options function to generate the options for the simulation
  options_matrix <- create_options(tree, num_options = num_runs)

  # Perform the Monte Carlo simulation
  simulation_results <- 1:num_runs %>%
    sapply(function(x) {
      evaluate_scenario(tree, as.matrix(options_matrix[, x]))
    })

  # Save options to a file if required
  if (write_to_file) {
    write.table(options_matrix,
      file = "MC options.csv",
      sep = ",", row.names = TRUE, col.names = NA
    )
  }

  if (verbose) {
    cat("\n Simulation finished at: ", date())
  }

  return(simulation_results)
}



#' Displays the Monte Carlo simulation results
#'
#' Visualizes the Monte Carlo simulation outcomes for a given node as a bar chart,
#' depicting the frequency of each outcome. Additionally, the bar lengths are saved
#' to a .csv file named "MC bar lengths.csv".
#'
#' @param node Node of interest.
#' @param mc_results Monte Carlo simulation results matrix.
#' @param num_runs Number of Monte Carlo simulations conducted.
#'
#' @return A vector representing the bar chart data.
#'
#' @importFrom graphics legend text
#'
#' @export
show_mc_results <- function(node, mc_results, num_runs) {
  node_type <- ifelse(node@IsLeaf, "L", "A")

  bar_data <- mc_results[node@Name, ] %>% table()

  # Fill missing bars with zeros
  if (length(bar_data) < node@RangeScale) {
    bar_data <- bar_data + numeric(node@RangeScale)
  }

  # Normalize bar data
  bar_data <- (bar_data) / num_runs

  # Plot the bar chart
  bar_positions <- barplot(
    bar_data,
    main = paste(node@Name, " [", node_type, "]", sep = ""),
    xlab = "Modalities",
    ylab = "Frequencies",
    ylim = c(0, max(bar_data) + 0.1),
    las = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 0.9
  )

  text(bar_positions, bar_data,
    format(round(bar_data, 2)),
    xpd = TRUE, cex = 0.8, pos = 3
  )

  legend(
    "topright",
    legend = paste(names(bar_data), abbreviate(node@ScaleLabel)),
    box.lty = 0, cex = 0.8
  )

  # Save the bar lengths to a .csv file
  cat(
    row.names = node@Name,
    bar_data,
    file = "MC bar lengths.csv",
    sep = ",", fill = TRUE, append = TRUE
  )

  return(bar_data)
}
