#' Estimates Execution Time for Monte Carlo Simulations
#'
#' Estimates the time required to run a set number of Monte Carlo simulations
#' based on the time taken to run a smaller test set.
#'
#' @param tree A \code{Tree} object for simulations.
#' @param num_runs A \code{Numeric}, total number of simulations desired.
#' @param num_test A \code{Numeric}, number of test simulations for time
#'   estimation (default is 50).
#'
#' @return A \code{character} string with estimated execution time (in minutes)
#'
#' @examples
#' tree <- dexisensitivity::masc2
#' estimate_mc_time(tree, num_runs = 1000, num_test = 50)
#'
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

  return(paste0("Estimated time for ",
                num_runs,
                " simulations: ",
                round(estimated_minutes, 2),
                " minutes"))
}


#' Monte Carlo Simulation on a Decision Tree
#'
#' Conducts a Monte Carlo simulation over a provided decision tree for a
#' specified number of runs. Optionally, the function can save the random
#' options selected for the analysis in a .csv file named "MC options.csv".
#'
#' @param tree \code{Tree} object to be used in the simulation.
#' @param num_runs \code{numeric} indicating the number of Monte Carlo
#'   simulations to be executed.
#' @param write_to_file \code{character} Name of the file created to save the
#'   Monte Carlo's results. If \code{NULL}, don't write any file. Default is
#'   \code{NULL}
#'
#' @return A \code{matrix} containing the results of the Monte Carlo simulation.
#'
#' @examples
#' tree <- dexisensitivity::masc2
#' MC <- monte_carlo(tree, 100)
#'
#' @export
monte_carlo <- function(tree, num_runs, write_to_file = NULL) {

  message(paste0("Starting simulation at ", date()))

  # Use create_options function to generate the options for the simulation
  options_matrix <- create_options(tree, num_options = num_runs)

  # Perform the Monte Carlo simulation
  simulation_results <- 1:num_runs |>
    sapply(function(x) {
      evaluate_scenario(tree, as.matrix(options_matrix[, x]))
    })

  # Save options to a file if required
  if (!is.null(write_to_file)) {
    write.table(options_matrix,
      file = paste0(write_to_file, ".csv"),
      sep = ",", row.names = TRUE, col.names = NA
    )
  }

  message(paste0("Simulation finished at ", date()))

  return(simulation_results)
}



#' Visualization of Monte Carlo Simulation Results
#'
#' Displays the outcomes of the Monte Carlo simulation for a specific
#' \code{Node} as a bar chart, showcasing the frequency of each result.
#' Furthermore, the lengths of the bars are saved in a .csv file titled "MC bar
#' lengths.csv".
#'
#' @param node \code{Node} object representing the node of interest in the
#'   simulation.
#' @param mc_results \code{matrix} containing the Monte Carlo simulation
#'   results.
#' @param num_runs \code{numeric} indicating the number of Monte Carlo
#'   simulations that were executed.
#' @param save \code{character} indicating where to save the graphic. By default,
#'   save is NULL and don't save the graphic.
#'
#' @return A \code{vector} depicting the data used in the bar chart.
#'
#' @importFrom graphics legend text
#'
#' @examples
#' tree <- dexisensitivity::masc2
#' MC <- monte_carlo(tree, 100)
#' show_mc_results(tree@Nodes[[2]], MC, 100)
#'
#' @export
show_mc_results <- function(node, mc_results, num_runs, save = NULL) {
  node_type <- ifelse(node@IsLeaf, "L", "A")

  bar_data <- mc_results[node@Name, ] |> table()

  # Fill missing bars with zeros
  if (length(bar_data) < node@RangeScale) {
    bar_data <- factor(mc_results[node@Name, ],levels=1:node@RangeScale) |> table()
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
  if(!is.null(save)) {
    stopifnot("save should be a character" = is.character(save))
    cat(
      row.names = node@Name,
      bar_data,
      file = save,
      sep = ",", fill = TRUE, append = TRUE
    )
  }

  return(bar_data)
}
