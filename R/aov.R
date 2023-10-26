#' Execution Time Estimation for Factorial Simulations
#'
#' Estimates the expected execution time for a given number of factorial
#' simulations using the duration taken to run a sample subset of simulations.
#'
#' @param tree \code{Tree} object upon which the simulations are performed.
#' @param test_runs \code{numeric} denoting the number of simulations utilized
#'   for estimating the execution time. Default value is set to \code{50}.
#'
#' @return No explicit return value. The function prints the calculated
#'   estimated execution time.
#'
#' @export
estimate_aov_time <- function(tree, test_runs = 50) {
  # Compute the total number of simulations based on tree leaves
  total_simulations <- tree@Nodes %>%
    sapply(function(node) {
      if (node@IsLeaf) node@RangeScale
    }) %>%
    unlist() %>%
    prod()

  # Generate sample scenarios
  sample_scenarios <- matrix(
    nrow = tree@NumberOfLeaves,
    ncol = test_runs
  )
  rownames(sample_scenarios) <- tree@Leaves

  sample_scenarios[] <- sapply(tree@Leaves, function(leaf) {
    node <- tree@Nodes[[get_id(tree@Nodes, leaf)[1]]]
    sample(node@RangeScale,
      size = test_runs,
      prob = node@Probability, replace = TRUE
    )
  })

  # Measure time taken for the subset of simulations
  start_time <- Sys.time()
  apply(sample_scenarios, 2, function(scenario) {
    evaluate_scenario(tree, as.matrix(scenario))
  })
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "mins")

  # Estimate total execution time
  estimated_time_minutes <- elapsed_time * total_simulations / test_runs
  cat(
    "\n", tree@NumberOfLeaves, " factors",
    "\n Approximate time to run the ", total_simulations,
    " modalities: ", estimated_time_minutes, " minutes"
  )
}


#' Dual Order AOV on a Decision Tree
#'
#' Conducts an Analysis of Variance (AOV) on a provided decision tree, computing
#' both first-order and second-order effects.
#'
#' @param tree \code{Tree} object to be analyzed.
#'
#' @importFrom stats aov as.formula
#'
#' @return A \code{list} containing results for both first-order and
#'   second-order AOV analyses.
#'
#' @export
aov_tree <- function(tree) {
  # Create the factorial plan
  factorial_plan <- create_factorial_plan(tree)

  # Simulate scenarios based on the plan
  results <- sapply(1:dim(factorial_plan)[2], function(x) {
    evaluate_scenario(tree, as.matrix(factorial_plan[, x]))
  })
  results <- results[c(tree@RootName, tree@Leaves), ]

  # Convert results matrix to data frame and factorize
  results_df <- as.data.frame(t(results))
  for (i in 1:tree@NumberOfLeaves) {
    results_df[[i + 1]] <- factor(results_df[[i + 1]])
  }

  # Abbreviate column names
  colnames(results_df) <- abbreviate(colnames(results_df),
    minlength = 4,
    dot = FALSE
  )

  # Generate AOV formulas
  formula_1 <- generate_aov_formula(results_df, order = 1)
  formula_2 <- generate_aov_formula(results_df, order = 2)

  # AOV for first-order effects
  aov_results_1 <- aov(formula_1, data = results_df)
  output <- list()
  output[[1]] <- round(compute_aov_total_sensitivity(aov_results_1), 3)
  output[[2]] <- round(compute_aov_sensitivity_effects(aov_results_1), 3)

  # AOV considering 2nd order interactions
  aov_results_2 <- aov(formula_2, data = results_df)
  output[[3]] <- round(compute_aov_total_sensitivity(aov_results_2), 3)
  output[[4]] <- round(compute_aov_sensitivity_effects(aov_results_2), 3)

  return(output)
}


#' Factorial Plan Generation for a Decision Tree
#'
#' Extracts the leaf indices from a provided decision tree and constructs a
#' factorial plan utilizing the \code{gen.factorial} function.
#'
#' @param tree \code{Tree} object to be analyzed.
#'
#' @importFrom AlgDesign gen.factorial
#'
#' @return A \code{matrix} detailing the generated factorial plan.
#'
#' @keywords internal
create_factorial_plan <- function(tree) {
  # Extract leaf indices from the tree
  leaf_indices <- tree@Nodes %>%
    sapply(function(node) {
      if (node@IsLeaf) {
        node@RangeScale
      }
    }) %>%
    unlist()

  # Generate factorial plan
  factorial_plan <- t(gen.factorial(as.numeric(leaf_indices), center = FALSE))
  rownames(factorial_plan) <- tree@Leaves

  return(factorial_plan)
}


#' Construct AOV Formula from Results Dataframe
#'
#' Builds an Analysis of Variance (AOV) formula based on a provided results
#' dataframe and the desired order of interaction.
#'
#' @param results_df \code{data.frame} containing the results for which an AOV
#'   formula is to be constructed.
#' @param order \code{numeric} value specifying the interaction order: 1 for
#'   first-order and 2 for second-order.
#'
#' @return A \code{formula} object suitable for AOV analysis.
#'
#' @keywords internal
generate_aov_formula <- function(results_df, order = 1) {
  # Construct AOV formula based on the order
  predictors <- paste(colnames(results_df)[2:ncol(results_df)], collapse = "+")
  if (order == 2) {
    formula_str <- paste(colnames(results_df)[1], "~", "(", predictors, ")^2")
  } else {
    formula_str <- paste(colnames(results_df)[1], "~", predictors)
  }

  return(as.formula(formula_str))
}


#' Compute Sensitivity Criteria for Model Terms
#'
#' Derives sensitivity criteria for each term present in a provided fitted
#' model.
#'
#' @param aov_obj An \code{aov} object resulting from a call to the \code{aov()}
#'   function.
#'
#' @return A \code{data.frame} detailing the sensitivity criteria for each model
#'   term. The data frame consists of the following columns:
#'   \itemize{
#'     \item \code{df}: degrees of freedom.
#'     \item \code{ss}: sum of squares.
#'     \item \code{ss.ratio}: ratio of the term's sum of squares to the model's
#'       total sum of squares.
#'     \item \code{cm}: mean square (or variance) associated with each term.
#'     \item \code{F}: F-statistic relevant to each term.
#'   }
#'   Entries are organized in descending order based on the \code{ss.ratio}
#'   values.
#'
#' @keywords internal
compute_aov_sensitivity_effects <- function(aov_obj) {
  # Ensure the object is of class 'aov'
  if (!inherits(aov_obj, "aov")) {
    stop("Provided object isn't of class 'aov'")
  }

  # ANOVA results extraction
  aov_summary <- summary(aov_obj)[[1]]
  sum_squares <- aov_summary[, "Sum Sq"]
  degrees_freedom <- aov_summary[, "Df"]
  mean_squares <- sum_squares / degrees_freedom

  # Total sum of squares, degrees of freedom, and mean squares
  total_ss <- sum(sum_squares)
  total_df <- sum(degrees_freedom)
  total_ms <- total_ss / total_df

  # Residual sum of squares, degrees of freedom, and mean squares
  residual_df <- aov_obj$df.residual
  if (residual_df > 0) {
    residual_ss <- sum_squares[length(sum_squares)]
    residual_ms <- residual_ss / residual_df
  } else {
    residual_ss <- NA
    residual_ms <- NA
  }

  # Calculate sensitivity criteria for each term
  output <- data.frame(
    df = degrees_freedom,
    ss = sum_squares,
    ss.ratio = sum_squares / total_ss,
    cm = mean_squares,
    F = mean_squares / residual_ms
  )

  rownames(output) <- rownames(aov_summary)
  output <- output[rev(order(output$ss.ratio)), ]

  return(output)
}


#' Compute Total Sensitivity Factors for Model Terms
#'
#' Derives the total sensitivity factors for each term present in a provided
#' fitted model.
#'
#' @param aov_obj An \code{aov} object resulting from a call to the \code{aov()}
#'   function.
#'
#' @return A \code{data.frame} detailing the total sensitivity factors for each
#'   model term.
#'
#' @keywords internal
compute_aov_total_sensitivity <- function(aov_obj) {
  # Ensure the object is of class 'aov'
  if (!inherits(aov_obj, "aov")) {
    stop("Provided object isn't of class 'aov'")
  }

  # Retrieve ANOVA results
  factor_indicator <- attr(aov_obj$terms, "factors")[-1, ]
  aov_summary <- summary(aov_obj)[[1]]
  sum_squares <- aov_summary[, "Sum Sq"]
  degrees_freedom <- aov_summary[, "Df"]
  mean_squares <- sum_squares / degrees_freedom

  # Calculate total and residual values
  total_ss <- sum(sum_squares)
  total_df <- sum(degrees_freedom)
  total_ms <- total_ss / total_df

  residual_df <- aov_obj$df.residual
  if (residual_df > 0) {
    residual_ss <- sum_squares[length(sum_squares)]
    residual_ms <- residual_ss / residual_df
  } else {
    residual_ss <- NA
    residual_ms <- NA
  }

  # Compute total sensitivity criteria
  if (residual_df > 0) {
    sum_squares <- sum_squares[-length(sum_squares)]
    degrees_freedom <- degrees_freedom[-length(degrees_freedom)]
  }

  total_sum_squares <- factor_indicator %*% sum_squares
  total_degrees_freedom <- factor_indicator %*% degrees_freedom
  total_mean_squares <- total_sum_squares / total_degrees_freedom

  # Main effects computation
  main_filter <- apply(factor_indicator, 2, sum) == 1
  main_sum_squares <- factor_indicator[, main_filter] %*% sum_squares[main_filter]

  # Prepare output
  output <- data.frame(
    df = total_degrees_freedom,
    ss = total_sum_squares,
    ss.ratio = total_sum_squares / total_ss,
    main.ss.ratio = main_sum_squares / total_ss,
    cm = total_mean_squares,
    F = total_mean_squares / residual_ms
  )

  output <- output[rev(order(output$ss.ratio)), ]

  return(output)
}


#' Visualize AOV Outcomes
#'
#' Renders the outcomes of an Analysis of Variance (AOV) through bar plots,
#' allowing a comprehensive display of both total sums and specific effects.
#'
#' @param aov_results A \code{list} containing the AOV results.
#' @param show_main \code{logical} indicating if the main effects and total sums
#'   of squares should be included in the visualization. Default is \code{TRUE}.
#' @param num_plots \code{numeric} specifying the count of effects to be
#'   showcased in the display.
#' @param horizontal \code{logical} determining if the bar plots should be
#'   oriented horizontally. Default is \code{TRUE}.
#' @param axis_label_style \code{numeric} designating the label styling for the
#'   plot's axes. Default is 1.
#' @param ... Additional arguments affecting the bar plot's aesthetics.
#'
#' @importFrom grDevices heat.colors
#'
#' @return A \code{data.frame} containing proportions derived from the sum of
#'   squares.
#'
#' @export
visualize_aov <- function(aov_results,
                                  show_main = TRUE,
                                  num_plots = 8,
                                  horizontal = TRUE,
                                  axis_label_style = 1,
                                  ...) {
  # Setting up the plotting environment
  old_par <- par(mfrow = c(2, 2))
  base::on.exit(par(old_par), add = TRUE)

  # Create barplots for each result set in the AOV results
  create_aov_barplot(
    data = aov_results[[1]], is_effect = FALSE,
    horizontal = horizontal, num_plots = num_plots,
    show_main = show_main, axis_label_style = axis_label_style,
    ...
  )
  create_aov_barplot(
    data = aov_results[[2]], is_effect = TRUE,
    horizontal = horizontal, num_plots = num_plots,
    show_main = show_main, axis_label_style = axis_label_style,
    ...
  )
  create_aov_barplot(
    data = aov_results[[3]], is_effect = FALSE,
    horizontal = horizontal, num_plots = num_plots,
    show_main = show_main, axis_label_style = axis_label_style,
    ...
  )
  create_aov_barplot(
    data = aov_results[[4]], is_effect = TRUE,
    horizontal = horizontal, num_plots = num_plots,
    show_main = show_main, axis_label_style = axis_label_style,
    ...
  )

  # Return the final result set
  return(aov_results[[4]])
}

#' AOV Bar Plot Generator
#'
#' Constructs bar plots to illustrate the outcomes of an Analysis of Variance
#' (AOV). Tailored to visualize both the total sum of squares and specific
#' effects.
#'
#' @param data A \code{data.frame} containing the AOV results designated for
#'   plotting.
#' @param is_effect \code{logical} signifying whether the provided data
#'   showcases effects (if \code{TRUE}) or totals (if \code{FALSE}).
#' @param horizontal \code{logical} indicating the orientation of the bar plots.
#'   If \code{TRUE}, plots are displayed horizontally; otherwise, vertically.
#' @param num_plots \code{numeric} dictating the count of effects to be
#'   presented in the plot.
#' @param show_main \code{logical} stating if the main effects and total sums of
#'   squares should be amalgamated and exhibited. Relevant only when `is_effect`
#'   is \code{FALSE}.
#' @param axis_label_style \code{numeric} defining the label styling for the
#'   plot axes.
#' @param ... Supplementary arguments relayed to the `barplot` function for
#'   aesthetic adjustments.
#'
#' @keywords internal
create_aov_barplot <- function(data,
                               is_effect,
                               horizontal,
                               num_plots,
                               show_main,
                               axis_label_style,
                               ...) {
  # Selects either all rows or a subset based on `num_plots` and whether we are
  # dealing with effects or not. If dealing with effects, it takes the minimum
  # of `num_plots` and the number of rows in the data. Otherwise, it takes all
  # rows.
  selected <- if (is_effect) min(num_plots, nrow(data)) else nrow(data)

  # Reverses the order of the rows based on `selected` to ensure the largest
  # effects (or totals) are displayed at the top.
  data <- data[rev(1:(selected)), ]

  # Removes spaces from the row names, which usually represent variable names or
  # effects in AOV.
  names <- gsub(" ", "", rownames(data))

  # Depending on the combination of flags (`show_main` and `is_effect`),
  # the function decides which type of bar plot visualization to generate.

  # If `show_main` is TRUE and not dealing with effects, it visualizes both main
  # effects and total sums of squares side by side.
  if (show_main && !is_effect) {
    data_subset <- rbind(data$main.ss.ratio, data$ss.ratio - data$main.ss.ratio)
    barplot(data_subset,
      horiz = horizontal, names.arg = names,
      main = "Main-effect and total Sums of Squares",
      col = heat.colors(2), beside = FALSE, las = axis_label_style, ...
    )

    # If dealing with effects, it visualizes the proportional sums of squares.
    # It also provides a cumulated sum of squares for reference.
  } else if (is_effect) {
    ss_cumulated <- rev(cumsum(rev(data$ss.ratio)))
    data_subset <- if (horizontal) {
      rbind(data$ss.ratio, ss_cumulated)
    } else {
      rbind(data$ss.ratio, ss_cumulated - data$ss.ratio)
    }
    barplot(data_subset,
      horiz = horizontal, names.arg = names,
      main = "Sums of Squares proportions",
      col = heat.colors(2), beside = horizontal,
      las = axis_label_style, ...
    )

    # If not dealing with main effects or specific effects, it visualizes the
    # total sums of squares.
  } else {
    barplot(data$ss.ratio,
      horiz = horizontal, names.arg = names,
      main = "Total Sums of Squares", las = axis_label_style, ...
    )
  }
}
