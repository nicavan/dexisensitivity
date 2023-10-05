#' Estimates the execution time for factorial simulations
#'
#' Estimates the execution time for a specified number of factorial simulations
#' based on the time taken to run a subset of simulations.
#'
#' @param tree The Tree object on which simulations are run.
#' @param test_runs Number of simulations to be used for time estimation.
#'   Default is 50.
#'
#' @return No explicit return. Prints the estimated execution time.
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
  sample_scenarios <- matrix(nrow = tree@NumberOfLeaves,
                             ncol = test_runs)
  rownames(sample_scenarios) <- tree@Leaves

  sample_scenarios[] <- sapply(tree@Leaves, function(leaf) {
    node <- tree@Nodes[[get_id(tree@Nodes, leaf)[1]]]
    sample(node@RangeScale, size = test_runs,
           prob = node@Probability, replace = TRUE)
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
  cat("\n", tree@NumberOfLeaves, " factors",
      "\n Approximate time to run the ", total_simulations, " modalities: ",
      estimated_time_minutes, " minutes")
}


#' Calculate AOV Results
#'
#' Provides the results of an Analysis of Variance (AOV) based on a given tree.
#'
#' @param tree Tree object for analysis.
#'
#' @importFrom stats aov as.formula
#'
#' @return A list containing AOV results.
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
    results_df[[i+1]] <- factor(results_df[[i+1]])
  }

  # Abbreviate column names
  colnames(results_df) <- abbreviate(colnames(results_df),
                                     minlength = 4, dot = FALSE)

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


#' Create Factorial Plan from Tree
#'
#' Extracts leaf indices from the tree and creates the factorial plan based on
#' gen.factorial function.
#'
#' @param tree Tree object for analysis.
#'
#' @importFrom AlgDesign gen.factorial
#'
#' @return A matrix representing the factorial plan.
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


#' Generate AOV formula
#'
#' Constructs an Analysis of Variance (AOV) formula based on the results
#' dataframe.
#'
#' @param results_df Data frame containing AOV results.
#' @param order Integer representing the order of interaction; 1 for
#'   first-order, 2 for second-order.
#'
#' @return A formula object for AOV.
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


#' Calculate sensitivity criteria for model terms
#'
#' Provides sensitivity criteria for each term in a fitted model.
#'
#' @param aov_obj An object of class `aov` from a call to `aov()`.
#'
#' @return A data frame with sensitivity criteria for each term in the model.
#'   The columns of the data frame include:
#'   - `df`: degrees of freedom
#'   - `ss`: sum of squares
#'   - `ss.ratio`: ratio of sum of squares to total sum of squares
#'   - `cm`: mean square (or variance) for each term
#'   - `F`: F-statistic for each term
#'
#'   Rows are ordered in decreasing order of ss.ratio.
compute_aov_sensitivity_effects <- function(aov_obj) {
  # Ensure the object is of class 'aov'
  if(!inherits(aov_obj, "aov")) {
    stop("Provided object isn't of class 'aov'")
  }

  # ANOVA results extraction
  aov_summary <- summary(aov_obj)[[1]]
  sum_squares <- aov_summary[, "Sum Sq"]
  degrees_freedom <- aov_summary[, "Df"]
  mean_squares <- sum_squares/degrees_freedom

  # Total sum of squares, degrees of freedom, and mean squares
  total_ss <- sum(sum_squares)
  total_df <- sum(degrees_freedom)
  total_ms <- total_ss/total_df

  # Residual sum of squares, degrees of freedom, and mean squares
  residual_df <- aov_obj$df.residual
  if (residual_df > 0) {
    residual_ss <- sum_squares[length(sum_squares)]
    residual_ms <- residual_ss/residual_df
  } else {
    residual_ss <- NA
    residual_ms <- NA
  }

  # Calculate sensitivity criteria for each term
  output <- data.frame(df = degrees_freedom, ss = sum_squares,
                       ss.ratio = sum_squares/total_ss, cm = mean_squares,
                       F = mean_squares/residual_ms)

  rownames(output) <- rownames(aov_summary)
  output <- output[rev(order(output$ss.ratio)), ]

  return(output)
}


#' Calculate total Sensitivity Factors for Model Terms
#'
#' Computes total sensitivity factors for each term in a fitted model.
#'
#' @param aov_obj An object of class `aov` from a call to `aov()`.
#'
#' @return A data frame with total sensitivity factors for each term in the
#'   model.
compute_aov_total_sensitivity <- function(aov_obj) {

  # Ensure the object is of class 'aov'
  if(!inherits(aov_obj, "aov")) {
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


#' Visualize AOV results
#'
#' Visualizes the results of an Analysis of Variance (AOV). It provides both a
#' complete total and effect visualization using bar plots.
#'
#' @param aov_results A list containing the AOV results.
#' @param show_main Logical, if TRUE, main effects and total sums of squares are
#'   displayed in the plot. Defaults to TRUE.
#' @param num_plots The number of effects to display.
#' @param horizontal Logical, if TRUE, the bar plot is displayed horizontally.
#'   Defaults to TRUE.
#' @param axis_label_style The style of axis labels on the bar plots. Default is
#'   1.
#' @param ... Additional arguments to control the appearance of the bar plots.
#'
#' @importFrom grDevices heat.colors
#'
#' @return A data frame with the summed square proportions.
#'
#' @export
visualize_aov_results <- function(aov_results,
                                  show_main = TRUE,
                                  num_plots = 8,
                                  horizontal = TRUE,
                                  axis_label_style = 1,
                                  ...) {



  # Setting up the plotting environment
  par(mfrow = c(2, 2))

  # Create barplots for each result set in the AOV results
  create_aov_barplot(data = aov_results[[1]], is_effect = FALSE,
                     horizontal = horizontal, num_plots = num_plots,
                     show_main = show_main, axis_label_style = axis_label_style,
                     ...)
  create_aov_barplot(data = aov_results[[2]], is_effect = TRUE,
                     horizontal = horizontal,num_plots = num_plots,
                     show_main = show_main, axis_label_style = axis_label_style,
                     ...)
  create_aov_barplot(data = aov_results[[3]], is_effect = FALSE,
                     horizontal = horizontal, num_plots = num_plots,
                     show_main = show_main, axis_label_style = axis_label_style,
                     ...)
  create_aov_barplot(data = aov_results[[4]], is_effect = TRUE,
                     horizontal = horizontal, num_plots = num_plots,
                     show_main = show_main, axis_label_style = axis_label_style,
                     ...)

  # Return the final result set
  return(aov_results[[4]])
}

#' Create AOV Bar Plot
#'
#' Helper function specifically designed to create bar plots to visualize the
#' results of an AOV (Analysis of Variance). It has the flexibility to handle
#' both total sums of squares and effect visualizations.
#'
#' @param data A data frame containing the AOV results to be plotted.
#' @param is_effect A logical flag. If TRUE, indicates that the data represents
#'   effects; otherwise, it represents totals.
#' @param horizontal A logical flag. If TRUE, the bar plots will be oriented
#'   horizontally; otherwise, they will be vertical.
#' @param num_plots An integer specifying the number of effects to display in
#'   the bar plot.
#' @param show_main A logical flag. If TRUE, the main effects and total sums of
#'   squares are combined and displayed. This is effective only if `is_effect`
#'   is FALSE.
#' @param axis_label_style An integer representing the style of the axis labels
#'   on the bar plots.
#' @param ... Additional arguments that will be passed to the `barplot` function
#'   to control its appearance.
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
    barplot(data_subset, horiz = horizontal, names.arg = names,
            main = "Main-effect and total Sums of Squares",
            col = heat.colors(2), beside = FALSE, las = axis_label_style, ...)

    # If dealing with effects, it visualizes the proportional sums of squares.
    # It also provides a cumulated sum of squares for reference.
  } else if (is_effect) {
    ss_cumulated <- rev(cumsum(rev(data$ss.ratio)))
    data_subset <- if (horizontal) {
      rbind(data$ss.ratio, ss_cumulated)
    } else {
      rbind(data$ss.ratio, ss_cumulated - data$ss.ratio)
    }
    barplot(data_subset, horiz = horizontal, names.arg = names,
            main = "Sums of Squares proportions",
            col = heat.colors(2), beside = horizontal,
            las = axis_label_style, ...)

    # If not dealing with main effects or specific effects, it visualizes the
    # total sums of squares.
  } else {
    barplot(data$ss.ratio, horiz = horizontal, names.arg = names,
            main = "Total Sums of Squares", las = axis_label_style, ...)
  }
}





