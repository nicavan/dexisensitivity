#' Create a Synoptic Plot from a Given Tree Structure
#'
#' Generates a synoptic plot based on a specified tree structure, providing
#' visualization of its structure and evaluation options.
#'
#' @param tree \code{Tree} structure object.
#' @param option \code{list} containing evaluation options for the tree.
#' @param depth \code{numeric} representing the depth of the tree; if provided,
#'   produces a sub-tree.
#'
#' @return A \code{ggplot} object representing the synoptic plot.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=1, seed = 42)
#' create_synoptique(tree, option)
#'
#' @export
create_synoptique <- function(tree, option, depth = NA) {
  # Determine if a subtree is required based on depth
  if (!is.na(depth) && depth != tree@RootName) {
    tree <- create_sub_tree(tree, depth)
  }

  # Create dataframe for plotting
  df <- create_data_frame_for_plotting(tree)

  # Evaluate scenarios and add labels
  df <- evaluate_and_label(df, tree, option)

  # Get coordinates for all boxes
  df2 <- get_box_coordinates(df)

  # Create synoptic plot
  p <- create_plot(df2)

  return(p)
}


#' Produce Dataframe Suitable for Plotting from Tree Structure
#'
#' Transforms a given tree structure into a dataframe, which is organized
#' and formatted in a manner that makes it suitable for plotting purposes.
#'
#' @param tree \code{Tree} structure object.
#'
#' @return \code{data.frame} that is structured for plotting.
#'
#' @keywords internal
create_data_frame_for_plotting <- function(tree) {
  # Initial data extraction
  df <- data.frame(attribut = tree@Attributes)

  # Check and assign leaf node status
  df$isleaf <- lapply(1:tree@NumberOfAttributes, function(x) {
    tree@Nodes[[x]]@IsLeaf
  }) |>
    unlist()

  # Assign depth and size attributes
  df$depth <- lapply(1:tree@NumberOfAttributes, function(x) {
    if(df[x,"isleaf"]){
      tree@Nodes[[x]]@Depth:tree@Depth
    }else{
      tree@Nodes[[x]]@Depth
    }
  })

  df$taille <- lapply(1:tree@NumberOfAttributes, function(x) {
    if (df[x, "isleaf"]) {
      1
    } else {
      subtree <- create_sub_tree(tree, tree@Nodes[[x]]@Name)
      # this sum is greater than subtree@NumberOfLeaves if there are repeted leaves
      sum(subtree@Attributes %in% subtree@Leaves) +
        sum(subtree@Attributes %in% subtree@LeafAggregated) - length(subtree@LeafAggregated)
    }
  }) |>
    unlist()

  return(df)
}


#' Evaluate Tree Node and Assign Labels
#'
#' Evaluates each node in the given tree based on the provided options and
#' then assigns appropriate labels to them.
#'
#' @param df \code{data.frame} that originates from the tree.
#' @param tree \code{Tree} structure object.
#' @param option Evaluation options for the tree.
#'
#' @return \code{data.frame} updated with evaluations and labels.
#'
#' @keywords internal
evaluate_and_label <- function(df, tree, option) {
  # Evaluation based on scenario and range scaling
  df$eval <- evaluate_scenario(tree, option)
  df$rangecol <- lapply(1:tree@NumberOfAttributes, function(x) {
    tree@Nodes[[x]]@RangeScale
  }) |>
    unlist()

  # Compose labels
  df$label <- paste0(df$attribut, " (", df$eval, "/", df$rangecol, ")")

  return(df)
}


#' Extract Box Coordinates for Plotting
#'
#' Extracts the necessary coordinates from the data frame to plot boxes
#' representing the nodes of a decision tree. The function considers the depth
#' and size attributes to determine the positions and dimensions of the boxes.
#'
#' @param df \code{data.frame} with depth and size attributes from the tree.
#'
#' @return \code{data.frame} enhanced with box plotting coordinates, which
#'   includes information about box position, dimensions, and labels.
#'
#' @keywords internal
get_box_coordinates <- function(df) {
  # Calculate box coordinates
  df2 <- lapply(1:max(unlist(df$depth)), function(x) {
    listtemp <- list()

    for (i in 1:dim(df)[1]) {
      if (x %in% unlist(df[i, "depth"][[1]])) {
        listtemp[[i]] <- df[i, ]
      }
    }

    dftemp <- bind_rows(listtemp)

    dftemp$ymin <- cumsum(lag(dftemp$taille, default = 0))
    dftemp$ymax <- cumsum(dftemp$taille)
    dftemp$xmin <- lapply(dftemp$depth, min) |> unlist()
    dftemp$xmax <- lapply(dftemp$depth, max) |> unlist() + 1

    return(dftemp)
  }) |> bind_rows()

  # Normalize evaluation and determine label and rectangle dimensions
  df2$norm_eval <- (df2$eval - 1) / (df2$rangecol - 1)
  df2$label_length <- nchar(df2$label)
  df2$rect_width <- df2$xmax - df2$xmin
  df2$rect_height <- df2$ymax - df2$ymin
  df2$mean_x <- (df2$xmin + df2$xmax) / 2
  df2$mean_y <- (df2$ymin + df2$ymax) / 2

  return(df2)
}


#' Create Synoptic Plot from Data Frame
#'
#' Constructs a synoptic plot visualizing nodes of a decision tree based on the
#' plotting details provided in a data frame. The plot is color-coded based on
#' normalized evaluations, with rectangles representing tree nodes and labels
#' indicating node attributes.
#'
#' @param df2 \code{data.frame} containing plotting details, which should
#'   include coordinates for boxes, normalized evaluation values, and labels.
#'
#' @return A \code{ggplot} object depicting the synoptic plot of the decision
#'   tree.
#'
#' @keywords internal
create_plot <- function(df2) {
  # Design synoptic plot
  p <- ggplot(data = df2) +
    geom_rect(
      aes_string(
        xmin = "xmin", xmax = "xmax",
        ymin = "ymin", ymax = "ymax",
        fill = "norm_eval"
      ),
      color = "black", size = 0.5
    ) +
    geom_text(aes_string(x = "mean_x", y = "mean_y", label = "label"),
      hjust = 0.5, vjust = 0.5, color = "black", size = 3
    ) +
    scale_fill_gradient2(
      low = "red", mid = "yellow", high = "green",
      name = "Normed Evaluation", midpoint = 0.5
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom"
    ) +
    scale_y_reverse(breaks = 1:7, labels = paste("Leaf", 1:7)) +
    scale_size_continuous(range = c(3, 6), guide = "none")

  return(p)
}

### Previous version
## Kept to get last state of test to implement labels adjustments
# create_synoptique <- function(tree, option, depth = NA) {
#
#   if (!is.na(depth) && depth != tree@RootName) {
#     tree <- create_sub_tree(tree, depth)
#   }
#
#
#   ### create data_frame used for plot values
#   # attribut column
#   df <- data.frame(attribut = tree@Attributes)
#
#   # isleaf column (needed for construction)
#   df$isleaf <- lapply(df$attribut, function(x) {
#     tree@Nodes[[which(tree@Attributes == x)]]@IsLeaf
#   }) |>
#     unlist()
#
#   # depth column
#   df$depth <- lapply(df$attribut, function(x) {
#     if (df[df$attribut==x, "isleaf"]) {
#       tree@Nodes[[which(tree@Attributes == x)]]@Depth:tree@Depth
#     } else {
#       tree@Nodes[[which(tree@Attributes == x)]]@Depth
#     }
#   })
#
#   # taille column
#   df$taille <- lapply(df$attribut, function(x) {
#     if (df[df$attribut==x, "isleaf"]) {
#       1
#     } else {
#       subtree <- create_sub_tree(tree, x)
#       length(subtree@Leaves)
#     }
#   }) |>
#     unlist()
#
#   # columns used for colors
#   df$eval <- evaluate_scenario(tree, option)
#   df$rangecol <- lapply(df$attribut, function(x) {
#     tree@Nodes[[which(tree@Attributes == x)]]@RangeScale
#   }) |>
#     unlist()
#
#   # labels column
#   df$label <- paste0(df$attribut, " (", df$eval, "/", df$rangecol, ")")
#
#
#   ### get all box coordinates
#   df2 <- lapply(1:max(unlist(df$depth)), function(x) {
#     listtemp <- list()
#     for(i in 1:dim(df)[1]) {
#       if (x %in% unlist(df[i, "depth"][[1]])) {
#         listtemp[[i]] <- df[i, ]
#       }
#     }
#
#     dftemp <- bind_rows(listtemp)
#
#     dftemp$ymin <- cumsum(lag(dftemp$taille, default = 0))
#     dftemp$ymax <- cumsum(dftemp$taille)
#     dftemp$xmin <- lapply(dftemp$depth, min) |> unlist()
#     dftemp$xmax <- lapply(dftemp$depth, max) |> unlist() + 1
#
#     return(dftemp)
#   }) |> bind_rows()
#
#   # get normed color
#   df2$norm_eval <- (df2$eval-1) / (df2$rangecol-1)
#
#   # Scale text with x et y
#   #
#   df2$label_length <- nchar(df2$label)
#
#   #
#   df2$rect_width <- df2$xmax - df2$xmin
#   df2$rect_height <- df2$ymax - df2$ymin
#
#   # for (i in 1:dim(df2)[1]) {
#   #   max_chars <- 20*df2[i, "rect_width"]
#   #   wrapped_text <- strwrap(df2[i, "label"], width = max_chars, simplify = FALSE)
#   #   # df2[i, "adjusted_label"] <- sapply(wrapped_text, function(txt) paste(txt, collapse = "\n"))
#   #   #
#   #   # df2[i, "text_scale"] <- min(with(df2[i, ],1 + 5 * (rect_width/max(length(wrapped_text[[1]]))) * (1 - label_length/max(label_length))),
#   #   #                             0.5*df2[i, ]$rect_height)
#   # }
#
#   # adjust_label <- function(label, rect_width) {
#   #
#   #   max_chars <- 20*rect_width # C'est un paramètre que vous pouvez ajuster selon vos besoins
#   #   wrapped_text <- strwrap(label, width = max_chars, simplify = FALSE)
#   #   paste(wrapped_text, collapse = "\n")
#   #   wrapped_text <- sapply(wrapped_text, function(txt) paste(txt, collapse = "\n"))
#   # }
#   # df2$adjusted_label <- mapply(adjust_label, df2$label, df2$rect_width)
#
#
#   df2$mean_x <- (df2$xmin + df2$xmax) / 2
#   df2$mean_y <- (df2$ymin + df2$ymax) / 2
#   ### Plot creation
#
#   p <- ggplot(data = df2) +
#     geom_rect(aes_string(xmin = 'xmin', xmax = 'xmax', ymin = 'ymin', ymax = 'ymax', fill = 'norm_eval'),
#               color = "black", size = 0.5) +
#
#     # Text labels
#     geom_text(aes_string(x = 'mean_x', y = 'mean_y', label = 'label'),
#               hjust = 0.5, vjust = 0.5, color = "black", size = 3) +
#
#     # Scale and theme
#     scale_fill_gradient2(low = "red", mid = "yellow", high = "green", name = "Normed Evaluation", midpoint = 0.5) +
#     theme_minimal() +
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title = element_blank(),
#           legend.position = "bottom") +
#     scale_y_reverse(breaks = 1:7, labels = paste("Leaf", 1:7)) +
#     scale_size_continuous(range = c(3, 6), guide = "none")
#
#
#   return(p)
#
# }


#' Create a List of Synoptic Plots Based on Different Options
#'
#' Produces a list of synoptic plots, one for each set of options specified in
#' the columns of the 'options' matrix. Each plot visualizes nodes of a decision
#' tree based on different evaluation options.
#'
#' @param tree \code{Tree structure object} representing the decision tree.
#' @param options \code{matrix} where each column denotes a distinct set of
#'   options for evaluating the decision tree.
#' @param depth Optional \code{numeric} specifying the depth of the tree. If
#'   provided, a sub-tree is produced up to the specified depth. By default, it
#'   is set to NA.
#'
#' @return A \code{list} of \code{ggplot} objects. Each item in the list is a
#'   synoptic plot corresponding to a column from the 'options' matrix.
#'
#' @seealso \code{\link{create_synoptique}}
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' options <- create_options(tree, num_options=3, seed = 42)
#' create_list_synoptique(tree, options)
#'
#' @export
create_list_synoptique <- function(tree, options, depth = NA) {
  list_synop <- list()

  # Generate a synoptic plot for each column in options
  for (i in 1:dim(options)[2]) {
    list_synop[[i]] <- create_synoptique(tree, as.matrix(options[, i]), depth)
  }

  return(list_synop)
}
