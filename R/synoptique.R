#' Title
#'
#' @param Tree Tree
#' @param option option
#' @param depth depth
#'
#' @return
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
create_synoptique <- function(Tree, option, depth = NA) {

  if (!is.na(depth)) {
    if (!(depth == Tree@RootName)) {
      Tree <- create_sub_tree(Tree, depth)
    }
  }

  ### create data_frame used for plot values
  # attribut column
  df <- data.frame(attribut = Tree@Attributes)

  # isleaf column (needed for construction)
  df$isleaf <- lapply(df$attribut, function(x) {
    Tree@Nodes[[which(Tree@Attributes == x)]]@IsLeaf
  }) %>%
    unlist()

  # depth column
  df$depth <- lapply(df$attribut, function(x) {
    if (df[df$attribut==x, "isleaf"]) {
      Tree@Nodes[[which(Tree@Attributes == x)]]@Depth:Tree@Depth
    } else {
      Tree@Nodes[[which(Tree@Attributes == x)]]@Depth
    }
  })

  # taille column
  df$taille <- lapply(df$attribut, function(x) {
    if (df[df$attribut==x, "isleaf"]) {
      1
    } else {
      subtree <- create_sub_tree(Tree, x)
      length(subtree@Leaves)
    }
  }) %>%
    unlist()

  # columns used for colors
  df$eval <- evaluate_scenario(Tree, option)
  df$rangecol <- lapply(df$attribut, function(x) {
    Tree@Nodes[[which(Tree@Attributes == x)]]@RangeScale
  }) %>%
    unlist()

  # labels column
  df$label <- paste0(df$attribut, " (", df$eval, "/", df$rangecol, ")")


  ### get all box coordinates
  df2 <- lapply(1:max(unlist(df$depth)), function(x) {
    listtemp <- list()
    for(i in 1:dim(df)[1]) {
      if (x %in% unlist(df[i, "depth"][[1]])) {
        listtemp[[i]] <- df[i, ]
      }
    }

    dftemp <- bind_rows(listtemp)

    dftemp$ymin <- cumsum(lag(dftemp$taille, default = 0))
    dftemp$ymax <- cumsum(dftemp$taille)
    dftemp$xmin <- lapply(dftemp$depth, min) %>% unlist()
    dftemp$xmax <- lapply(dftemp$depth, max) %>% unlist() + 1

    return(dftemp)
  }) %>% bind_rows()

  # get normed color
  df2$norm_eval <- (df2$eval-1) / (df2$rangecol-1)

  # Scale text with x et y
  #
  df2$label_length <- nchar(df2$label)

  #
  df2$rect_width <- df2$xmax - df2$xmin
  df2$rect_height <- df2$ymax - df2$ymin

  # for (i in 1:dim(df2)[1]) {
  #   max_chars <- 20*df2[i, "rect_width"]
  #   wrapped_text <- strwrap(df2[i, "label"], width = max_chars, simplify = FALSE)
  #   # df2[i, "adjusted_label"] <- sapply(wrapped_text, function(txt) paste(txt, collapse = "\n"))
  #   #
  #   # df2[i, "text_scale"] <- min(with(df2[i, ],1 + 5 * (rect_width/max(length(wrapped_text[[1]]))) * (1 - label_length/max(label_length))),
  #   #                             0.5*df2[i, ]$rect_height)
  # }

  # adjust_label <- function(label, rect_width) {
  #
  #   max_chars <- 20*rect_width # C'est un paramètre que vous pouvez ajuster selon vos besoins
  #   wrapped_text <- strwrap(label, width = max_chars, simplify = FALSE)
  #   paste(wrapped_text, collapse = "\n")
  #   wrapped_text <- sapply(wrapped_text, function(txt) paste(txt, collapse = "\n"))
  # }
  # df2$adjusted_label <- mapply(adjust_label, df2$label, df2$rect_width)


  df2$mean_x <- (df2$xmin + df2$xmax) / 2
  df2$mean_y <- (df2$ymin + df2$ymax) / 2
  ### Plot creation

  p <- ggplot(data = df2) +
    geom_rect(aes_string(xmin = 'xmin', xmax = 'xmax', ymin = 'ymin', ymax = 'ymax', fill = 'norm_eval'),
              color = "black", size = 0.5) +

    # Text labels
    geom_text(aes_string(x = 'mean_x', y = 'mean_y', label = 'label'),
              hjust = 0.5, vjust = 0.5, color = "black", size = 3) +

    # Scale and theme
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green", name = "Normed Evaluation", midpoint = 0.5) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") +
    scale_y_reverse(breaks = 1:7, labels = paste("Leaf", 1:7)) +
    scale_size_continuous(range = c(3, 6), guide = "none")


  return(p)

}


#' Title
#'
#' @param Tree Tree
#' @param options options
#' @param depth depth
#'
#' @return
#' @export
create_list_synoptique <- function(Tree, options, depth = NA) {
  list_synop <- list()
  for(i in 1:dim(options)[2]) {
    list_synop[[i]] <- create_synoptique(Tree, as.matrix(options[, i]), depth)
  }
  return(list_synop)
}

