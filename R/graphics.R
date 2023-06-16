#'@title scatter graph
#'@description scatter graph
#'@param series data.frame contain x, value, and variable
#'@param label_series title label
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@export
plot_scatter <- function(series, label_series = "", label_x = "", label_y = "", colors = NULL) {
  x <- 0
  value <- 0
  variable <- 0
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable, group=variable)) + geom_point(size=1)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.position = "bottom") + theme(legend.key = element_blank())
  return(grf)
}

#'@title plot points
#'@description plot points
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_points <- function(data, label_x = "", label_y = "", colors = NULL) {
  x <- 0
  value <- 0
  variable <- 0
  series <- reshape::melt(as.data.frame(data), id.vars = c(1))
  cnames <- colnames(data)[-1]
  colnames(series)[1] <- "x"
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable, group=variable)) + geom_point(size=1)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=cnames)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.key = element_blank())
  return(grf)
}

#'@title plot series
#'@description plot series
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@return plot
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_series <- function(data, label_x = "", label_y = "", colors = NULL) {
  x <- 0
  value <- 0
  variable <- 0
  series <- reshape::melt(as.data.frame(data), id.vars = c(1))
  cnames <- colnames(data)[-1]
  colnames(series)[1] <- "x"
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable, group=variable)) + geom_point(size=1.5) + geom_line(linewidth=1)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=cnames)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.key = element_blank())
  return(grf)
}

#'@title plot bar graph
#'@description plot bar graph
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param alpha level of transparency
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@export
plot_bar <- function(data, label_x = "", label_y = "", colors = NULL, alpha=1) {
  series <- as.data.frame(data)
  if (!is.factor(series[,1]))
    series[,1] <- as.factor(series[,1])
  grf <- ggplot(series, aes_string(x=colnames(series)[1], y=colnames(series)[2]))
  if (!is.null(colors)) {
    grf <- grf + geom_bar(stat = "identity", fill=colors, alpha=alpha)
  }
  else {
    grf <- grf + geom_bar(stat = "identity", alpha=alpha)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

#'@title plot grouped bar
#'@description plot grouped bar
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param alpha level of transparency
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_groupedbar <- function(data, label_x = "", label_y = "", colors = NULL, alpha=1) {
  variable <- 0
  value <- 0
  x <- 0
  cnames <- colnames(data)[-1]
  series <- reshape::melt(as.data.frame(data), id.vars = c(1))
  colnames(series)[1] <- "x"
  if (!is.factor(series$x))
    series$x <- as.factor(series$x)

  grf <- ggplot(series, aes(x, value, fill=variable))
  grf <- grf + geom_bar(stat = "identity",position = "dodge", alpha=alpha)
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(cnames, values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

#'@title plot stacked bar
#'@description plot stacked bar
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param alpha level of transparency
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_stackedbar <- function(data, label_x = "", label_y = "", colors = NULL, alpha=1) {
  x <- 0
  value <- 0
  variable <- 0
  cnames <- colnames(data)[-1]
  series <- reshape::melt(as.data.frame(data), id.vars = c(1))
  colnames(series)[1] <- "x"
  if (!is.factor(series$x))
    series$x <- as.factor(series$x)

  grf <- ggplot(series, aes(x=x, y=value, fill=variable)) + geom_bar(stat="identity", colour="white")
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(cnames, values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$x))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}

#'@title plot radar
#'@description plot radar
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_radar <- function(data, label_x = "", label_y = "", colors = NULL)  {
  series <- as.data.frame(data)
  if (!is.factor(series[,1]))
    series[,1] <- as.factor(series[,1])
  series$group <- 1
  grf <- ggplot(series, aes_string(x=colnames(series)[1], y=colnames(series)[2], group="group"))
  grf <- grf + geom_point(size=2, color=colors)
  grf <- grf + geom_polygon(size = 1, alpha= 0.1, color=colors)
  grf <- grf + theme_light()
  grf <- grf + coord_polar()
  return(grf)
}

#'@title plot lollipop
#'@description plot lollipop
#'@param data data.frame contain x, value, and variable
#'@param colors color vector
#'@param xlabel x-axis label
#'@param ylabel y-axis label
#'@param size_text size of text
#'@param size_ball size of ball
#'@param alpha_ball transparency of ball
#'@param min_value minimum value
#'@param max_value_gap maximum value gap
#'@param flip flip axis
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_lollipop <- function(data, colors, xlabel = "", ylabel = "", size_text=3, size_ball=8, alpha_ball=0.2, min_value=0, max_value_gap=1, flip = TRUE) {
  value <- 0
  x <- 0
  cnames <- colnames(data)[-1]
  series <- reshape::melt(as.data.frame(data), id.vars = c(1))
  colnames(series)[1] <- "x"
  if (!is.factor(series$x))
    series$x <- as.factor(series$x)
  series$value <- round(series$value)

  grf <- ggplot(data=series, aes(x=x, y=value, label=value)) +
    geom_segment(aes(x=x, xend=x, y=min_value, yend=(value-max_value_gap)), color=colors, size=1) +
    geom_text(color="black", size=size_text) +
    geom_point(color=colors, size=size_ball, alpha=alpha_ball) +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ylab(xlabel) + xlab(xlabel)
  if (flip)
    grf <- grf + coord_flip()

  return(grf)
}

#'@title plot pie
#'@description plot pie
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param textcolor text color
#'@param bordercolor border color
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@importFrom dplyr filter summarise group_by arrange mutate
#'@export
plot_pieplot <- function(data, label_x = "", label_y = "", colors = NULL, textcolor="white", bordercolor="black") {
  x <- prop <- ypos <- label <- value <- desc <- n <- 0

  prepare.pieplot <- function(series) {
    colnames(series) <- c("x", "value")
    if (!is.factor(series$x)) {
      series$x <- as.factor(series$x)
    }

    series$colors <- colors

    series <- series |>
      dplyr::arrange(desc(x)) |>
      dplyr::mutate(prop = value / sum(series$value) *100) |>
      dplyr::mutate(ypos = cumsum(prop)- 0.5*prop) |>
      dplyr::mutate(label = paste(round(value / sum(value) * 100, 0), "%"))
    return(series)
  }
  series <- prepare.pieplot(data)

  # Basic piechart
  grf <- ggplot(series, aes(x="", y=prop, fill=x)) + geom_bar(width = 1, stat = "identity", color=bordercolor)
  grf <- grf + theme_minimal(base_size = 10)
  grf <- grf + coord_polar("y", start=0)
  grf <- grf + geom_text(aes(y = ypos, label = label), size=6, color=textcolor)
  if (!is.null(colors))
    grf <- grf + scale_fill_manual(series$x, values = colors)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme(axis.text.x=element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  return(grf)
}


#'@title plot histogram
#'@description plot histogram
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param color color vector
#'@param alpha transparency level
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@importFrom graphics hist
#'@importFrom dplyr filter summarise group_by arrange mutate
#'@export
plot_hist <-  function(data, label_x = "", label_y = "", color = 'white', alpha=0.25) {
  variable <- 0
  value <- 0
  cnames <- colnames(data)[1]
  series <- reshape::melt(as.data.frame(data))
  series <- series |> dplyr::filter(variable %in% cnames)
  tmp <- graphics::hist(series$value, plot = FALSE)
  grf <- ggplot(series, aes(x=value))
  grf <- grf + geom_histogram(breaks=tmp$breaks,fill=color, alpha = alpha, colour="black")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + scale_fill_manual(name = cnames, values = color)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  return(grf)
}

#'@title plot boxplot
#'@description plot boxplot
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param barwith width of bar
#'@return ggplot graphic
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_boxplot <- function(data, label_x = "", label_y = "", colors = NULL, barwith=0.25) {
  value <- 0
  variable <- 0
  cnames <- colnames(data)
  series <- reshape::melt(as.data.frame(data))
  grf <- ggplot(aes(y = value, x = variable), data = series)
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(fill = colors, width=barwith)
  }
  else {
    grf <- grf + geom_boxplot(width=barwith)
  }
  grf <- grf + labs(color=cnames)
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(cnames, values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}


#'@title plot boxplot per class
#'@description plot boxplot per class
#'@param data data.frame contain x, value, and variable
#'@param class_label name of attribute for class label
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_boxplot_class <- function(data, class_label, label_x = "", label_y = "", colors = NULL) {
  value <- 0
  variable <- 0
  x <- 0
  data <- reshape::melt(data, id=class_label)
  colnames(data)[1] <- "x"
  if (!is.factor(data$x))
    data$x <- as.factor(data$x)
  grf <- ggplot(data=data, aes(y = value, x = x))
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(fill=colors)
  }
  else {
    grf <- grf + geom_boxplot()
  }
  grf <- grf + labs(color=levels(data$x))
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual(levels(data$x), values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  return(grf)
}


#'@title plot density
#'@description plot density
#'@param data data.frame contain x, value, and variable
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param bin bin width
#'@param alpha level of transparency
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_density <-  function(data, label_x = "", label_y = "", colors = NULL, bin = NULL, alpha=0.25) {
  value <- 0
  variable <- 0
  grouped <- ncol(data) > 1
  cnames <- colnames(data)
  series <- reshape::melt(as.data.frame(data))
  if (grouped) {
    grf <- ggplot(series, aes(x=value,fill=variable))
    if (is.null(bin))
      grf <- grf + geom_density(alpha = alpha)
    else
      grf <- grf + geom_density(binwidth = bin, alpha = alpha)
  }
  else {
    grf <- ggplot(series, aes(x=value))
    if (is.null(bin)) {
      if (!is.null(colors))
        grf <- grf + geom_density(fill=colors, alpha = alpha)
      else
        grf <- grf + geom_density(alpha = alpha)
    }
    else {
      if (!is.null(colors))
        grf <- grf + geom_density(binwidth = bin,fill=colors, alpha = alpha)
      else
        grf <- grf + geom_density(binwidth = bin, alpha = alpha)
    }
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  if (!is.null(colors))
    grf <- grf + scale_fill_manual(name = cnames, values = colors)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank(), legend.position = "bottom")
  return(grf)
}

#'@title plot density per class
#'@description plot density per class
#'@param data data.frame contain x, value, and variable
#'@param class_label name of attribute for class label
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@param colors color vector
#'@param bin bin width
#'@param alpha level of transparency
#'@examples trans <- dal_transform()
#'@import ggplot2
#'@importFrom reshape melt
#'@export
plot_density_class <-  function(data, class_label, label_x = "", label_y = "", colors = NULL, bin = NULL, alpha=0.5) {
  value <- 0
  variable <- 0
  x <- 0
  data <- reshape::melt(data, id=class_label)
  colnames(data)[1] <- "x"
  if (!is.factor(data$x))
    data$x <- as.factor(data$x)
  grf <- ggplot(data=data, aes(x = value, fill = x))
  if (is.null(bin))
    grf <- grf + geom_density(alpha = alpha)
  else
    grf <- grf + geom_density(binwidth = bin, alpha = alpha)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  if (!is.null(colors))
    grf <- grf + scale_fill_manual(name = levels(data$x), values = colors)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank(), legend.position = "bottom")
  return(grf)
}

