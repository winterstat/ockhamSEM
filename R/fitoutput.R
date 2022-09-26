# Fitting Propensity Plot and Table Functions

#' Euler Plot of fitting propensity for a specific fit index
#'
#' @param x Data frame of specific fit index values extracted from object of class fitprop, as created by run.fitprop function.
#' @param m Integer value indicating location of fit index in list of fit indices to plot
#' @param fm Character value indicating which fit index to plot
#' @param whichmod Index number corresponding to which model(s) to include on the plot. Defaults to all models.
#' @param savePlot Logical value indicating whether to save plot to a list (TRUE) or just produce plot to output.
#' @param samereps Logical value indicating whether to use only results from replications in which all selected models yielded results.
#' @param cutoff Numeric vector indicating what cut value of the fit indice(s) to use for euler plots. When not specified, defaults to 0.1 for all indices.
#' @param lower.tail Logical vector indicating whether lower values of each fit index corresponds to good fit.
#' @param mod.lab Optional character vector of labels for each model.
#' @param mod.brewer.pal Optional string corresponding to the palette from RColorBrewer to use for the different models. e.g.,
#'   see \code{\link[RColorBrewer]{display.brewer.all}}.
#'
#' @returns A ggplot object of the Euler plot.
#'
#' @export
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom eulerr euler
#' @importFrom dplyr filter
#' @importFrom ggalt geom_encircle
#' @importFrom RColorBrewer brewer.pal
#' @importFrom geomtextpath geom_textpath

euler.fitprop <- function(x,
                          m = m,
                          fm = fm,
                          whichmod = NULL,
                          savePlot = FALSE,
                          samereps = TRUE,
                          cutoff = NULL,
                          lower.tail = NULL,
                          mod.lab = NULL,
                          mod.brewer.pal = "Set1") {
  # Define binding for global variables (to avoid note)
  model <- label_text <- NULL

  dat <- x
  #nmod <- ncol(dat) - 1
  nmod <- ncol(dat)
  nrep <- nrow(dat)

  # Categorize fit index values
  # if (lower.tail[m]) {
  #   dat[, whichmod] <-
  #     dat[, whichmod] < cutoff[m] # how many meet cutoff criterion
  # } else {
  #   dat[, whichmod] <-
  #     dat[, whichmod] > cutoff[m] # how many meet cutoff criterion
  # }
  #
  if (lower.tail[m]) {
    dat <-
      dat < cutoff[m] # how many meet cutoff criterion
  } else {
    dat <-
      dat > cutoff[m] # how many meet cutoff criterion
  }

  # Add column representing full data space
  #tmp <-
  #  cbind(data.frame(Total = rep(TRUE, nrow(dat))), dat[, whichmod])

  tmp <-
    cbind(data.frame(Total = rep(TRUE, nrow(dat))), dat)

  # If samereps, then omit if not all reps converged
  if (samereps) {
    tmp <- na.omit(tmp)
  }

  # Add column names for models
  # colnames(tmp) <- c("Total", mod.lab)

  # Find percentage of total data space that each model fits
  perc.fit <- apply(tmp, 2, mean)
  perc.fit <- perc.fit * 100
  perc.fit <- round(perc.fit, digits = 2)
  perc.fit <- perc.fit[2:length(perc.fit)]


  # Order of FP from largest to smallest
  fp_order <- order(colSums(tmp), decreasing = TRUE)
  total_loc <- which(fp_order == 1)
  fp_order <- fp_order[-total_loc]


  # Get euler plot coordinates
  eulerfit <- euler(tmp, shape = "circle")

  #Transform Euler data into xy coordinates to use with ggplot:
  for (i in 1:nrow(eulerfit$ellipses)) {
    # For each model + total data space
    # Extract Euler values
    ellipse.val <- eulerfit$ellipses[i, ]

    # Check if the ellipse is empty (cutoff was not reached by any of the datasets)
    if (is.na(ellipse.val[1, 1])) {
      # Create a data frame with the x and y coordinates for this model set to NA
      temp <-
        data.frame("x" = NA,
                   "y" = NA,
                   "model" = row.names(ellipse.val))

    } else {
      # If the ellipse does exist, extract its coordinates:
      xc <- as.numeric(ellipse.val[1, 1]) # center x or h
      yc <- as.numeric(ellipse.val[1, 2]) # center on y or k
      a <- as.numeric(ellipse.val[1, 3]) # major axis length
      b <- as.numeric(ellipse.val[1, 4]) # minor axis length

      # Number of points on circle
      t <-
        seq(0, 2 * pi, length.out = 56) # Make length.out higher for smoother circles (56)

      # Get x and y coordinates
      x <- xc + a * cos(t)
      y <- yc + b * sin(t)

      # Create a data frame with the x and y coordinates for this model
      temp <-
        data.frame("x" = x,
                   "y" = y,
                   "model" = row.names(ellipse.val))
    }

    if (i == 1) {
      all.ellipse <- temp
    } else {
      all.ellipse <- rbind(all.ellipse, temp)
    }
  } # End for loop transform to xy

  all.lab <- c("Complete Data Space", paste0(mod.lab, " (", perc.fit, "%)"))

  all.ellipse$model <- factor(all.ellipse$model,
                              levels = c("Total", mod.lab),
                              labels = all.lab)

  # Create background dots to represent the full data space

  # Get x and y range and add a little bit:
  range.x <- range(all.ellipse$x, na.rm = T) + c(-.25, .25)
  range.y <- range(all.ellipse$y, na.rm = T) + c(-.25, .25)

  # Create sequences that span the x and y axis
  seq.x <- seq(range.x[1], range.x[2], length.out = 100)
  seq.y <- seq(range.y[1], range.y[2], length.out = 100)

  # Create a data frame that contains all combinations of x and y values
  combi.xy <- expand.grid(seq.x, seq.y)

  # Filter points so that only those inside the "complete data space" circle remain
  # Points that are in the circle are: (x - h)^2 + (y - k)^2 < R^2
  combi.xy$r2 <- (combi.xy$Var1 - eulerfit$ellipses[1, 1])^2 + (combi.xy$Var2 - eulerfit$ellipses[1, 2])^2

  combi.xy <- dplyr::filter(combi.xy, .data$r2 < eulerfit$ellipses[1, 3]^2)

  line.pal <- c("dashed", "dotted", "dotdash", "longdash", "twodash")
  line.pal <- rep_len(line.pal, nmod)
  color.pal <- brewer.pal(n = max(3, nmod), name = mod.brewer.pal)
  fill.pal <-  brewer.pal(n = max(3, nmod), name = mod.brewer.pal)


  # Set up plot text elements
  title.text = "Fitting Propensity per Model\n"
  #legend.title = "Model"
  legend.title = ""
  legend.loc = "bottom"
  num.datasets = nrow(tmp)
  caption.text = paste0("* Approximated by ",format(num.datasets, big.mark = ",", trim = TRUE)," random datasets.")

  if (lower.tail[m] == FALSE) {
    caption.text = paste0(caption.text, " Fit based on ", toupper(fm), " \u2265 ", cutoff[m], ".")
  } else if (lower.tail[m] == TRUE) {
    caption.text = paste0(caption.text, " Fit based on ", toupper(fm), " \u2264 ", cutoff[m], ".")
  }

  # Get coordinates for Complete data space label
  text_curve <-
    dplyr::filter(all.ellipse, model == "Complete Data Space" &
                    x < 0 & y > 0)
  text_curve$label_text <- "All Possible Data*"
  text_curve <- text_curve[, c("x", "y", "label_text")]

  # Create graph
  graph <- ggplot(combi.xy, aes(x = .data$Var1, y = .data$Var2)) +
    geom_point(color = "grey60",
               alpha = .3,
               size = .1) +
    geom_encircle(
      #ggalt::geom_encircle(
      data = all.ellipse,
      aes(
        x = x,
        y = y,
        fill = .data$model,
        color = .data$model
      ),
      alpha = .45,
      size = 2,
      s_shape = 0.4,
      expand = 0.00
    ) +
    scale_color_manual(legend.title,
                       limits = levels(all.ellipse$model)[fp_order], #if not ordered, replace fp_order with 2:ncol(tmp)
                       values = color.pal) +
    scale_fill_manual(
      legend.title,
      limits = levels(all.ellipse$model)[fp_order],#if not ordered, replace fp_order with 2:ncol(tmp)
      values = fill.pal,
      na.value = NA
    ) +
    coord_cartesian(xlim = range.x,
                    ylim = range.y,
                    expand = TRUE) +
    geom_textpath(
      #geomtextpath::geom_textpath(
      data = text_curve,
      aes(x = x, y = y, label = label_text),
      text_only = TRUE,
      #text_smoothing = 100,
      hjust = 0.5,
      vjust = 1.5,
      fontface = 2,
      family = "sans",
      size = 4.5
    ) +
    labs(caption = caption.text,
         title = title.text) +
    theme_void() +
    theme(
      plot.margin = margin(5, 0, 0, 0, "pt"),
      # t,r,b,l
      legend.justification = "top",
      legend.position = legend.loc,
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      text = element_text(family = "sans"),
      plot.caption = element_text(size = 8),
      plot.title = element_text(
        hjust = .5,
        face = "bold",
        size = 18,
        margin = margin(5, 0, -15, 0)
      ),
      # make space between title and plot smaller
      aspect.ratio = 1
    ) +
    NULL

  return(graph)
}

#' ECDF Plot of fitting propensity for a specific fit index
#'
#' @param x Data frame of specific fit index values extracted from object of class fitprop, as created by run.fitprop function.
#' @param m Integer value indicating location of fit index in list of fit indices to plot
#' @param fm Character value indicating which fit index to plot
#' @param whichmod Index number corresponding to which model(s) to include on the plot. Defaults to all models.
#' @param savePlot Logical value indicating whether to save plot to a list (TRUE) or just produce plot to output.
#' @param xlim Numeric vector of length 2 indicating the limits for the x-axis of the plot.
#' @param samereps Logical value indicating whether to use only results from replications in which all selected models yielded results.
#' @param cutoff Optional numeric vector indicating what cut value of the fit indice(s) to use. When not specified, no cutoff line is included in resulting plots.
#' @param lower.tail Logical vector indicating whether lower values of each fit index corresponds to good fit.
#' @param mod.lab Optional character vector of labels for each model.
#' @param mod.brewer.pal Optional string corresponding to the palette from RColorBrewer to use for the different models. e.g.,
#'   see \code{\link[RColorBrewer]{display.brewer.all}}.
#'
#' @returns A ggplot object of the ECDF plot.
#'
#'
#' @export
#' @import ggplot2
#' @importFrom stats ecdf
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom RColorBrewer brewer.pal

ecdf.fitprop <- function(x,
                         m = m,
                         fm = fm,
                         whichmod = NULL,
                         savePlot = FALSE,
                         xlim = c(0, 1),
                         samereps = TRUE,
                         cutoff = NULL,
                         lower.tail = NULL,
                         mod.lab = NULL,
                         mod.brewer.pal = "Set1") {
  dat <- x
  #nmod <- ncol(dat) - 1
  nmod <- ncol(dat)
  nrep <- nrow(dat)

  # From here on is ECDF specific code
  if (samereps) {
    dat <- na.omit(dat)
  }
  #dat <- gather(dat, "variable", "value", all_of(whichmod))
  dat <- pivot_longer(dat, cols = all_of(mod.lab), names_to = "variable", values_to = "value")

  graph <- ggplot(dat, aes(x = .data$value)) +
    stat_ecdf(
      aes(linetype = .data$variable, color = .data$variable),
      na.rm = TRUE,
      size = .7
    ) +
    labs(y = "Cumulative Probability",
         x = paste0(toupper(fm), " value")) +
    scale_color_brewer(palette = mod.brewer.pal)

  if (lower.tail[m]) {
    graph <- graph + xlim(xlim[1], xlim[2])
  } else {
    graph <- graph + xlim(xlim[2], xlim[1])
  }

  if (!is.null(cutoff)) {

    graph <- graph + geom_segment(
      aes(
        x = cutoff[m],
        y = -Inf,
        xend = cutoff[m],
        yend = Inf
      ),
      size = .7,
      linetype = "dotted"
    ) +
      labs(caption = paste0("Vertical dotted line represents cutoff value: ", cutoff[m]))
  }

  graph <- graph +
    guides(color = guide_legend(title = "Model")) +
    guides(linetype = guide_legend(title = "Model", values = c("Cutoff" = 3))) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      text = element_text(family = "sans")
    )

  return(graph)
}

#' Overview of intersections of fitting propensity of included models
#'
#' @param x Object of class fitprop, as created by run.fitprop function.
#' @param whichmod Index number corresponding to which model(s) to include on the plot. Defaults to all models.
#' @param whichfit Character vector indicating which indices of model fit to include. Defaults to all saved indices.
#' @param samereps Logical value indicating whether to use only results from replications in which all selected models yielded results.
#' @param cutoff Optional numeric vector indicating what cut value of the fit indice(s) to use. When not specified, no cutoff line is included in resulting plots.
#' @param lower.tail Logical vector indicating whether lower values of each fit index corresponds to good fit.
#' @param mod.lab Optional character vector of labels for each model.
#' @param saveTable Logical value indicating whether to save tables to a list (TRUE) or just produce table to output.
#'
#' @returns A list of matrices listing the different intersections, their frequency, and proportion within the dataspace.
#'
#' @examples
#' \donttest{
#' #' # Set up a covariance matrix to fit models to
#' p<-3 # number of variables
#' temp_mat <- diag(p) # identity matrix
#' colnames(temp_mat) <- rownames(temp_mat) <- paste0("V", seq(1, p))
#'
#' # Define and fit two models using lavaan package
#' mod1a <- 'V3 ~ V1 + V2
#'   V1 ~~ 0*V2'
#' mod2a <- 'V3 ~ V1
#'   V2 ~ V3'
#'
#' mod1a.fit <- sem(mod1a, sample.cov=temp_mat, sample.nobs=500)
#' mod2a.fit <- sem(mod2a, sample.cov=temp_mat, sample.nobs=500)
#'
#' # Run fit propensity analysis (50 reps to save time)
#' # Onion approach, save srmr and CFI
#' res <- run.fitprop(mod1a.fit, mod2a.fit, fit.measure=c("srmr","cfi"),
#'   rmethod="onion",reps=50)
#'
#' # Generate intersection tables
#' intersect.fitprop(res)
#'
#' # Specify custom cutoffs and lower.tail
#' intersect.fitprop(res, cutoff = c(.08, .95), lower.tail = c(TRUE, FALSE))
#'
#' # Save tables as a list and access first one.
#' tables <- intersect.fitprop(res, saveTable = TRUE)
#' tables[[1]]
#'
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom dplyr count
#' @importFrom dplyr group_by_all

intersect.fitprop <- function(x,
                              whichmod = NULL,
                              whichfit = colnames(x$fit_list[[1]]),
                              samereps = TRUE,
                              cutoff = NULL,
                              lower.tail = NULL,
                              mod.lab = NULL,
                              saveTable = FALSE) {
  if (class(x) != "fitprop") {
    stop('At this time, this function can only handle objects of class fitprop')
  }

  data <- x$fit_list
  nmod <- length(data) # number of models
  nfit <- ncol(data[[1]]) # number of fit measures
  nrep <- nrow(data[[1]]) # number of available replications

  if (is.null(whichfit)) {
    whichfit <- colnames(data[[1]])
  }

  if (!is.null(whichmod) & is.null(mod.lab)) {
    mod.lab <- paste0("Model ", whichmod)
  } else if(is.null(whichmod) & is.null(mod.lab)) {
    mod.lab <- paste0("Model ", 1:nmod)
  }

  if (is.null(whichmod)) {
    whichmod <- 1:nmod
  }

  if(!is.null(lower.tail) & length(whichfit) != length(lower.tail)) {
    message("'whichfit' and 'lower.tail' do not match in number of elements. Mismatch may exist.")
  }

  if (is.null(lower.tail)) {
    lower.tail <- rep(TRUE, length(whichfit))
    message(
      "By default, lower tail indicates good fit. Use argument 'lower.tail' to adjust this value per fit index."
    )
  }

  if(!is.null(cutoff) & length(whichfit) != length(cutoff)) {
    message("'whichfit' and 'cutoff' do not match in number of elements. Mismatch may exist.")
  }

  # If no cutoff values are specified, set defaults and send message to user
  if(is.null(cutoff)) {
    cutoff <- rep(.1, length(whichfit))
    message("Cutoff value defaults to 0.1 for all fit indices. Pick better values with argument 'cutoff'.")
  }

  tables <- list()
  # loop over fit indices
  m <- 1
  for (fm in whichfit) {
    # extract and format data
    dat <- matrix(, nrep, nmod)
    j <- 1
    for (mod in 1:nmod) {
      dat[, j] <- data[[mod]][, fm]
      j <- j + 1
    }
    dat <- as.data.frame(dat)
    dat <- dat[,whichmod]
    colnames(dat) <- mod.lab
    #dat$id <- 1:nrow(dat)

    # Categorize fit index values
    # if (lower.tail[m]) {
    #   dat[, whichmod] <-
    #     dat[, whichmod] < cutoff[m] # how many meet cutoff criterion
    # } else {
    #   dat[, whichmod] <-
    #     dat[, whichmod] > cutoff[m] # how many meet cutoff criterion
    # }

    if (lower.tail[m]) {
      dat <-
        dat < cutoff[m] # how many meet cutoff criterion
    } else {
      dat <-
        dat > cutoff[m] # how many meet cutoff criterion
    }


    # Extract just the selected models
    #tmp <- dat[, whichmod]
    tmp <- dat

    # If samereps, then omit if not all reps converged
    if (samereps) {
      tmp <- na.omit(tmp)
    }

    # Add column names for models
    tmp <- data.frame(tmp)
    colnames(tmp) <- mod.lab

    # Add column for models that never fit
    tmp$None <- rowSums(tmp[,mod.lab]) == 0

    # Set up order of intersections
    # Find out how often each intersection occurs
    #order_intersect <- dplyr::count(dplyr::group_by_all(tmp))

    order_intersect <- count(group_by_all(tmp))


    # Find out which models are involved with each intersection and list them with commas
    intersect <- unique(apply(order_intersect[,-c(ncol(order_intersect))], 1,
                              function(r) paste(names(order_intersect[,-c(ncol(order_intersect))])[as.logical(r)], collapse = ", "))
    )

    order_intersect <- cbind(order_intersect, intersect = intersect)
    order_intersect$num_comma <- sapply(order_intersect$intersect, function(x) length(strsplit(x, ",")[[1]]))

    freq_order <- order(order_intersect$num_comma, -rank(order_intersect$n), decreasing = FALSE)
    order_intersect <- order_intersect[freq_order,c("intersect", "n")]

    order_intersect$proportion <- order_intersect$n / nrow(tmp)

    names <- order_intersect$intersect
    order_intersect <- as.matrix(order_intersect[,2:3])

    row.names(order_intersect) <- names

    tables[[m]] <- order_intersect


    # do something with plot
    if (saveTable) {
      tables[[m]] <- order_intersect
    } else {
      cat(paste0("\nIntersections for ", toupper(fm), "\n\n"))
      print.default(round(order_intersect, 3))
    }
    m <- m + 1
  }

  if (saveTable) {
    invisible(tables)
    tables <- setNames(tables, whichfit)
  }


}


#' Plot of fitting propensity rankings for a specific fit index across cutoff values
#'
#' @param x Data frame of specific fit index values extracted from object of class fitprop, as created by run.fitprop function.
#' @param m Integer value indicating location of fit index in list of fit indices to plot
#' @param fm Character value indicating which fit index to plot
#' @param whichmod Index number corresponding to which model(s) to include on the plot. Defaults to all models.
#' @param savePlot Logical value indicating whether to save plot to a list (TRUE) or just produce plot to output.
#' @param xlim Numeric vector of length 2 indicating the limits for the x-axis of the plot.
#' @param samereps Logical value indicating whether to use only results from replications in which all selected models yielded results.
#' @param lower.tail Logical vector indicating whether lower values of each fit index corresponds to good fit.
#' @param mod.lab Optional character vector of labels for each model.
#' @param ties.method Character value indicating the method for treating ties in the rank function (see \code{\link[base]{rank}}).
#' @param mod.brewer.pal Optional string corresponding to the palette from RColorBrewer to use for the different models. e.g.,
#'   see \code{\link[RColorBrewer]{display.brewer.all}}.
#'
#' @returns A ggplot object of the rankings plot.
#'
#'
#' @export
#' @import ggplot2
#' @importFrom stats ecdf
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom RColorBrewer brewer.pal

ranks.fitprop <- function(x,
                          m = m,
                          fm = fm,
                          whichmod = NULL,
                          savePlot = FALSE,
                          xlim = c(0, 1),
                          samereps = TRUE,
                          lower.tail = NULL,
                          mod.lab = NULL,
                          ties.method = "min",
                          mod.brewer.pal = "Set1") {
  # Define binding for global variables (to avoid note)
  name <- NULL

  dat <- x
  #dat <- dat[, whichmod]
  nmod <- ncol(dat)
  nrep <- nrow(dat)

  if (samereps) {
    dat <- na.omit(dat)
  }

  xseq <- seq(0, 1, .05)

  tmp <- data.frame(xseq)
  for(j in 1:nmod) {
    tmp[,j] <- ecdf(dat[,j])(xseq)

  }

  if(lower.tail[m]) {
    ranks <- t(apply(-tmp, 1, rank, ties.method = ties.method))
  } else {
    ranks <- t(apply(tmp, 1, rank, ties.method = ties.method))
  }

  ranks <- data.frame(ranks, xseq)
  colnames(ranks) <- c(mod.lab, "xseq")
  ranks <- pivot_longer(ranks, cols = -xseq, values_to = "rank")

  graph <- ggplot(ranks, aes(x = xseq, y = rank, group = name)) +
    geom_line(aes(color = name), alpha = .6, size = 2) +
    geom_point(aes(color = name), alpha = .6, size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = 1:nmod) +
    labs(x = paste0(toupper(fm), " cutoff"),
         y = "Fitting Propensity Rank") +
    scale_color_brewer(palette = mod.brewer.pal) +
    guides(color = guide_legend(title = "Model")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      text = element_text(family = "sans"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(size = 10),
      panel.border = element_blank()
    )

  if (lower.tail[m]) {
    graph <- graph + scale_x_continuous(limits = c(xlim[1], xlim[2]), breaks = xseq)
  } else {
    graph <- graph + scale_x_reverse(limits = c(xlim[2], xlim[1]), breaks = xseq)
  }


  return(graph)
}

#' Plot of pairwise fitting propensity for a specific fit index across possible values
#'
#' @param x Data frame of specific fit index values extracted from object of class fitprop, as created by run.fitprop function.
#' @param m Integer value indicating location of fit index in list of fit indices to plot
#' @param fm Character value indicating which fit index to plot
#' @param whichmod Index number corresponding to which model(s) to include on the plot. Defaults to all models.
#' @param savePlot Logical value indicating whether to save plot to a list (TRUE) or just produce plot to output.
#' @param xlim Numeric vector of length 2 indicating the limits for the x-axis of the plot.
#' @param samereps Logical value indicating whether to use only results from replications in which all selected models yielded results.
#' @param lower.tail Logical vector indicating whether lower values of each fit index corresponds to good fit.
#' @param mod.lab Optional character vector of labels for each model.
#' @param mod.brewer.pal Optional string corresponding to the palette from RColorBrewer to use for the different models. e.g.,
#'   see \code{\link[RColorBrewer]{display.brewer.all}}.
#'
#' @returns A ggplot object of the pairwise FP comparison plot
#'
#'
#' @export
#' @import ggplot2
#' @importFrom stats ecdf
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom RColorBrewer brewer.pal

pairwise.fitprop <- function(x,
                             m = m,
                             fm = fm,
                             whichmod = NULL,
                             savePlot = FALSE,
                             xlim = c(0, 1),
                             samereps = TRUE,
                             lower.tail = NULL,
                             mod.lab = NULL,
                             mod.brewer.pal = "Set1") {

  # Define binding for global variables (to avoid note)
  ymax <- group <- NULL

  dat <- x
  #dat <- dat[, whichmod]
  nmod <- ncol(dat)
  nrep <- nrow(dat)

  # From here on is ECDF specific code
  if (samereps) {
    dat <- na.omit(dat)
  }

  color.pal <- brewer.pal(n = max(3, nmod), name = mod.brewer.pal)

  xseq <- seq(0, 1, .001)

  if(lower.tail[m]) {
    decdf <- ecdf(dat[,1])(xseq) - ecdf(dat[,2])(xseq)
  } else {
    decdf <- ecdf(dat[,2])(xseq) - ecdf(dat[,1])(xseq)
  }

  tmp <- data.frame("xseq" = xseq, "decdf" = decdf)
  fp_prefer <- data.frame(ymax = c(Inf, -Inf), group = mod.lab)

  graph <- ggplot() +
    geom_rect(data = fp_prefer, aes(xmin = 0, xmax = 1, ymin = 0, ymax = ymax, fill = group), alpha = .5) +
    scale_fill_brewer(name = "Model with\nhigher FP", palette = "Greys") +
    geom_line(data = tmp, aes(x = xseq, y = decdf),
              color = color.pal[1], size = 0.8) +
    labs(x = paste0(toupper(fm), " value"),
         y = "Difference in Cumulative Density") +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      text = element_text(family = "sans"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(size = 10),
      panel.border = element_blank()
    )

  if (lower.tail[m]) {
    graph <- graph + scale_x_continuous(breaks = seq(xlim[1], xlim[2], .1))
  } else {
    graph <- graph + scale_x_reverse(breaks = seq(xlim[1], xlim[2], .1))
  }


  return(graph)

}
