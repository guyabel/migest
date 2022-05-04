#' Chord diagram for directional origin-destination data
#' 
#' Adaption of \code{circlize::chordDiagramFromDataFrame()} with defaults set to allow for more effective visualisation of directional origin-destination data
#'
#' @param x Data frame with origin in first column, destination in second column and bilateral measure in third column
#' @param lab Named vector of labels for plot. If \code{NULL} will use names from \code{d}
#' @param lab_bend1 Named vector of bending labels for plot. Note line breaks do not work with \code{facing = "bending"} in circlize.
#' @param lab_bend2 Named vector of second row of bending labels for plot. 
#' @param label_size Font size of label text.
#' @param label_nudge Numeric value to nudge labels towards (negative number) or away (positive number) the sector axis.
#' @param label_squeeze Numeric value to nudge \code{lab_bend1} and \code{lab_bend2} labels apart (negative number) or together (positive number).  
#' @param axis_size Font size on axis labels.
#' @param axis_breaks Numeric value for how often to add axis label breaks. Default not activated, uses default from \code{circlize::circos.axis()}
#' @param ... Arguments for \code{circlize::chordDiagramFromDataFrame()}.
#' @param no_labels Logical to indicate if to include plot labels. Set to \code{FALSE} by default.
#' @param no_axis Logical to indicate if to include plot axis. Set to \code{FALSE} by default.
#' @param zero_margin Set margins of the plotting graphics device to zero. Set to \code{TRUE} by default.
#' @param clear_circos_par Logical to run \code{circlize::circos.clear()}. Set to \code{TRUE} by default. Set to \code{FALSE} if you wish to add further to the plot.
#' @param start.degree Argument for \code{circlize::circos.par()}.
#' @param gap.degree Argument for \code{circlize::chordDiagramFromDataFrame()}.
#' @param track.margin Argument for \code{circlize::chordDiagramFromDataFrame()}.
#' @param points.overflow.warning Argument for \code{circlize::chordDiagramFromDataFrame()}.
#'
#' @return Chord diagram based on first three columns of \code{x}. The function tweaks the defaults of \code{circlize::chordDiagramFromDataFrame()} for easier plotting of directional origin-destination data. Users can override these defaults and pass additional tweaks using any of the \code{circlize::chordDiagramFromDataFrame()} arguments.
#' 
#' The layout of the plots are designed to specifically work on plotting images into PDF devices with widths and heights of 7 inches (the default dimension when using the \code{pdf} function). See the end of the examples for converting PDF to PNG images in R. 
#' 
#' Fitting the sector labels on the page is usually the most time consuming task. Use the different label options, including line breaks, \code{label_nudge}, track height in \code{preAllocateTracks} and font sizes in \code{label_size} and \code{axis_size} to find the best fit. If none of the label options produce desirable results, plot your own using \code{circlize::circos.text} having set \code{no_labels = TRUE} and \code{clear_circos_par = FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(countrycode)
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/26239945")
#' 
#' # use dictionary to get region to region flows
#' d <- f %>%
#'   mutate(
#'     orig = countrycode(
#'       sourcevar = orig, custom_dict = dict_ims,
#'       origin = "iso3c", destination = "region"),
#'     dest = countrycode(
#'       sourcevar = dest, custom_dict = dict_ims,
#'       origin = "iso3c", destination = "region")
#'   ) %>%
#' group_by(year0, orig, dest) %>%
#' summarise_all(sum) %>%
#' ungroup()
#' d
#'
#' # 2015-2020 pseudo-Bayesian estimates for plotting
#' pb <- d %>%
#'     filter(year0 == 2015) %>%
#'     mutate(flow = da_pb_closed/1e6) %>%
#'     select(orig, dest, flow)
#' pb
#'     
#' # pdf(file = "chord.pdf")
#' mig_chord(x = pb)
#' # dev.off()
#' # file.show("chord.pdf")
#' 
#' # pass arguments to circlize::chordDiagramFromDataFrame
#' # pdf(file = "chord.pdf")
#' mig_chord(x = pb, 
#'           # order of regions
#'           order = rev(unique(pb$orig)),
#'           # spacing for labels
#'           preAllocateTracks = list(track.height = 0.3),
#'           # colours
#'           grid.col = c("blue", "royalblue", "navyblue", "skyblue", "cadetblue", "darkblue")
#'           ) 
#' # dev.off()
#' # file.show("chord.pdf")
#' 
#' # multiple line labels to fit on longer labels
#' r <- pb %>%
#'   sum_region() %>%
#'   mutate(lab = str_wrap_n(string = region, n = 2)) %>%
#'   separate(col = lab, into = c("lab1", "lab2"), sep = "\n", remove = FALSE, fill = "right")
#' r
#' 
#' # pdf(file = "chord.pdf")
#' mig_chord(x = pb,
#'           lab = r %>%
#'             select(region, lab) %>%
#'             deframe(),
#'           preAllocateTracks = list(track.height = 0.25),
#'           label_size = 0.8,
#'           axis_size = 0.7
#'           )
#' # dev.off()
#' # file.show("chord.pdf")
#' 
#' # bending labels
#' # pdf(file = "chord.pdf")
#' mig_chord(x = pb,
#'           lab_bend1 = r %>%
#'             select(region, lab1) %>%
#'             deframe(),
#'           lab_bend2 = r %>%
#'             select(region, lab2) %>%
#'             deframe()
#'           )
#' # dev.off()
#' # file.show("chord.pdf")
#' 
#' 
#' # convert pdf to image file
#' # library(magick)
#' # p <- image_read_pdf("chord.pdf")
#' # image_write(image = p, path = "chord.png")
#' # file.show("chord.png")
mig_chord <- function(
  x, 
  lab = NULL,
  lab_bend1 = NULL,
  lab_bend2 = NULL,
  label_size = 1, 
  label_nudge = 0,
  label_squeeze = 0,
  axis_size = 0.8,
  axis_breaks = NULL,
  ..., 
  no_labels = FALSE,
  no_axis = FALSE,
  clear_circos_par = TRUE,
  zero_margin = TRUE,
  start.degree = 90, 
  gap.degree = 4,
  track.margin = c(-0.1, 0.1), 
  points.overflow.warning = FALSE
){
  z <- list(...)
  z$df <- x[, 1:3]
  if(is.null(z$transparency))
    z$transparency = 0.25
  if(is.null(z$directional))
    z$directional = 1
  if(is.null(z$direction.type))
    z$direction.type = c("diffHeight", "arrows")
  if(is.null(z$link.arr.type))
    z$link.arr.type = "big.arrow"
  if(is.null(z$diffHeight))
    z$diffHeight = -0.05
  if(is.null(z$link.sort))
    z$link.sort = TRUE
  if(is.null(z$link.largest.ontop))
    z$link.largest.ontop = TRUE
  if(is.null(z$link.largest.ontop))
    z$link.largest.ontop = TRUE
  keep_axis <- TRUE
  if(!is.null(z$annotationTrack))
    if(!("axis" %in% z$annotationTrack))
      keep_axis <- FALSE
  if(no_axis)
    keep_axis <- FALSE
  if(is.null(z$annotationTrack))
    z$annotationTrack = "grid"
  
  # y <- list()
  # y[] <- z[!names(z) %in% formalArgs(chordDiagramFromDataFrame)]
  # z <- z[names(z) %in% formalArgs(chordDiagramFromDataFrame)]
  
  h <- 0.2
  if(!is.null(lab_bend1) | !is.null(lab_bend2))
    h <- 0.05
  if(is.null(z$preAllocateTracks))
    z$preAllocateTracks = list(track.height = h)

  if(is.null(z$grid.col)){
    region <- col <- NULL
    z$grid.col <- x %>%
      dplyr::rename(orig = 1, 
                    dest = 2, 
                    flow = 3) %>%
      sum_region() %>%
      dplyr::mutate(col = grDevices::colorRampPalette(migest::umbrella)(dplyr::n())) %>%
      dplyr::select(region, col) %>%
      tibble::deframe()
  }

  circlize::circos.clear()
  circlize::circos.par(
    start.degree = start.degree, 
    track.margin = track.margin, 
    gap.degree = gap.degree,
    points.overflow.warning = points.overflow.warning
  )
  if(zero_margin)
    graphics::par(mar = rep(0, 4))
  
  do.call(what = circlize::chordDiagramFromDataFrame, args = z)
  
  if(keep_axis){
    # if(is.null(y$h))
    #   y$h <- "bottom"
    # if(is.null(y$labels.cex))
    #   y$labels.cex <- axis_size
    circlize::circos.trackPlotRegion(
      track.index = 1, 
      bg.border = NA, 
      panel.fun = function(x, y) {
        if(is.null(axis_breaks))
          circlize::circos.axis(h = "bottom", labels.cex = axis_size, 
                                labels.niceFacing = FALSE)
          # do.call(what = circlize::circos.axis, args = y)
        if(!is.null(axis_breaks)){
          # if(is.null(y$major.at))
          #   y$major.at <- seq(from = 0, to = sum(z$df[,3]), by = axis_breaks)
          # do.call(what = circlize::circos.axis, args = y)
          mm <- sum(z$df[,3])
            if(!is.null(z$xmax))
              mm <- max(z$xmax)*2
          circlize::circos.axis(
            h = "bottom", labels.cex = axis_size, labels.niceFacing = FALSE,
            major.at = seq(from = 0, to = mm, by = axis_breaks)
          )
        }
      })
  }
    
  if(!no_labels){
    circlize::circos.trackPlotRegion(
      track.index = 1, 
      bg.border = NA, 
      panel.fun = function(x, y) {
        xx = circlize::get.cell.meta.data("xlim")
        s = circlize::get.cell.meta.data("sector.index")
        if(is.null(lab) & is.null(lab_bend1) & is.null(lab_bend2)){
          lab <- s
          names(lab) <- s
        }
        if(is.null(lab_bend1) & is.null(lab_bend2)){
          yy <- label_nudge + -0.5 + -0.55 * log(z$preAllocateTracks$track.height)
          if(!keep_axis)
            yy <- yy - 0.3
          circlize::circos.text(
            x = mean(xx), y = yy, labels = lab[s], cex = label_size,
            facing = "clockwise", adj = c(0, 0.5), niceFacing = TRUE)
        }
        if(!is.null(lab_bend1)){
          yy <- 3 + label_nudge
          if(is.null(lab_bend2))
            yy <- 2 +label_nudge
          if(!is.null(lab_bend2))
            if(is.na(lab_bend2[s]))
              yy <- 2 + label_nudge
          circlize::circos.text(
            x = mean(xx), y = yy, labels = lab_bend1[s], cex = label_size, 
            facing = "bending", niceFacing = FALSE)
        }
        if(!is.null(lab_bend2)){
          circlize::circos.text(
            x = mean(xx), y = 2 + label_nudge - label_squeeze, labels = lab_bend2[s], 
            cex = label_size, facing = "bending", niceFacing = FALSE)
        }
      }
    )
  }
  if(clear_circos_par)
    circlize::circos.clear()
}
# figuring out label distances relationship
# x = tibble(track = c(0.05, 0.1, 0.2), 
#        y = c(1.2, 0.6, 0.4))
# x %>%
#   ggplot(aes(x = track, y = y)) +
#   geom_point() +
#   scale_x_log10()
# 
# 
# lm(formula = y ~ log(track), data = x)
