##
##install packages if not already done so (uncomment)
##
# install.packages("circlize")

library("circlize")

##
##load data (df0: 2010-15 global flows, df1: meta data for plot)
##global flow data based on estimates from:
##
#
# Abel, G.J. (2016) Estimates of global bilateral migration glows by gender between 1960 and 2015. 
# Vienna Institute of Demography Working Papers 2/2016.
#

df0 <- read.csv(system.file("vidwp", "reg_flow.csv", package = "migest"), stringsAsFactors=FALSE)
df1 <- read.csv(system.file("vidwp", "reg_plot.csv", package = "migest"), stringsAsFactors=FALSE)

##
##default chord diagram
##

chordDiagram(x = df0)

##
##plot parameters
##

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

#increasing the gaps between sectors, start at 12 o'clock, ensure no gap between the chord and the sector at the begining
# subdue warning messages and have no margins in the plot

##
##chord diagram with user selected adjustments for bilateral migration flow data
##

chordDiagram(x = df0, grid.col = df1$col, transparency = 0.25,
             order = df1$region, directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

# First line of arguments reads in the data (df0) and sets the colours base on the meta data (df1).
# Second line provides the order of outer sectors and indicates that chords should be directional.
# Third line indicates that the direction of the chords will be illustrated by both arrows and a difference in height. The
#  height difference is negative to make the chord shorter at the end (with the arrow head).
# Fourth line ensures the annotations outside the sectors are not plotted, but provides a track measures to later add 
#  annotatinos such as labels and axis (see below).
# Fifth line indicates the plot should use big arrows, sort the chords left to right in each sector and 
#  plots the smallest chords first.

##
##add in labels and axis
##

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = df1$reg1[df1$region == sector.index]
    reg2 = df1$reg2[df1$region == sector.index]
    
    circos.text(x = mean(xlim), y = ifelse(test = nchar(reg2) == 0, yes = 5.2, no = 6.0), 
                labels = reg1, facing = "bending", cex = 1.4)
    circos.text(x = mean(xlim), y = 4.4, 
                labels = reg2, facing = "bending", cex = 1.4)
    circos.axis(h = "top", 
                major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
                minor.ticks = 1, major.tick.percentage = 0.5,
                labels.niceFacing = FALSE)
  }
)

# First line indicates that first track (rather than any other that may have been created) will be used.
# Second line ensures no borders are plotted on the track.
# Third line adds a track.
# Fourth and fifth line collect individual track meta data from plot object.
# Sixth and seventh line collect matching name information from plot data frame (df1).
# The first circos.text adds text from (reg1) either at y = 6 (if there is a second part of the name in reg2) or 5.2.
# The second circost.text adds text (reg2).
# The circos.axis add axis with major and minor ticks, without flipping the axis labels in the bottom half.

##
##Printing
##

# text(x = -1.1, y = -1, pos = 4, cex = 0.6, 
#      labels = "Based on estimates from:")
# text(x = -1.1, y = -1 - 1*0.03, pos = 4, cex = 0.6, 
#      labels = expression(paste(
#        plain(" Abel G.J. (2016) "), italic("Estimates of Global Bilateral Migration Flows by Gender")
#      )))
# text(x = -1.1, y = -1 - 2*0.03, pos = 4, cex = 0.6, 
#      labels = expression(paste(
#          italic(" between 1960 and 2015. "), plain("Vienna Institute of Demography Working Papers. 2/2016")
#      )))
# 
# dev.copy2pdf(file = "cfplot_reg2.pdf", height=10, width=10)
# file.show("./cfplot_reg2.pdf")
#  
# dev.print(png, file = "cfplot_reg2.png", height=10, width=10, units = "in", res=500)
# file.show("cfplot_reg2.png")
