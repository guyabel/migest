##
## load packages
##
# install.packages("tidyverse")
library(tidyverse)
library(migest)

##
## read in table and define matrix (m) and reference data.frame (df1)
##
science_reg <- read_tsv(system.file("science", "region_custom.txt", package = "migest"), 
                        skip = 1, n_max = 10)

# set up a region data frame
r <- science_reg %>%
  select(1:3) %>% 
  rename(order = 1, 
         rgb = 2,
         region = 3) %>% 
  arrange(order) %>% 
  separate(col = rgb, into = c("r","g","b")) %>% 
  mutate(col = rgb(red = r, green = g, blue = b, max = 255),
         label = str_replace_all(string = region, pattern = "_", replacement = " "))
r

# set up a flow data frame
d <- science_reg %>%
  rename(orig = 3) %>%
  select(-(1:2)) %>%
  pivot_longer(cols = -1, names_to = "dest", values_to = "flow") %>%
  mutate(flow = flow/1e6, 
         # drop small flows - not sure why we thought this was a good idea
         vis = flow > quantile(x = flow, 0.65))
d

##
## plot using mig_chord adaption of circlize::chordDiagram
##
pdf(file = "chord_reg.pdf")
mig_chord(x = d, 
          order = r$region,
          grid.col = r %>% 
            select(region, col) %>%
            deframe(), 
          lab_bend1 = r %>%
            select(region, label) %>%
            deframe(),
          label_size = 0.9,
          axis_breaks = 1,
          link.visible = d$vis
          )
dev.off()
file.show("chord_reg.pdf")

# ##
# ## Original code, pre circlize::chordDiagram and improvements in visualising 
# ## migration data using migest::mig_chord. Uncomment to run.
# ##
# ##
# ##install packages if not already done so (uncomment)
# ##
# # install.packages("circlize")
# # install.packages("plyr")
# 
# ##
# ##read in table and define matrix (m) and reference data.frame (df1)
# ##
# m<-read.table(system.file("science", "region_custom.txt", package = "migest"), skip=2, stringsAsFactors=FALSE)
# 
# #data.frame for details on each region
# df1<-m[,1:3]
# names(df1)<-c("order","rgb","region")
# df1$region<-gsub("_", " ", df1$region)
# 
# #flow matrix
# m<-m[,-(1:3)]/1e06
# m<-as.matrix(m)
# dimnames(m)<-list(orig=df1$region,dest=df1$region)
# 
# ##
# ##sort order of data.frame and matrix for plotting in circos
# ##
# library("plyr")
# df1<-arrange(df1, order)
# df1$region <- factor(df1$region, levels=df1$region)
# m<-m[levels(df1$region),levels(df1$region)]
# 
# ##
# ##define ranges of circos sectors and their colors (both of the sectors and the links)
# ##
# df1$xmin <- 0
# df1$xmax <- rowSums(m)+colSums(m)
# n<-nrow(df1)
# df1 <- cbind(df1, matrix(as.numeric(unlist(strsplit(df1$rgb,","))),nrow=n, byrow=TRUE) )
# names(df1)[ncol(df1)-2:0]<-c("r","g","b")
# df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
# df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)
# 
# ##
# ##plot sectors
# ##
# library("circlize")
# par(mar=rep(0,4))
# circos.clear()
# 
# #basic circos graphic parameters
# circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.1), start.degree = 90, gap.degree =4)
# 
# #sector details
# circos.initialize(factors = df1$region, xlim = cbind(df1$xmin, df1$xmax))
# 
# #plot sectors
# circos.trackPlotRegion(ylim = c(0, 1), factors = df1$region, track.height=0.1, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
#                        #panel.fun for each sector
#                        panel.fun = function(x, y) {
#                          #select details of current sector
#                          name = get.cell.meta.data("sector.index")
#                          i = get.cell.meta.data("sector.numeric.index")
#                          xlim = get.cell.meta.data("xlim")
#                          ylim = get.cell.meta.data("ylim")
#                        
#                          #plot country labels
#                          circos.text(x=mean(xlim), y=2.2, labels=name, facing = "bending", cex=0.8)
#                          
#                          #plot main sector
#                          circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
#                                      col = df1$rcol[i], border=df1$rcol[i])
#                          
#                          #blank in part of main sector
#                          circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3, 
#                                      col = "white", border = "white")
#                          
#                          #white line all the way around
#                          circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
#                          
#                          #plot axis
#                          circos.axis(labels.cex=0.8, major.at=seq(0,floor(df1$xmax)[i]), minor.ticks=1, labels.niceFacing = FALSE,
#                                      labels.away.percentage = 0.15)
#                        })
# 
# 
# ##
# ##plot links
# ##
# #add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
# df1$sum1 <- colSums(m)
# df1$sum2 <- numeric(n)
# 
# #create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
# df2<-cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
# df2<-reshape(df2, idvar="orig", varying=list(1:n), direction="long", timevar="dest", time=rownames(m),  v.names = "m")
# df2<-arrange(df2,desc(m))
# 
# #keep only the largest flows to avoid clutter
# df2<-subset(df2, m>quantile(m,0.65))
# 
# circos.par(track.margin=c(0,0))
# 
# for(k in 1:nrow(df2)){
#   #i,j reference of flow matrix
#   i<-match(df2$orig[k],df1$region)
#   j<-match(df2$dest[k],df1$region)
#   
#   #plot link
#   circos.link(sector.index1=df1$region[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
#               sector.index2=df1$region[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
#               col = df1$lcol[i])
#   
#   #update sum1 and sum2 for use when plotting the next link
#   df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
#   df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
# }
# 
# #remove created objects
# rm(df1,df2,m,i,j,k,n)
