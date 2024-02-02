p0 <- c("#40A4D8","#33BEB7","#B2C224","#FECC2F","#FBA127","#F66320","#DB3937", "#A463D7", "#0C5BCE")
png(filename = "./hex/pie.png", width = 1200, height = 1200)
par(mar = rep(0, 4))
pie(rep(1,length(p0)), col = p0, border = "transparent", labels = NA)
dev.off()

# swirl pie chart using:
# https://www.gifgit.com/image/swirl-image
# radius = 100, angle = 0

library(magick)
library(sysfonts)
library(hexSticker)
font_add("harlow", "HARLOWSI.TTF")

s <- image_read(path = "hex/pie_swirl.png")
sticker(s, package="migest",
        p_size = 50, p_color = "grey20",
        p_family = "harlow",
        p_y = 1.1,
        s_x = 1, s_y = 1,
        s_width=3, s_height=3,
        filename="./hex/logo.png",
        # spotlight = TRUE, l_x = 0.25,
        h_color = "grey20",
        h_fill = "grey20", white_around_sticker = TRUE)

file.show("./hex/logo.png")


p <- image_read("hex/logo.png")
pp <-
  p %>%
  image_rotate(180) %>%
  image_chop(geometry = "x1") %>%
  image_rotate(180) %>%
  image_extent(geometry = "518x600", gravity = "north") %>%
  image_fill(color = "transparent", refcolor = "white",
             fuzz = 75, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white",
             fuzz = 75, point = "+517+1") %>%
  image_fill(color = "transparent", refcolor = "white",
             fuzz = 75, point = "+1+590") %>%
  image_fill(color = "transparent", refcolor = "white",
             fuzz = 75, point = "+510+590")
image_write(image = pp, path = "./hex/logo_transp.png")
file.show("./hex/logo_transp.png")

usethis::use_logo(img = "./hex/logo_transp.png")


# hex
# https://pngtree.com/freepng/cartoon-airplane-material-blue-jet-clipart_5862275.html
# https://www.pngegg.com/en/png-eehpm
# https://e7.pngegg.com/pngimages/292/743/png-clipart-mikoyan-gurevich-mig-17-mikoyan-gurevich-mig-15-shenyang-j-5-airplane-russian-aircraft-corporation-mig-airplane-angle-fighter-aircraft.png

# p <- image_read(path = "./hex/—Pngtree—cartoon airplane material blue jet_5862275.png")
# library(hexSticker)
# sticker(p, package="migest",
#         p_size = 45, p_color = "grey20",
#         p_family = "harlow",
#         p_y =1.45,
#         s_x = 1, s_y = 0.725,
#         s_width=1.4, s_height=1.4,
#         filename="./hex/logo.png",
#         # spotlight = TRUE, l_x = 0.25,
#         h_color = "grey20",
#         h_fill = "grey90")
# s <- image_read(path = "./hex/pie_swirl.png") %>%
#   image_resize("2400x2400")
# p <- image_read(path = "./hex/—Pngtree—cartoon airplane material blue jet_5862275.png")
#
# c(s,p) %>%
#   image_mosaic()
#

