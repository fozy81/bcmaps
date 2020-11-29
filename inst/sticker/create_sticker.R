library(hexSticker)
library(ggplot2)
library(scotmaps)
library(sf)
library(dplyr)

la <- local_authorities()

p <- ggplot() +
  geom_sf(data = la, aes(fill = local_authority), size = 0.05) +
  guides(fill = FALSE) +
  scale_fill_discrete() +
  theme_void() +
  theme_transparent() +
  theme(panel.grid.major = element_line(colour = "grey60", size = 0.1))

sticker(p, package = "scotmaps",
        p_size = 11,
        p_y = 0.4, p_color = "#003C71", p_family = "sans",
        s_x = 1, s_y = 1.05,
        s_width = 1.3, s_height = 1.3,
        h_fill = "white", h_color = "#003C71",
        filename = file.path("inst/sticker/scotmaps.png"))
