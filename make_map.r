#downloading and loading packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot, googleway, ggplot2, ggrepel,ggspatial, sf, rnaturalearth,
 rnaturalearthdata, install = TRUE, update = getOption("pac_update"), 
 character.only = FALSE)


#getting data to make world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#making a map
ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico",
    fontface = "italic", color = "#383838", size = 6)

#hashtag makes that line of code not run (called comments)
#uncommment this and rename it to save your map!)

ggsave("gulf_of_mexico.pdf")
