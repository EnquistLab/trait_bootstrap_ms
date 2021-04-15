library(ggplot2)
library(extrafont)
library(sysfonts)
library(showtext)
library(gggibbous)
library(ggtext)
library(patchwork)
library(ggridges)
library(ggbump)
library(ggfx)

##Fonts----

font_add_google("Noto Sans",
                "Noto")

font_paths()  
font_files()
font_families()

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

##General Theme for plotting----

figure_theme = 
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1),
    axis.text.x = element_text(color = "grey65", size = rel(.7)),
    axis.text.y = element_text(color = "grey65", size = rel(.7)),
    axis.title.x = element_text(color = "grey65", size = rel(.9)),
    axis.title.y = element_text(color = "grey65", size = rel(.9)),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#141438", colour = NA),
    panel.background = element_rect(fill = "#141438", colour = NA),
    legend.background = element_rect(fill = "#141438", colour = NA),
    legend.position = 'bottom',
    legend.key = element_blank(),
    legend.text = element_text(color = "grey65", size = rel(.6)),
    legend.title = element_text(color = "grey65", size = rel(.7)),
    text = element_text(family = "Noto", color = "grey65"), #font change
    plot.margin = margin(15, 5, 10, 15)
    )


##a--🍩 Theme----

theme_doughnut =
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#141438",
                                   colour = NA),
    panel.background = element_rect(fill = "#141438",
                                    colour = NA),
    strip.text.y.left = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(1), face = "bold", vjust = 0,
                                colour = "grey65", angle = 0),
    strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                    size = rel(1),
                                    colour = "grey65", face = "bold"),
    legend.text = element_text(color = "grey65", size = rel(.7)),
    legend.title = element_text(color = "grey65", size = rel(.9)),
    plot.margin = margin(15, 19, 10, 15),
    legend.position = 'bottom'
  )

##b--🌖 Theme----

theme_moon = 
  figure_theme +
  theme(
    legend.position = 'right',
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.9), face = "bold",
                                colour = "grey65"),
    strip.text.x.top = element_text(margin = margin(0, 0, 5, 0),
                                    size = rel(.9),
                                    colour = "grey65", face = "bold"),
    panel.grid.major.y = element_line(size = 0.05,
                                      colour = "grey65"),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 0.6),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0, 0, 10, 0),
                              size = 15, face = "bold",
                              colour = "grey65")
  ) 

##c--🍭Theme----

theme_lollipop = 
  figure_theme +
  theme(axis.text.y = element_blank(),
        panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                        size = 0.6),
        strip.text.y = element_text(margin = margin(0, 10, 0, 0),
                                    size = rel(1), face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = rel(1),
                                        colour = "grey65", face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = "grey65", size = rel(.7)),
        legend.title = element_text(color = "grey65", size = rel(.9)))

##Colour palete----
colors <- c("Cross-Site CW" = "#F235B0",
            "Site-Specific CW" = "#65B4E6",
            "Parametric BS" = "#D4F294",
            "Non-Parametric BS" = "#F29472",
            "True" = "#ACBAF5")

# Join colors with categories
pal_df <- data.frame(c = unname(colors)[1:4], l = names(colors)[1:4])

##For facet labels----

#To capitalise labels
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

#rename traits
traits_parsed <- c(
  biomass_per_ind = "Biomass",
  dry_mass_mg = "Dry mass",
  height = "Height",
  leaf_area_mm2 = "Leaf area",
  LMA_mg_mm2 = "LMA"
)

traits_panama <- c(
  Area = "Area",
  Dry.weight = "Dry weight",
  Fresh.weight = "Fresh weight",
  LDMC = "LDMC",
  LMA = "LMA",
  LCC = "Carbon content",
  LNC = "Nitrogen content",
  N.C = "Nitrogen:Carbon"
)

## Manual Legend for moon plots----
moon_legend = 
  ggplot(data.frame(y = c(1,1.5,2,2.5), 
                    x = 0, ratio = 1:4 * 0.25),
         aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
  geom_text(aes(x = x + 0.7,
                label = paste0(ratio*100,"%")),
            size = 4,
            colour = "grey65",
            family = "Noto") +
  coord_fixed() +
  ggtitle("Value in CI") +
  lims(y = c(0.5, 2.7), x = c(-0.5, 2.5)) +
  theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      halign = 0,
                                      size = rel(1.7)),
        plot.title.position = "panel",
        text = element_text(colour = "grey65",
                            family = "Noto"))

## Inset images ----
img1 = png::readPNG("images/Colorado.png")
img2 = png::readPNG("images/Frogs.png")
img3 = png::readPNG("images/Panama.png")
img4 = png::readPNG("images/AZ.png")
img5 = png::readPNG("images/Phyto.png")
