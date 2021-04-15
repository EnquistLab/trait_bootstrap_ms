library(ggplot2)
library(extrafont)
library(sysfonts)
library(showtext)
library(gggibbous)
library(ggtext)
library(patchwork)
library(ggridges)

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
    panel.grid.major.y = element_line(size = 0.1, color = "#5e5e5e"),
    axis.text = element_text(size = rel(.7)),
    axis.title= element_text(size = rel(.9)),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom',
    legend.key = element_blank(),
    legend.text = element_text(size = rel(.6)),
    legend.title = element_text(size = rel(.7)),
    text = element_text(family = "Noto", color = "#5e5e5e"), #font change
    plot.margin = margin(15, 5, 10, 15)
    )


##a--🍩 Theme----

theme_doughnut =
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "white",
                                    colour = NA),
    strip.text.y.left = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(1), face = "bold", vjust = 0,
                                angle = 0),
    strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                    size = rel(1),face = "bold"),
    legend.text = element_text(size = rel(.7)),
    legend.title = element_text(size = rel(.9)),
    plot.margin = margin(15, 19, 10, 15),
    legend.position = 'bottom',
    text = element_text(family = "Noto", color = "#5e5e5e")
  )

##b--🌖 Theme----

theme_moon = 
  figure_theme +
  theme(
    legend.position = 'right',
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.9), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 5, 0),
                                    size = rel(.9),
                                    face = "bold"),
    panel.grid.major.y = element_line(size = 0.05),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                    size = 0.6),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0, 0, 10, 0),
                              size = 15, face = "bold")
  ) 

##c--🍭Theme----

theme_lollipop = 
  figure_theme +
  theme(axis.text.y = element_blank(),
        panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                        size = 0.6),
        strip.text.y = element_text(margin = margin(0, 10, 0, 0),
                                    size = rel(1), face = "bold"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = rel(1), face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(.7)),
        legend.title = element_text(size = rel(.9)))

##Colour palete----
colors <- c("Cross-Site CW" = "#ff4ccc",
            "Site-Specific CW" = "#4c79ff",
            "Parametric BS" = "#90ff4c",
            "Non-Parametric BS" = "#f3b155",
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
img1 = png::readPNG("images/Colorado_dark.png")
img2 = png::readPNG("images/Frogs_dark.png")
img3 = png::readPNG("images/Panama_dark.png")
img4 = png::readPNG("images/AZ_dark.png")
img5 = png::readPNG("images/Phyto_dark.png")
