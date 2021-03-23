library(ggplot2)
library(extrafont)
library(sysfonts)
library(showtext)
library(gggibbous)
library(ggtext)

##Fonts

font_add_google("Noto Sans",
                "Noto")

font_paths()  
font_files()
font_families()

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

##Theme for plotting

figure_theme = 
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.1),
      axis.text.x = element_text(size = 11, face = "bold", color = "grey65"),
      axis.title.x = element_text(color = "grey65"),
      axis.text.y = element_text(size = 9, color = "grey65"),
      axis.line = element_blank(),
      plot.background = element_rect(fill = "#141438", colour = NA),
      panel.background = element_rect(fill = "#141438", colour = NA),
      legend.background = element_rect(fill = "#141438", colour = NA),
      legend.position = 'bottom',
      legend.key = element_blank(),
      legend.text = element_text(color = "grey65"),
      legend.title = element_text(color = "grey65"),
      text = element_text(family = "Noto", color = "grey65"), #font change
      plot.margin = margin(25, 25, 10, 25))

##Colour palete
colors <- c("Cross-Site CWM" = "#F235B0",
            "Site-Specific CWM" = "#65B4E6",
            "Parametric BS" = "#D4F294",
            "Non-Parametric BS" = "#F29472",
            "True" = "#ACBAF5")

# Join colors with categories
pal_df <- data.frame(c = unname(colors)[1:4], l = names(colors)[1:4])

##For facet labels

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

##Manual Legend for moon plots
moon_legend = 
ggplot(data.frame(y = c(1,1.5,2,2.5), 
                  x = 0, ratio = 1:4 * 0.25),
       aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
  geom_text(aes(x = x + 0.6,
                label = paste0(ratio*100,"%")),
            size = 3,
            colour = "grey65",
            family = "Noto") +
  coord_fixed() +
  ggtitle("Value in CI") +
  lims(y = c(0.5, 2.7), x = c(-1, 2)) +
  theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      halign = 0,
                                      size = rel(1.05)),
        plot.title.position = "panel",
        text = element_text(colour = "grey65",
                            family = "Noto"))
