library(ggplot2)

##Theme for plotting

figure_theme = 
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.1),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 9, color = "grey65"),
      plot.background = element_rect(fill = "#391959"),
      panel.background = element_rect(fill = "#391959"),
      legend.position = 'bottom',
      legend.key = element_blank())

##Colour palete
colors <- c("Cross-Site CWM" = "#F235B0",
            "Site-Specific CWM" = "#65B4E6",
            "Parametric BS" = "#D4F294",
            "Non-parametric BS" = "#F29472",
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
