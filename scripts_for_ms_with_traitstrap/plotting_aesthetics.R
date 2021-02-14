library(ggplot2)

##Theme for plotting

figure_theme = 
  theme(panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.1),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 9, color = "grey65"),
      legend.position = 'bottom')

##Colour palete
colors <- c("Cross-Site CWM" = "#492259",
            "Site-Specific CWM" = "#D95284",
            "Non-parametric BS" = "#546FBF",
            "Parametric BS" = "#62F4B6",
            "True" = "#D98032")

# Join colors with categories
pal_df <- data.frame(c = unname(colors)[1:4], l = names(colors)[1:4])