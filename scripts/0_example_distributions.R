library(tidyverse)
library(PearsonDS)
library(e1071)
pearsonMoments()
#dist = rpearson(n = 100,moments = c(1,1,0,3)) #note that rpearson does not use excess kurtosis

#what are moments of a normal dist?
#mean = 1
#var = 1
#skewness = 0
#kurt = 3 (or 0 for excess)


#mean

  set.seed(1984) # Because Orwell, and the Eurythmics
  mean_ex <-
  bind_rows(
  
  data.frame(ex = "2",
             dist = rpearson(n = 10000,moments = c(-1,1,0,3))),
  data.frame(ex = "1",
             dist = rpearson(n = 10000,moments = c(0,1,0,3))),
  data.frame(ex = "3",
             dist = rpearson(n = 10000,moments = c(1,1,0,3))))


  mean_plot <-
    ggplot(data = mean_ex,
           mapping = aes(x=dist,
                         linetype=ex))+
      geom_density(outline.type = "upper",bw=.5,size=2)+
      xlab(NULL)+
      ylab(NULL)+
      theme(axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none")
  
    ggsave(plot = mean_plot,
           filename = here::here("images/box_mean.png"),
           height = 100, width = 100,
           units = "mm", dpi = 600)
  
# variance

  set.seed(1984)
  var_ex <-
    bind_rows(
      
      data.frame(ex = "2",
                 dist = rpearson(n = 10000,moments = c(0,.1,0,3))),
      data.frame(ex = "1",
                 dist = rpearson(n = 10000,moments = c(0,1,0,3))),
      data.frame(ex = "3",
                 dist = rpearson(n = 10000,moments = c(0,2,0,3))))
  
  variance_plot <-
  ggplot(data = var_ex,
         mapping = aes(x=dist,
                       linetype=ex))+
    geom_density(outline.type = "upper",bw=.5,size=2)+
    xlab(NULL)+
    ylab(NULL)+
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") 
  
  ggsave(plot = variance_plot,
         filename = here::here("images/box_variance.png"),
         height = 100, width = 100,
         units = "mm", dpi = 600)
  


# skewness

  set.seed(1984)
  skew_ex <-
    bind_rows(
      
      data.frame(ex = "2",
                 dist = rpearson(n = 10000,moments = c(0,1,-1,3))),
      data.frame(ex = "1",
                 dist = rpearson(n = 10000,moments = c(0,1,0,3))),
      data.frame(ex = "3",
                 dist = rpearson(n = 10000,moments = c(0,1,1,3))))
  
  skew_plot <-
  ggplot(data = skew_ex,
         mapping = aes(x=dist,
                       linetype=ex))+
    geom_density(outline.type = "upper",bw=.5,size=2)+
    xlab(NULL)+
    ylab(NULL)+
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") 
  
  ggsave(plot = skew_plot,
         filename = here::here("images/box_skewness.png"),
         height = 100, width = 100,
         units = "mm", dpi = 600)
  
  
  
# kurtosis
  
  set.seed(1984)
  
  kurt_ex <-
    bind_rows(
      
      data.frame(ex = "2",
                 dist = rpearson(n = 10000,moments = c(0,1,0,1.1))),
      data.frame(ex = "1",
                 dist = rpearson(n = 10000,moments = c(0,1,0,3))),
      data.frame(ex = "3",
                 dist = rpearson(n = 10000,moments = c(0,1,0,6))))
  
  kurt_plot <- 
  ggplot(data = kurt_ex,
         mapping = aes(x=dist,
                       linetype=ex))+
    geom_density(outline.type = "upper", bw = .5, size = 2)+
    xlab(NULL)+
    ylab(NULL)+
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") 
  
  ggsave(plot = kurt_plot,
         filename = here::here("images/box_kurtosis.png"),
         height = 100, width = 100,
         units = "mm", dpi = 600)
  
  

#cullen frey plot
  
  
  set.seed(1984)
  cf_ex <-
    bind_rows(
      
      data.frame(ex = "2",
                 dist = rpearson(n = 10000,moments = c(0, 1, -1, 3))), #high skew, low kurt
      data.frame(ex = "1",
                 dist = rpearson(n = 10000,moments = c(0, 1, 0, 3))), #normal
      data.frame(ex = "3",
                 dist = rpearson(n = 10000,moments = c(0, 1, 1, 4))),
      data.frame(ex = "4",
                 dist = rpearson(n = 10000,moments = c(0, 1, 1.5, 5))),
      data.frame(ex = "5",
                 dist = rpearson(n = 10000,moments = c(0, 1, 1.8, 6))),
      data.frame(ex = "6",
                 dist = rpearson(n = 10000,moments = c(0, 1, 2, 7))),
      data.frame(ex = "7",
                 dist = rpearson(n = 10000,moments = c(0, 1, -1, 2.5))),
      data.frame(ex = "8",
                 dist = rpearson(n = 10000,moments = c(0, 1, -1.1, 2.4))),
      data.frame(ex = "9",
                 dist = rpearson(n = 10000,moments = c(0, 1, 1.4, 3.9))),
      data.frame(ex = "10",
                 dist = rpearson(n = 10000,moments = c(0, 1, 1.7, 7)))
      
      
      
      )
  
  ggplot(data = cf_ex,
         mapping = aes(x=dist,
                       linetype=ex))+
    geom_density(outline.type = "upper", bw = .5, size = 2)+
    xlab(NULL)+
    ylab(NULL)+
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") 
  
  
  cf_plot <-
  cf_ex %>%
    group_by(ex)%>%
    summarise(skewnness = skewness(dist),
              kurtosis = kurtosis(dist))%>%
    mutate(skewness_2 = skewnness^2)%>%
    ggplot(mapping = aes(x=skewness_2,y = kurtosis))+
    geom_point(size=5)+
    xlab(bquote(~skewness^2))+
    ylab("kurtosis")+
    theme(axis.line = element_line(size = 2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_text(size = 30))

  
  ggsave(plot = cf_plot,
         filename = here::here("images/box_skewness_plus_kurtosis.png"),
         height = 100, width = 100,
         units = "mm", dpi = 600)
  
  
  