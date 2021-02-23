
##############
#'@param imputed_traits Imputed traits object taken from trait_impute
#'@importFrom  Weighted.Desc.Stat w.mean w.var w.skewness w.kurtosis
#'@return A tibble. Should match the output structure from traitstrap
community_weighted_moments <- function(imputed_traits){
  
  attrib <- attr(imputed_traits,"attrib")
  #out <- 
  
  
  imputed_traits %>% 
    group_by_at(c(as.character(attrib$scale_hierarchy),attrib$trait_col)) %>% 
    summarise(mean = w.mean(.data[[attrib$value]],weight),
              variance = w.var(.data[[attrib$value]],weight),
              skewness = w.skewness(.data[[attrib$value]],weight),
              kurtosis = w.kurtosis(.data[[attrib$value]],weight),
              .groups="keep") %>% mutate(n=1)%>%relocate(n)-> out
  
  
  return(out)
  
  
  #for each hierarchy x trait combo, do cwm, etc
  
}  












