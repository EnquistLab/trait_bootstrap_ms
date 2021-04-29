#Stupid elevation thing


atraits <- readRDS(file = "data/all_traits_unscaled_RMBL.rds")

#Removed 7% of records with any missing data
message(paste(round(nrow(na.omit(atraits))/nrow(atraits)*100),"percent complete data. Pruning incomplete rows."))
atraits <- na.omit(atraits)

#correct unit error
atraits$dry_mass_mg[which(atraits$dry_mass_mg==7000)] <- 7

#Add LMA and remove SLA
atraits$LMA_mg_mm2 <- atraits$dry_mass_mg/atraits$leaf_area_mm2
atraits <- atraits[grep(pattern = "SLA",x = colnames(atraits),invert = T)]

#Add individual ID column and covert to long form
atraits$ID <- 1:nrow(atraits)


#Convert to tidy/skinny/long form
atraits <- gather(data = atraits,key = "trait","value",3:7)

#log transform
#ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
#atraits$value <- log10(atraits$value)
#ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")



#Scale and center traits  
# atraits <- atraits %>%
#   group_by(trait) %>% 
#   mutate(value = as.numeric(scale(value))) %>% 
#   ungroup() # %>% group_by(trait) %>% summarize(mean=mean(value),sd=sd(value))

#Get community data
community <- atraits %>% 
  group_by(taxon,site) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")


species_list <- unique(community$taxon)

library(BIEN)
bien_traits <- BIEN_trait_list()
bien_traits
unique(atraits$trait)

btraits <-
BIEN_trait_traitbyspecies(species = species_list,
                          trait = c("whole plant height","leaf area per leaf dry mass" ))


#scale a to match b
unique(btraits$unit)
unique(atraits$trait)
#height: amanda = cm, bien = m
#lma = amanda= m2/kg-1, bien = 


btraits %>%
  select(scrubbed_species_binomial,trait_name,trait_value,unit)%>%
  mutate(trait_val_numeric = as.numeric(trait_value))%>%
  #this converts height to the correct scale and sla to lma
  #no further conversion needed on lma, since kg/m^2 = mg/mm^2
  mutate(rmbl_scale = case_when(trait_name == "whole plant height" ~ trait_val_numeric*100 ,
                                trait_name == "leaf area per leaf dry mass"  ~ (1/trait_val_numeric)))%>%
  mutate(rmbl_name = case_when(trait_name == "whole plant height" ~ "height",
                               trait_name == "leaf area per leaf dry mass" ~ "LMA_mg_mm2" )) -> btraits

source("r_functions/draw_traits_tidy.R")

set.seed(2005)

field_traits <- draw_traits_tidy(tidy_traits = atraits,sample_size = 9)
colnames(field_traits)


btraits %>%
  mutate(site = "unknown",
         taxon = scrubbed_species_binomial,
         ID = 1:n(),
         trait = rmbl_name,
         value = rmbl_scale) -> bien_traits

# Do imputing
field_imputed <-
trait_impute(comm = community,
             traits = atraits,
             scale_hierarchy = "site",
             global = TRUE,
             taxon_col = "taxon",
             abundance_col = "abundance",
             trait_col = "trait",
             value_col = "value",
             min_n_in_sample = 1)

global_imputed <-
  trait_impute(comm = community,
               traits = bien_traits,
               scale_hierarchy = "site",
               global = TRUE,
               taxon_col = "taxon",
               abundance_col = "abundance",
               trait_col = "trait",
               value_col = "value",
               min_n_in_sample = 1)

#Do bootstrapping
field_bootstrapped <-
trait_np_bootstrap(imputed_traits = field_imputed,
                   nrep = 200,
                   sample_size = 200)

global_bootstrapped <-
  trait_np_bootstrap(imputed_traits = field_imputed,
                     nrep = 200,
                     sample_size = 200)




#Do summarizing
field_summary <-
trait_summarise_boot_moments(bootstrap_moments = field_bootstrapped,
                             parametric = T,
                             ci = 0.95)

global_summary <-
  trait_summarise_boot_moments(bootstrap_moments = global_bootstrapped,
                               parametric = T,
                               ci = 0.95)


atraits %>% 
  group_by(site,trait) %>%
  summarise(true_mean=mean(value),
            true_variance=var(value),
            true_skewness = skewness(value),
            true_kurtosis = e1071::kurtosis(value)) -> true_moments


summary_combined <-
  rbind(data.frame(source = "field", field_summary),
        data.frame(source = "global", global_summary))



#Next, append true moments to output data for convenience
summary_combined <- merge(x = summary_combined,
                y = true_moments,
                by = c("site","trait"))


bootstrapped_combined <-
  merge(x = bootstrapped_combined,
        y = true_moments,
        by = c("site","trait"))


saveRDS(object = summary_combined,file = "output_data/global_vs_local_summary.RDS")

saveRDS(object = summary_combined,file = "output_data/global_vs_local_not_summarized.RDS")


##########################################################
library(ape)

list.files("C:/Users/Brian Maitner/Desktop/current_projects/")






#note: log scale before doing traitstrap








