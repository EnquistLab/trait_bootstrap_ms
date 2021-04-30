#elevation thing

#load traits
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

#toss traits we aren't using
atraits %>%
  filter(trait %in% c("LMA_mg_mm2","height" )) -> atraits

#check names
species_list <-unique(atraits$taxon)
TNRS_out <- 
  TNRS::TNRS(taxonomic_names = species_list)

species_list<-
merge(x = data.frame(taxon=species_list),
      y = TNRS_out[c("Name_submitted","Accepted_species")],
      by.x = 'taxon',
      by.y="Name_submitted")

#manually fix some

species_list$Accepted_species[which(species_list$taxon=="Viola nuttallianum")] <- "Viola nuttallii"
species_list$Accepted_species[which(species_list$taxon=="Carex sp")] <- "Carex"
species_list$Accepted_species[which(species_list$taxon=="Penstemon 99999")] <- "Penstemon"

#Get community data
community <- atraits %>% 
  group_by(taxon,site) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")

library(BIEN)
source("r_functions/get_consensus_family.R")

species_list <- 
  get_consensus_family(species_list = species_list,
                       taxon = "Accepted_species")

#Replace NA's with other information to prevent NA's from being matched
species_list$consensus_family[which(species_list$taxon=="Grass 1")] <- "Poaceae"
species_list$Accepted_species[which(species_list$taxon=="Grass 1")] <- "Grass 1"
species_list$genus[which(species_list$taxon=="Grass 1")] <- "Grass"


#update community with higher taxonomy stuff

community <- merge(x = community,
                   y = species_list,
                   by.x = "taxon",
                   by.y = "taxon")

community %>% mutate(family = consensus_family) -> community

#pull relevant traits for the families of interest
btraits_fam <- 
  BIEN_trait_traitbyfamily(family = species_list$consensus_family,
                            trait = c("whole plant height",
                                      "leaf area per leaf dry mass" ),
                           all.taxonomy = T)

#pull relevant traits for the genera of interest
btraits_gen <- 
  BIEN_trait_traitbygenus(genus = species_list$genus,
                           trait = c("whole plant height",
                                     "leaf area per leaf dry mass" ),
                          all.taxonomy = T)

#pull relevant traits for the species of interest
btraits_species <-
BIEN_trait_traitbyspecies(species = species_list$Accepted_species,
                          trait = c("whole plant height",
                                    "leaf area per leaf dry mass" ),
                          all.taxonomy = T)

#Note that hopefully the family level data contains the other two, but this way make sure we get everything

btraits <-
rbind(btraits_fam[colnames(btraits_species)],
      btraits_gen[colnames(btraits_species)],
      btraits_species)

btraits <- unique(btraits)
#rm(btraits_fam,btraits_gen)


#there seem to be some trait values of zero.  rounding error?  who knows.  toss em.
btraits <- btraits[which(btraits$trait_value > 0),]

#scale b to match a
#height: amanda = cm, bien = m
#lma = amanda= mg/km^22, bien = m2/kg
btraits %>%
  select(scrubbed_species_binomial,
         scrubbed_genus,
         scrubbed_family,
         trait_name,
         trait_value,
         unit)%>%
  mutate(trait_val_numeric = as.numeric(trait_value))%>%
  #this converts height to the correct scale and sla to lma
  #no further conversion needed on lma, since kg/m^2 = mg/mm^2
  mutate(rmbl_scale = case_when(trait_name == "whole plant height" ~ trait_val_numeric*100 ,
                                trait_name == "leaf area per leaf dry mass"  ~ (1/trait_val_numeric)))%>%
  mutate(rmbl_name = case_when(trait_name == "whole plant height" ~ "height",
                               trait_name == "leaf area per leaf dry mass" ~ "LMA_mg_mm2" )) -> btraits

source("r_functions/draw_traits_tidy.R")

set.seed(2005)

field_traits <- draw_traits_tidy(tidy_traits = atraits,
                                 sample_size = 9)



btraits %>%
  mutate(site = "unknown",
         taxon = scrubbed_species_binomial,
         genus = scrubbed_genus,
         family = scrubbed_family,
         ID = 1:n(),
         trait = rmbl_name,
         value = rmbl_scale)%>%
  select(ID,site,taxon,genus,family,trait,value) -> bien_traits

#rm(btraits,atraits)

bien_traits %>%
  mutate(value = log10(value)) -> bien_traits

field_traits %>%
  mutate(value = log10(value)) -> field_traits



#comparison

source("r_functions/compare_methods_small.R")#gutted version of a larger function for this particular case

local_traits <-
compare_methods(tidy_traits = field_traits,
                community = community,
                distribution_type = "normal")

global_traits <-
compare_methods(tidy_traits = bien_traits,
                community = community,
                distribution_type = "normal")

summary_combined <- 
  rbind(
    data.frame(trait_source = "global",global_traits),
    data.frame(trait_source = "local",local_traits)
    )

#get true values
atraits %>% 
group_by(site,trait) %>%
summarise(true_mean=mean(value),
          true_variance=var(value),
          true_skewness = skewness(value),
          true_kurtosis = e1071::kurtosis(value)) -> true_moments

#Next, append true moments to output data for convenience

summary_combined <- merge(x = summary_combined,
                          y = true_moments,
                          by = c("site","trait"))

saveRDS(object = summary_combined,file = "output_data/global_vs_local_summary.RDS")


##########################################################
library(ape)

phylo_vector <- list.files("C:/Users/Brian Maitner/Desktop/current_projects/global_plant_phylo/phylomaker_trees_randomly_resolved/",full.names = T)
phylo_vector <- phylo_vector[sample(x = 1:length(phylo_vector),size = 100,replace = F)]

traits <- bien_traits
community




#input: phylogenies, traits, species list
    #toggle to use all traits vs sample

#output: traits

get_traits_from_phylos <- function(phylo_vector,
                                   traits,
                                   species_list,
                                   subsample_traits = FALSE){
  
  
  
  
  for(i in 1:length(phylo_vector)){
    
    tree_i <- read.tree(phylo_vector[i])
    
    community$present <-  gsub(pattern = " ",replacement = "_",x = community$Accepted_species) %in% tree_i$tip.label
    
    
    
    
    
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}



