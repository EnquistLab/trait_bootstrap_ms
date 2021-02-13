#return distribution
get_distributions <- function (imputed_traits, nrep = 3, sample_size = 200) 
{
  attrib <- attr(imputed_traits, "attrib")
  value_col <- attrib$value_col
  bootstrapMoments <- map_df(1:nrep, ~{
    slice_sample(imputed_traits, n = sample_size, replace = TRUE, 
                 weight_by = weight) }, .id = "n")
    
  bootstrapMoments <- bootstrapMoments[c(as.character(attrib$scale_hierarchy),attrib$trait_col,"n","value")]
  
    #%>% summarise(mean = mean(.data[[value_col]]), 
    #                                               variance = var(.data[[value_col]]), skewness = skewness(.data[[value_col]]), 
    #                                               kurtosis = kurtosis(.data[[value_col]]))
  
  attr(bootstrapMoments, "attrib") <- attrib
  class(bootstrapMoments) <- class(bootstrapMoments)[!class(bootstrapMoments) == 
                                                       "imputed_trait"]
  return(bootstrapMoments)
}


##############################################################################################################

#parametric version

get_distributions_parametric <- function( imputed_traits, 
          distribution_type, 
          nrep = 3, 
          samples_per_abundance = 2,
          sample_size=200){
  
  
  value_col <- attributes(imputed_traits)$attrib$value_col
  trait_col <- attributes(imputed_traits)$attrib$trait_col
  taxon_col <- attributes(imputed_traits)$attrib$taxon_col
  scale_hierarchy <- attributes(imputed_traits)$attrib$scale_hierarchy
  
  
  #If only a single type of distribution was supplied, apply it to all traits
  if(length(distribution_type)==1){
    
    #distributions <- unique(imputed_traits$Trait)
    distributions <- unique(as.data.frame(imputed_traits)[,trait_col])
    
    names(distributions)[1:length(distributions)] <- distribution_type
    distribution_type<-distributions
    rm(distributions)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  #First, we need to fit distributions
  #For each trait x species x level of the hierarchy (note this only works if hierarchy is only geographic)
  
  distributions_to_fit <- unique(imputed_traits[c(as.character(scale_hierarchy),taxon_col,trait_col)])
  
  distribution_parameters <- as.data.frame(distributions_to_fit)
  
  
  #Add columns for different distribution parameters as needed
  
  #Normal
  if("normal" %in% names(distribution_type)){
    
    distribution_parameters$norm_mean<-NA
    distribution_parameters$norm_sd<-NA
    
  }
  
  #lognormal
  
  if("lognormal" %in% names(distribution_type) | "log-normal" %in% names(distribution_type)){
    
    distribution_parameters$lnorm_meanlog <- NA
    distribution_parameters$lnorm_sdlog <- NA
    
  }
  
  #beta
  
  if("beta" %in% names(distribution_type)){
    
    distribution_parameters$beta_shape1 <- NA
    distribution_parameters$beta_shape2 <- NA
    
  }
  
  
  
  
  for(i in 1:nrow(distributions_to_fit)){
    
    
    #pull all data matching the criteria  
    data_i <- imputed_traits[eval(parse(text = paste("which(",paste(paste("imputed_traits$",names(distributions_to_fit)," == '",distributions_to_fit[i,],"'",sep = ""  ),collapse = " & " ),")"))),value_col]
    data_i <- na.omit(data_i)  
    
    if(nrow(data_i)==0){next}
    
    #fit the specified distribution
    
    #Figure out what type of distribution to fit
    dist_type_i <- names(distribution_type)[which(distribution_type==as.character(distributions_to_fit[i,trait_col]))]
    
    #Normal
    if(dist_type_i=="normal"){
      
      if(nrow(data_i)==1){
        
        distribution_parameters$norm_mean[i] <- data_i
        distribution_parameters$norm_sd[i] <- 0  
        
      }else{ 
        
        fit_i<-NULL
        
        try(fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "norm",method = "mle"),silent = T)
        
        if(is.null(fit_i)){fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "norm",method = "mme")}
        
        
        distribution_parameters$norm_mean[i] <- fit_i$estimate[1]
        distribution_parameters$norm_sd[i] <- fit_i$estimate[2]
      }
    }#normal fit
    
    #log-normal
    
    if(dist_type_i %in% c("lognormal","log-normal")){
      
      if(nrow(data_i)==1){
        
        distribution_parameters$lnorm_meanlog[i] <- log(data_i)
        distribution_parameters$lnorm_sdlog[i] <- 0  
        
      }else{ 
        
        
        fit_i<-NULL
        
        try(fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "lnorm",method = "mle"),silent = T)
        
        if(is.null(fit_i)){fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "lnorm",method = "mme")}
        
        
        distribution_parameters$lnorm_meanlog[i] <- fit_i$estimate[1]
        distribution_parameters$lnorm_sdlog[i] <- fit_i$estimate[2]
      }
      
    }#lognormal
    
    #beta
    
    if(dist_type_i %in% c("beta") ){
      
      if(nrow(data_i)==1){
        
        if(!"unif_min" %in% colnames(distribution_parameters)){
          distribution_parameters$unif_min <- NA
          distribution_parameters$unif_max <- NA
          
        }
        
        
        distribution_parameters$unif_min[i] <- data_i
        distribution_parameters$unif_max[i] <- data_i
        
        
      }else{ 
        
        
        fit_i <- NULL
        
        try(fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "beta",method = "mle"),silent = T)
        
        if(is.null(fit_i)){fit_i <- fitdistrplus::fitdist(data = unlist(data_i),distr = "beta",method = "mme")}
        
        
        distribution_parameters$beta_shape1[i] <- fit_i$estimate[1]
        distribution_parameters$beta_shape2[i] <- fit_i$estimate[2]
      }
      
    }#lognormal
    
    
    
  }#i loop
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
  #Next, we need to use the fitted distributions to estimate moments
  
  distribution_moments <- as.data.frame(unique(imputed_traits[c(as.character(scale_hierarchy),trait_col)]))
  
  
  
  #Duplicate output to hold all replicates
  distribution_moments <- distribution_moments[rep(row.names(distribution_moments),nrep),]
  distribution_moments$n <- NA
  
  #Now, iterate through each scale hierarchy x trait combo and sample nreps
  
  out_distributions <- as.data.frame(matrix(nrow = nrow(distribution_moments)*sample_size,ncol = ncol(distribution_moments)+1))
  
  for( i in 1:nrow(unique(distribution_moments))){
    #print(i)
    distr_i <- unique(distribution_moments)[i,]  
    
    #abundances_i <- unique(imputed_traits[eval(parse(text = paste("which(",paste(paste("imputed_traits$",scale_hierarchy," == '",distr_i[scale_hierarchy],"'",sep = ""  ),collapse = " & " ),")"))),c(taxon_col,"abundance")])
    
    abundances_i <- unique(imputed_traits[eval(parse(text = paste("which(",paste(paste("imputed_traits$",as.character(scale_hierarchy)," == '",distr_i[as.character(scale_hierarchy)],"'",sep = ""  ),collapse = " & " ),")"))),c(taxon_col,"abundance")])
    
    out_i <- matrix(nrow = nrep, ncol=sum(floor(abundances_i$abundance*samples_per_abundance)))
    
    
    #pull all data matching the criteria  
    
    for(t in 1:nrow(abundances_i)){
      
      
      
      
      #distr_t  <- distribution_parameters[eval(parse(text = paste("which( ",paste(paste("distribution_parameters$",scale_hierarchy," == '",distr_i[scale_hierarchy],"'",
      #                                                                        sep = ""  ),collapse = " & " ),"& distribution_parameters$",taxon_col," == '",
      #                                                  abundances_i[t,taxon_col],"' & distribution_parameters$",trait_col, "== '",distr_i[trait_col],"' )",sep = ""))),]  
      
      distr_t  <- distribution_parameters[eval(parse(text = paste("which( ",paste(paste("distribution_parameters$",as.character(scale_hierarchy)," == '",distr_i[as.character(scale_hierarchy)],"'",
                                                                                        sep = ""  ),collapse = " & " ),"& distribution_parameters$",taxon_col," == '",
                                                                  abundances_i[t,taxon_col],"' & distribution_parameters$",trait_col, "== '",distr_i[trait_col],"' )",sep = ""))),]  
      
      
      
      
      #If the specified trait was not fit (missing data), supply NAs
      if(nrow(distr_t)==0){
        
        
        draw_t <- rep(NA,(floor(abundances_i$abundance[t]*samples_per_abundance)*nrep))
        
        #Record output (if any)
        
        if(length(draw_t)>0){
          
          
          #This bit fills the first empty columns in the output file
          out_i[,which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]:(which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]+(floor(abundances_i$abundance[t]*samples_per_abundance)-1))] <- draw_t
          
        }#if there is output
        
        #break out of iteration t with next, otherwise errors occur with following if statements
        next 
      }
      
      
      #If the specified trait has a normal distribution, draw from that
      if("norm_mean" %in% colnames(distr_t)){
        if(!is.na(distr_t$norm_mean)){
          
          draw_t <- rnorm(n = (floor(abundances_i$abundance[t]*samples_per_abundance)*nrep),
                          mean = distr_t$norm_mean[[1]],
                          sd =  distr_t$norm_sd[[1]])
          
          
          #Record output (if any)
          
          if(length(draw_t)>0){
            
            
            #This bit fills the first empty columns in the output file
            out_i[,which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]:(which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]+(floor(abundances_i$abundance[t]*samples_per_abundance)-1))] <- draw_t
            
          }#if there is output
          
          
        }}
      
      
      
      #Log normal fitting if needed
      
      if("lnorm_meanlog" %in% colnames(distr_t)){
        
        if(!is.na(distr_t$lnorm_meanlog)){
          
          draw_t <- rlnorm(n = (floor(abundances_i$abundance[t]*samples_per_abundance)*nrep),
                           meanlog = distr_t$lnorm_meanlog[[1]],
                           sdlog = distr_t$lnorm_sdlog[[1]])
          
          #Record output (if any)
          
          if(length(draw_t)>0){
            
            
            #This bit fills the first empty columns in the output file
            out_i[,which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]:(which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]+(floor(abundances_i$abundance[t]*samples_per_abundance)-1))] <- draw_t
            
          }#if there is output
          
          
          
        }  
        
        
      }
      
      #beta
      
      if("beta_shape1" %in% colnames(distr_t)){
        
        if(!is.na(distr_t$beta_shape1)){
          
          draw_t <- rbeta(n = (floor(abundances_i$abundance[t]*samples_per_abundance)*nrep),
                          shape1 = distr_t$beta_shape1[[1]],
                          shape2 = distr_t$beta_shape2[[1]]
          )
          
          #Record output (if any)
          
          if(length(draw_t)>0){
            
            
            #This bit fills the first empty columns in the output file
            out_i[,which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]:(which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]+(floor(abundances_i$abundance[t]*samples_per_abundance)-1))] <- draw_t
            
          }#if there is output
          
          
          
        }  
        
        
      }
      
      
      #unif
      
      if("unif_min" %in% colnames(distr_t)){
        
        if(!is.na(distr_t$unif_min)){
          
          draw_t <- runif(n = (floor(abundances_i$abundance[t]*samples_per_abundance)*nrep),
                          min = distr_t$unif_min[[1]],
                          max = distr_t$unif_max[[1]]
          )
          
          #Record output (if any)
          
          if(length(draw_t)>0){
            
            
            #This bit fills the first empty columns in the output file
            out_i[,which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]:(which(apply(X = out_i,MARGIN = 2,FUN = function(x){all(is.na(x))}))[1]+(floor(abundances_i$abundance[t]*samples_per_abundance)-1))] <- draw_t
            
          }#if there is output
          
          
          
        }  
        
        
      }
      
      
    }#t loop, drawing traits for each species
    
    
    #Calculate the moments of each replicate and populate the distribution_moments object accordingly
    
    out_i <- apply(X = out_i,MARGIN = 1,FUN = function(x){cbind(unique(distribution_moments)[i,],sample(x = x,size = sample_size,replace = F))})
    out_i <- do.call(rbind,out_i)
    
    #Assign rep number
    out_i$n <-sort(rep(1:nrep,sample_size)) 
    
    #add to output
    out_distributions[which.min(!is.na(out_distributions$V1)):(which.min(!is.na(out_distributions$V1))+nrow(out_i)-1),] <- out_i
    
    #cleanup
    rm(out_i)
    
  }#i loop sampling para distr.
  
  
  #Make output comparable to nonparametric output
  #global,site,trait,n,value
  
  
  colnames(out_distributions)<-c(colnames(distribution_moments),"value")
  
  
  
  
  return(out_distributions)
  
  
}
