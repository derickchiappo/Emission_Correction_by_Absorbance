#' @title: Lombardo
#' 
#' 
#' @description Using the method described by Lombardo et al.,2005 (https://doi.org/10.1016/j.ab.2004.11.002),
#' it predicts the peak of the spectrum
#' 
#' 
#' @param data dataframe with emission/absorbance data and corresponding wavelengths
#' @param emission_values 
#' @param bins wavelengths
#' @param peak wavelength at which maximum emission occurs
#' @param wave1 wavelength 1 to input into the formula
#' @param wave1 wavelength 2 to input into the formula
#' @param observed emission values with compound of interest alone
#' @param method vector with two choices: Custom or Best. Custom uses two wavelengths specificed in wave 1 and wave 2. 
#' Best determines the best combination of wave 1 and wave 2 given a fixed value(either wave 1 or wave 2) 
#' and the other wavelengths. 
#' @fixed vector where if method is custom determines the fixed wavelength
#' 
#' @example 
#' data("Emission_data")
#' 
#' Lombardo_Pred(test_emission,bins = "Wavelength","AMCA_Hemin",observed = "AMCA",
#' peak = 465,wave1 = 420,method = "Best",fixed = "wave1")   
#' 
#' Lombardo_Pred(test_emission,bins = "Wavelength","AMCA_Hemin",observed = "AMCA",
#' peak = 465,wave2 = 475,method = "Best",fixed = "wave2")   
#' 
#' 
#' @export
Lombardo_Pred <- function(data,bins,emission_values,peak = NA,wave1 = NA, wave2 = NA,observed = NA,
                          method = c("Custom","Best"),fixed = c("wave1","wave2")) { 
  
            if(method == "Custom" & is.na(wave1) | is.na(wave2))
  
            options(error = NULL)
            
            combos <- data.frame()        
      
            best_combo <- data.frame()
            
            exp_value <- data[which(data[,bins] == peak),observed] #true value

            if (method == "Custom") { 
      
              predicted <- 2*data[which(data[,bins] == peak),emission_values] +
                (data[which(data[,bins] == wave1),emission_values] - data[which(data[,bins] == wave2),emission_values]) ##emission_values are those to be predicted
            
              out <- cbind(predicted,exp_value)
              
              colnames(out) <- c("Predicted","Observed")
                  
            } else if(method == "Best") { 
              
                for (i in 1:nrow(data)) { 
                
                  combos[i,"fixed_lambda"] <- get(fixed)
                
                  combos[i,"ideal_lambda"] <- data[i,bins] }
            
                for (j in 1:nrow(combos)) { 
                  
                   combos[j,"Predicted"] <- 2*(data[which(data[,bins] == peak),emission_values]) - 
                    
                         (
                       (data[which(data[,bins] == get(fixed)),emission_values] - 
                      data[which(data[,bins] == combos[j,"ideal_lambda"]),emission_values] )
                   ) 
                  
                    combos[j,"Accuracy"] <-  ( abs ( (combos[j,"Predicted"] - data[which(data[,bins] == peak),observed]) ) / 
                                                data[which(data[,bins] == peak),observed] ) * 100
                    
                    best <- combos[which(combos[,"Accuracy"] == min(abs(combos[,"Accuracy"]))),]
                    
                    out <- merge(best,exp_value)
                    
                    colnames(out)[length(out)] <- "Observed"
                } 
            
            }
            
            return(out)
}
















