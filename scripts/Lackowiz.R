
#global solution where there is differentiating between primary and secondary inner filter effects
lack <- function(Abs_data,Em_data,Absorbance_col = NA,Emission_col = NA,Emission_wavelength = NA,
                 Excitation_Wavelength = NA,Spectral = FALSE) { 
        
        options(error = NULL)
        
        if(is.na(Emission_wavelength) | is.na(Excitation_Wavelength) )  {  
                stop("Emission/Excitation Wavelength is Empty") }
        
        
        if (Spectral == TRUE) { 
          
          if(nrow(Abs_data) != nrow(Em_data)) { 
            
            Common_wavelength_data <- Abs_data[Abs_data[["Wavelength"]] %in% Em_data[["Wavelength"]],]
            
            Common_wavelengths <- Common_wavelength_data[["Wavelength"]]
            
            Correction_Factor <- (Common_wavelength_data[,Absorbance_col])/2 
            
            Corrected_emission <- Em_data[which(Em_data[["Wavelength"]] %in% Common_wavelengths),Emission_col]*10^(Correction_Factor) 
            
            out <- data.frame(Wavelength = Em_data[which(Em_data[["Wavelength"]] %in% Common_wavelengths),"Wavelength"],
                                             observed = Em_data[which(Em_data[["Wavelength"]] %in% Common_wavelengths),Emission_col])
            
            out$predicted <- unlist(Corrected_emission)
            
            } else {
                
                Correction_Factor <- (Abs_data[,Absorbance_col])/2 
                
                Corrected_emission <- Em_data[,Emission_col]*10^(Correction_Factor)
                
                out <- data.frame(Wavelength = Em_data["Wavelength"],
                                  observed = Em_data[Emission_col]) 
                
                out$predicted <- unlist(Corrected_emission)
                                
                
                } 
        
        } else { 
                
                Emission_Abs <- Abs_data[[which(Abs_data$Wavelength == Emission_wavelength),Absorbance_col]]
                
                Excitation_Abs <- Abs_data[[which(Abs_data$Wavelength == Excitation_Wavelength),Absorbance_col]]
                
                Correction_Factor <- (Emission_Abs + Excitation_Abs)/2
                
                Corrected_emission <- Em_data[,Emission_col]*10^(Correction_Factor) 
                
                out <- data.frame(Wavelength = Em_data["Wavelength"],
                                  observed = Em_data[Emission_col])
                
                out$predicted <- unlist(Corrected_emission)
                                  
                
                }
        
        return(out)
        
        }






