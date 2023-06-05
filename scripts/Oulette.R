#' ABCF: Oulette
#'
#'Corrects raw emission values by the method proposed by Ouellette et al.(2018) DOI:10.1038/s41598-018-32699-2
#'
#' @param Abs_data a data table with raw absorbance values
#' @param Em_data a data table with raw emission value
#' @param Emission_col the column in Em_data that contains the raw emission values to be corrected
#' @param total_absorbance the column in Abs_data that contains the raw absorbance values used to generate the correction factor
#' @param Emission_wavelength wavelength at which maximal emission of the fluorophore occurs
#' @param Excitation_wavelength wavelength at which maximal emission of flourophore occurs
#' @param Inner_filter Vector of different inner filter effects to correct for
#' @details Inner_filter: A vector that determines if the raw emission values are corrected for primary, secondary, or both inner filter effects.
#'If not specificed, this function defaults to "primary".
#'
#' @return returns a data frame with the corrected raw emission values
#' @export
nanop <-
  function(Abs_data,Em_data,flourophosore_Absorbance,Emission_col,total_absorbance,
           Emission_wavelength = NA,Excitation_Wavelength = NA,Inner_filter = c("Primary","Secondary","Global")) {
              options(error = NULL)

              if(is.na(Emission_wavelength) | is.na(Excitation_Wavelength) )  {
               message("Emission/Excitation Wavelength is Empty") }


               Ratio_1 <-
                    (Abs_data[which(Abs_data$Wavelength == Excitation_Wavelength),total_absorbance]/
                        Abs_data[which(Abs_data$Wavelength == Excitation_Wavelength),flourophosore_Absorbance])

               Ratio_2  <-

                  (   (1 - (10^(-Abs_data[which(Abs_data$Wavelength == Excitation_Wavelength),flourophosore_Absorbance])))/
                  (1 - (10^(-Abs_data[which(Abs_data$Wavelength == Excitation_Wavelength),total_absorbance])))  )


                  Secondary <-
                  ( (2.3*Abs_data[which(Abs_data$Wavelength == Emission_wavelength),total_absorbance])/
                  (1 - (10^(-Abs_data[which(Abs_data$Wavelength == Emission_wavelength),total_absorbance]))) )

                  Primary <- Ratio_1*Ratio_2

                  Global <- Primary*Secondary

                  Corrected_emission <- Em_data[,Emission_col]*as.numeric(Primary)
                  
            if (Inner_filter == "Primary") {

                Corrected_emission <- Em_data[,Emission_col]*unlist(Primary)
                
                out <- data.frame(Em_data[,"Wavelength"],
                                  Em_data[,Emission_col])
                
                out$predicted <- unlist(Corrected_emission )

            } else if (Inner_filter == "Secondary") {

                Corrected_emission <- Em_data[,Emission_col]*unlist(Secondary)
                
                out <- data.frame(Em_data[,"Wavelength"],
                                  Em_data[,Emission_col])
                
                out$predicted <- unlist(Corrected_emission )


            } else if (Inner_filter == "Global"){

               Corrected_emission <- Em_data[,Emission_col]*unlist(Primary*Secondary)
               
               out <- data.frame(Em_data[,"Wavelength"],
                                 Em_data[,Emission_col])
               
               out$predicted <- unlist(Corrected_emission )

           }


          return(out)

  }









