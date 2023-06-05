# Emission_Correction_by_Absorbance
The use of fluorescence-based assays in the biomedical field is becoming more common but introduces novel data artificats that biologists may not be familiar with. These can include inner filter effects. Therefore, this RShiny presents code to correct fluorescence emission data, which may be affected by inner filter effects, by the most widely cited solutions using absorbance data from the sampe samples. 


To use the app, the data must be formmated as a table where the first column is composed of the wavelengths at which the readings were taken. 

Two sets of example data have been provided in this repository. The first dataset represents a matched set of absorbance emission data where wavelengths corresponds to each other in the dataset. Therefore, the absorbance and emission readings were taken at the same wavelengths. The second dataset represents a dataset where the wavelengths in the absorbance and emission tables differ. 
 
