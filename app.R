# Load packages ----
library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)

# Source helpers ----
source("scripts/Lackowiz.R")

source("scripts/Oulette.R")

source("scripts/Lombardo.R")

#ggplot standard outputs

plot_settings <- theme(legend.text = element_text(size = 12),
                       
                       legend.title = element_text(size = 14),
                       
                       axis.title = element_text(size = 14),
                       
                       axis.line = element_line(color = "black"),
                       
                       panel.background = element_rect(fill = 'lightgray'))

# User interface ----
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Inner filter correction methods</a>'), id="nav",
                 windowTitle = "Inner filter correction methods",
             
             
  tabPanel("About",
           h1("About"),
           h4("With this app, you can correct fluorescence emission data using the emission data itself or the accompanying absorbance data. 
              The use of fluorescence assays in biomedical research is becoming more common. 
              However, the use of fluorescence reagents introduces a novel source of data artifacts.These artifacts include inner filter effects."),
           
           br(),
           br(),
           
           h4("Inner Filter effects are a well-established phenomenon that describes how inherent components of a fluorescent system can attenuate fluorescence signals [1]. 
              Specifically, the presence of IFEs in a system disrupts the linear relationship between fluorophore concentration and fluorescence emission [2]. 
              Therefore, an IFE can distort the actual amount of fluorescence in a solution.IFEs are divided into two types based on whether the attenuation of fluorescent signal is due to absorption of the exciting light or reabsorption of the fluorophore's emitted light [1, 2] (Figure 1). 
              The former and latter are known as primary inner and secondary inner filter effect, respectively [1]. "),
           
            imageOutput("image_1"),
            
            br(),
            br(),
            
            h4("Since inner filter effecst are well-established phenomenom in fluorescence, a number of methods to correct for these effects has been developed. 
               These methods are brooadly based on the the respective absorbance data of the sample or other spectrophotometric parameters."),
            
            h4("This app has two tabs with methods where fluorescence data is corrected for inner fitler effects based on absorbance and the emission data itself.
               The methods inlcuded in this app are explained by their authors, Lackowicz[3] and Oulette et al.[4]. Please see their publications for the respective formulas.
               The second tab uses the method developed by Lombardo et. al. [5] where only the emission data is used to correct for inner filter effects."),
           
            h4(strong("Refences")),
            
            h4("[1] 	Wolfbeis OS, Leiner M. Mapping of the total fluorescence of human blood serum as a new method for its characterization. Anal Chim Acta 1985; 167: 203–215."),
            h4("[2] 	Kumar Panigrahi S, Kumar Mishra A. Inner filter effect in fluorescence spectroscopy: As a problem and as a solution. J Photochem Photobiol C Photochem Rev 2019; 41: 100318."),
            h4("[3] 	Lakowicz JR (ed). Principles of Fluorescence Spectroscopy. Boston, MA: Springer US. Epub ahead of print 2006. DOI: 10.1007/978-0-387-46312-4."),
            h4("[4]   Ouellette M, Masse F, Lefebvre-Demers M, et al. Insights into gold nanoparticles as a mucoadhesive system. Sci Rep 2018; 8: 14357."),
            h4("[5]  Lombardo ME, Araujo LS, Ciccarelli AB, et al. A spectrophotometric method for estimating hemin in biological systems. Analytical Biochemistry 2005; 341: 199–203."),
),
  
  ##Panel to view the loaded datasets 
  
  tabPanel("Datasets",
           sidebarPanel(
             
           fileInput("dataset",NULL,accept = ".xlsx"),
           selectInput("sheet_names",label = "Sheet Names",choices = ' '),
           actionButton("load_data",label = "Load dataset")
  ),
  
  mainPanel(dataTableOutput("datatable")),
  
  ),
           
  
  tabPanel("Absorbance-based Methods",
      sidebarPanel(
        
        
        fileInput("dataset_1",NULL,accept = ".xlsx"),
        selectInput("emission_sheet",label = "Emission Data",choices = ' '),
        selectInput("absorbance_sheet",label = "Absorbance Data",choices = ' '),
        actionButton("load_data_1",label = "Load dataset"),
        

        selectInput(
          "em_var",
          label = "Emission data variable to show",
          choices = ' '),
        
        br(),
        br(),
        
        selectInput("CorrectionMethod","Spectral Transformation method",
                    choices = c("None","Lackowiz","Oulette"),
                    selected = "None"),
        
         conditionalPanel(condition = "input.CorrectionMethod == 'Lackowiz'", 
                          
                          radioButtons("Spectral",label = "Arguement for Lackowicz",
                                       c("Yes" = TRUE,
                                         "No" = FALSE)),
                          
         ),
                          
        conditionalPanel(condition = "input.Spectral == 'FALSE' & input.CorrectionMethod == 'Lackowiz' | input.CorrectionMethod == 'Oulette' ", 
                         
                         selectInput("Ex_lambda",label = "Excitation Wavelength",
                                     choices = NULL),
                         
                         selectInput("Em_lambda",label = "Emission Wavelength",
                                     choices =  NULL),
                         
                         selectInput("Abs_Col","Column for correction",
                                     choices = ''),
        ),
                          
      
         
         conditionalPanel(condition = "input.CorrectionMethod == 'Oulette'", 
                          selectInput("Effect_type",
                                      label = "Type of Inner filter effect",
                                      choices = c("Primary","Secondary","Global"),
                                      selected = "Primary"),
                        
      ),
      
      ),
        
      mainPanel(plotOutput("emission_plot"),
                plotOutput("absorbance_plot"),
                tableOutput("table1_2")),
      
      position = "left",
      fluid = TRUE),
  
  tabPanel("Emission-based Methods",
           sidebarPanel(
             
             fileInput("dataset_2",NULL,accept = ".xlsx"),
             selectInput("sheet_names_2",label = "Sheet Names",choices = ' '),
             actionButton("load_data_2",label = "Load dataset"),
      
             
             selectInput("var3",label = "Observed Data",
                         choices = ''),
            
             
             selectInput("Peak", label = "Peak wavelength of the fluorophore",
                         choices = NULL),
             
             br(),
             br(),
             
             selectInput("CorrectionMethod1","Spectral Transformation method",
                         choices = c("None","Lombardo"),
                         selected = "None"),     
             
             conditionalPanel(condition = "input.CorrectionMethod1 != 'None'", 
                              
                              selectInput(
                                "var2",label = "Data to be  corrected",
                                choices = '',  # Update so it does it by column names
                              ),
                              
                              selectInput("w1",label = "Wavelength 1",
                                          choices = NULL),
                              
                              selectInput("w2",label = "Wavelength 2",
                                          choices = NULL),
             
                             radioButtons("Method",label = "Arguements for method choice",
                                          c("Custom" = "Custom",
                                            "Best" = "Best")),
                            
                            conditionalPanel(condition = "input.Method == 'Best' ",
                                             
                                             selectInput("wave",label = "Wavelength",
                                                         choices = c("wave1","wave2"),selected = "wave1")),
                                             
                                             
                             
                            ),
             
           ),
             
             mainPanel(plotOutput("Bar_plot"),
                       tableOutput("Prediction_table")),
             
             position = "left",
             fluid = TRUE),

              ),

)
  
# Server logic
server <- function(input, output, session) {
  
  
  ##Observe and datatable event generation for datasets panel 
  observe ({ 
    req(input$dataset)
    
    File <- input$dataset
    
    if (is.null(file)) {
      
      return("No file loaded")
      
    } else { 
      
      worksheets <- unique(readxl::excel_sheets(path = File$datapath))
      
      updateSelectInput(session,"sheet_names", choices = worksheets)
      
      }
    })

  observeEvent(input$load_data,{ 
    
    req(input$dataset)
    
    File <- input$dataset
    
   data_out <- readxl::read_xlsx(File$datapath,sheet = as.character(input$sheet_names))
   
   output$datatable <- renderDataTable({ data_out })

    })
  
  output$image_1 <- renderImage({ 
    list(src = "Inner_filter_effects.png",
    width = "50%",height = 400)
  },deleteFile = F)
  
  
  ##server arguments for absorbance-based methods panel 
  observe ({ 
    req(input$dataset_1)
    
    File <- input$dataset_1
    
    if (is.null(file)) {
      
      return("No file loaded")
      
    } else { 
      
      worksheets <- unique(readxl::excel_sheets(path = File$datapath))
      
      updateSelectInput(session,"emission_sheet", choices = worksheets)
      
      updateSelectInput(session,"absorbance_sheet",choices = worksheets)
      
    }
  })
  

  observeEvent(input$load_data_1,{ 
    
    req(input$dataset_1)
    
    File <- input$dataset_1
    
    em_out <- readxl::read_xlsx(File$datapath,sheet = input$emission_sheet)
    
    abs_out <- readxl::read_xlsx(File$datapath,sheet = input$absorbance_sheet)
    
    row_check <- nrow(em_out) == nrow(abs_out)
    
    if(isFALSE(row_check) & input$CorrectionMethod == "Lackowiz") { 
      
      em_out <- em_out[which(em_out[[1]] %in% abs_out[[1]]),]
      
      abs_out <- abs_out[which(em_out[[1]] %in% abs_out[[1]]),]
      
      
    }
    
    updateSelectInput(session,"em_var", choices = c(colnames(em_out)),
                      selected = colnames(em_out)[2])
    
    updateSelectInput(session,"Ex_lambda", choices = c(abs_out[1]))
    
    updateSelectInput(session,"Em_lambda", choices = c(abs_out[1]))
    
    updateSelectInput(session,"Abs_Col", choices = c(colnames(abs_out)),
                      selected = colnames(abs_out)[2])
  
    
    final_data <- reactive({ 
    
     if(input$CorrectionMethod == "None") {
      
        df <- em_out
      
      } else if(input$CorrectionMethod == "Lackowiz") { 
      
        df <- lack(abs_out,em_out,Absorbance_col = as.character(input$Abs_Col),
                               
                                                      Emission_col = as.character(input$em_var),
                               
                                                      Emission_wavelength = input$Em_lambda,
                               
                    Excitation_Wavelength = input$Ex_lambda,Spectral = input$Spectral)
        
      
    } else if(input$CorrectionMethod == "Oulette") {
      
       df <- nanop(abs_out,em_out,Emission_col = as.character(input$em_var),
                                
                         flourophosore_Absorbance = as.character(input$em_var), 
                         
                         total_absorbance = as.character(input$Abs_Col), 
                         
                         Emission_wavelength = input$Em_lambda,Excitation_Wavelength = input$Ex_lambda,Inner_filter = input$Effect_type )
    }
    
   })
  
  output$emission_plot <- renderPlot({ 
    
    if(input$CorrectionMethod != "None") { 
      
   ggplot(final_data(),aes(Wavelength)) + geom_point(aes(y = get(input$em_var),color = "Observed"),show.legend = TRUE,size = 3) + 
        geom_point(aes(y = predicted,color = "Predicted"),show.legend = TRUE,size = 3) +
        
        labs(x = "Wavelength (nm)",y = "Fluorescence Emission (R.F.U.)",color = "Legend") +
        
        plot_settings 
    } 
    
    else {
    
    ggplot(final_data(),aes(Wavelength,get(input$em_var))) + geom_point(color = "red",size = 3) +
        
        labs(x = "Wavelength (nm)",y = "Fluorescence Emission (R.F.U.)",color = "Legend") +
        
        plot_settings 
    }
    
    })
  
  output$absorbance_plot <- renderPlot({ 
    ggplot(abs_out,aes(Wavelength,get(input$em_var))) + geom_point(color = "blue",size = 3) +
      
      ylab("Absorbance (O.D.)") + plot_settings })
  
  output$table1_2 <- renderTable({  final_data()  })
  
  
  })
  
  ##arguments for emission-based methods tabs 
  observe ({ 
    req(input$dataset_2)
    
    if (is.null(file)) {
      
      return("No file loaded")
      
    } else { 
      
      worksheets <- unique(readxl::excel_sheets(path = input$dataset_2$datapath))
      
      updateSelectInput(session,"sheet_names_2", choices = worksheets)
    
    }
  })
  
  observeEvent(input$load_data_2,{ 
    
    req(input$dataset_2)
    
    File <- input$dataset_2
    
    data_out <- readxl::read_xlsx(File$datapath,sheet = input$sheet_names_2)
    
    updateSelectInput(session,"bins_2", choices = c(colnames(data_out)),
                      selected = "Wavelength")
    
    updateSelectInput(session,"var2", choices = c(colnames(data_out)),
                      selected = colnames(data_out)[2])
    
    updateSelectInput(session,"var3", choices = c(colnames(data_out)),
                      selected = colnames(data_out)[2])
    
    updateSelectInput(session,"w1", choices = c(data_out[1]))
    
    updateSelectInput(session,"w2", choices = c(data_out[1]))
    
    updateSelectInput(session,"Peak", choices = c(data_out[1]))
    
    Results <- reactive({
      
      if(input$CorrectionMethod1 == "None") { 
        
        data_out[which(data_out[,"Wavelength"] == input$Peak),input$var3]
        
      } else if(input$CorrectionMethod1 == "Lombardo" & input$Method == "Custom") { 
        
         Lombardo_Pred(data_out,bins = "Wavelength",emission_values = as.character(input$var2),observed = as.character(input$var3),
                             
                             peak = input$Peak,wave1 = input$w1,wave2 = input$w2,method = input$Method)
        
      } else if(input$CorrectionMethod1 == "Lombardo" & input$Method == "Best") { 
        
         Lombardo_Pred(data_out,bins = "Wavelength",emission_values = as.character(input$var2),observed = as.character(input$var3),
                             
                             peak = input$Peak,wave1 = input$w1,wave2 = input$w2,method = input$Method,fixed = input$wave)
        
      }
      
    })
    
    
    observe ({ 
      if(input$Method == "Best" & input$wave == "wave1")  { 
        
        updateSelectInput(session, "w2",
                          label = "Wavelength 2",
                          choices = c(data_out[,1]),
                          selected = Results()[,"ideal_lambda"]) 
        
      } else if (input$Method == "Best" & input$wave == "wave2") { 
        
        updateSelectInput(session, "w1",
                          label = "Wavelength 1",
                          choices = c(data_out[,1]),
                          selected = Results()[,"ideal_lambda"]) 
      }
    })
    
    
    output$Bar_plot <- renderPlot({ 
      
      if(input$CorrectionMethod1 == "None") {
        
        ggplot(Results()) + geom_col(aes(x = "Observed", y = get(input$var3),fill = "Observed")) + 
          labs(y = paste0("Raw fluorescence Emission value at ",as.character(input$Peak), " nm"),x = " ",fill = "Legend")  +
          plot_settings 
        
      } else if(input$CorrectionMethod1 == "Lombardo") {
        
        ggplot(Results()) + geom_col(aes(x = "Observed",y = Observed,fill = "Observed")) + 
          
          geom_col(aes(x = "Predicted",y = Predicted,fill = "Predicted")) +
          
          labs(x = "",y = paste0("Raw fluorescence Emission value at ",as.character(input$Peak), " nm"), fill = "Legend") +
          
          plot_settings 
        
      }
      
    })
    
    output$Prediction_table <- renderTable({ Results() })
    
  })

}
    
# Run the app
shinyApp(ui,server)
