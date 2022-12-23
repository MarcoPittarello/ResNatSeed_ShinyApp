###### librerie #####
library(readxl)
library(ggpubr)
library(grDevices)
library(mgcv)
library(glmmTMB)
library(visreg)
library(performance)
library(shiny)
library(rsconnect)
library(shinythemes)
library(vegan)
library(tidyverse)

# library(devtools)
# install_github("MarcoPittarello/ResNatSeed")

library(ResNatSeed)



# ^^^^^ UI - shiny app ------------------------------------------
ui <- fluidPage(
  titlePanel("ResNatSeed: an R package and Shiny web app to predict the REStoration potential of NATive SEEDs using topographic factors"),
  theme = shinytheme("cosmo"),
  navbarPage("ResNatSeed",
             
             # 1 - HOME PAGE----           
             tabPanel(icon("home"),
                      fluidRow(
                        column(2,
                               img(src = "seeds.jpg", height = 150, width = 150,style="display: block; margin-left: auto; margin-right: auto;")
                        ),#end column
                        column(8,
                               h1(strong("Welcome to ResNatSeed Web app",
                                         style="color:black;
                                         background-color:lightgreen;
                                         padding:10px;
                                         border-radius:4px"),
                                  align = "center")
                        ),
                        column(2,
                               img(src = "seeds.jpg", height = 150, width = 150,style="display: block; margin-left: auto; margin-right: auto;")
                        )#end column
                      ),#end fluidrow
                      fluidRow(
                        br(),
                        br(),
                        p("The United Nations Decade on Ecosystem Restoration is highlighting the importance of restoring ecosystems and trying to overcome the actual limitations to the process."),
                        p(strong("ResNatSeed"), "is an R package born to overcome the problem of subjectivity during the assessment of suitability between native seeds and a site that must be restored.
")),
                      fluidRow(
                        column(7,
                               p("The package", strong("allows to assess the restoration potential of native seeds in a restoration site using topographic factors only. "), "The input data are the composition of the seed mixture or the donor grassland and the ",strong("elevation, slope, and aspect")," of the restoration site. Such topographic factors are easily retrievable both in the field and through any Geographic Information System (GIS) software."),
                               p("The suitability of the species mixture for restoring a site is based on the modeling of a large database of vegetation surveys, characterized by botanical composition associated to the abovementioned topographic factors. Such a database can be either a default one, which is related to the Piedmont Region (NW Italy), or provided by the user to allow the usage of ResNatSeed for any geographical area."),
                               p(strong("The restoration potential is expressed with two synthetic indexes")," (named ",strong("Suitability Index and Reliability Index"),"), readily interpretable as well as they range between 0 (low suitability and realiability) and 1 (high suitability and realiability).")
                               ),
                        column(1,
                               img(src = "DeGrandis3.jpg", height = 300, width = 400)
                               ),#end column
                        ),#end fluid row
                      p("This ",strong("Shiny web app"),", is meant to provide an easy to use interface that could allow the use of ResNatSeed also to professionals and non-experts of the software R.
")
                      
             ),#end tabpanel
             
             # 2 - INSTRUCTIONS----           
             tabPanel("Instructions",
                      fluidRow(
                        tabsetPanel(
                          type="tabs",
                          ## glossary ----           
                          tabPanel("Glossary",
                                   h5(strong("restoration site:"),p("degraded site that needs to be restored through sowing")),
                                   h5(strong("donor grassland:"),p("grassland in which plant material is harvested (e.g. through brushing) to provide native seeds to restoration sites")),
                                   h5(strong("vegetation and topographical variables database:"),p("dataframe containing the composition of a set of vegetation surveys and their corresponding topographic variables (elevation, slope and aspect)")),
                                   h5(strong("training database:"),p("dataframe with the species eligible for the statistical modelling, selected on the basis of their frequency and abundance in the vegetation and topographical variables database. Each species is associated with the values of topographic variables (elevation, slope and aspect) and its abundance.")),
                                   h5(strong("species codes - CEP names:"),p("an eight-letter abbreviation of species names according to the Cornell Ecology Programs (CEP)")),
                                   h5(strong("seed mixture and donor grassland composition:"),p("The 'seed mixture' is the list of the species and their abundance in the seed batch. The 'donor grassland composition' is the list of the species and their abundance surveyed in a grassland where native seeds are harvested.")),
                                   h5(strong("topographical variables of the restoration site:"),p("values of the elevation, slope, and aspect of a degraded site that needs to be restored through sowing.")),
                                   h5(strong("Suitability Index (SI):"),p("suitability of a seed mixture or donor grassland to restore a site with specific topographic characteristics. It ranges between 0 and 1. When SI=0 the restoration site is totally beyond the optimal ecological ranges of all species of the seed mixture or donor grassland, which is therefore not appropriate for the site restoration. Conversely, when SI=1 the restoration site has the optimal ecological conditions for all species of the seed mixture or donor grassland, which is therefore perfectly appropriate for the site restoration.")),
                                   h5(strong("Reliability Index (RI):"),p("index of the reliability of the Suitability Index (SI). The RI ranges between 0 and 1. When RI is close to 0 it means that few to none species contribute to the computation of the SI, whereas when RI is close to 1 the SI is computed with most to all species. Therefore, the higher is the RI, the most reliable is the SI. Not all the species of the seed mixture and donor grassland composition may modeled as i) they can be missing from the training database or ii) the values of the topographic factors of the restoration site are beyond their ecological ranges (e.g. if the elevation of the restoration site is 250 m and a species as an elevation range bounded between 1000 and 3000 m, such a species cannot be modeled)"))
                                   ),
                          ## how to use  ----           
                          tabPanel("How to use ResNatSeed",
                                   p("ResNatSeed can operate in two different ways depending on the source of the training database (i.e. the default or customized one):"),
                                   p("The",strong("DEFAULT training database"),"is based on vegetation data collected in the Piedmont Region (North-western Italy). See the ",strong("TUTORIAL 1 - Default training database"),"for operational details."),
                                   p("The",strong("CUSTOMIZED training database"),"is based on vegetation data provided by the user. See the ",strong("TUTORIAL 2 - Customized training database"),"for operational details.")
                                   ),#end tabpanel
                          
                          ## Tutorial 1 ----           
                          tabPanel("Tutorial 1: DEFAULT training database",
                                   fluidRow(
                                     h3(strong("Step 1"),style="color:red"),
                                     column(5,
                                            img(src = "def_step1.PNG", height = 70, width = 350)
                                     ),
                                     column(5,
                                            p("Select the 'WebApp' panel",
                                              style="text-align:left;
                                            color:black;
                                            background-color:white;
                                            padding:15px;
                                            border-radius:10px")
                                     )#end column
                                     ),#end fluid row
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 2"),style="color:red"),
                                     column(5,
                                            img(src = "def_step2.PNG", height = 100, width = 350)
                                            ),
                                     column(5,
                                            p("Select 'Default (Piedmont, Italy)'"),
                                            p("When this setting is the default one, the training database refers to the vegetation surveys from the Piedmont Region - North-Western Italy, which includes 248 plant species eligible to modeling")
                                            )
                                     ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 3"),style="color:red"),
                                     column(5,
                                            img(src = "def_step3.PNG", height = 100, width = 350),
                                            tags$hr(),
                                            br(),
                                            img(src = "exampleDefCep.PNG", height = 233, width = 350)
                                     ),
                                     column(5,
                                            p("Click the Download button to download a 'csv' file with the 248 species eligible to modeling"),
                                            p("The database contains two colums: the species names following the Flora Alpina nomenclature (Aeschimann et al.,2004) and their abbrevation codes (CEP names) to use to create the seed mixture or donor grassland composition database")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 4"),style="color:red"),
                                     column(5,
                                            img(src = "exampleSeedMix.PNG", height = 210, width = 350)
                                     ),
                                     column(5,
                                            p("In a excel sheet create the mixture or donor grassland composition database, which is characterized by two columns:"),
                                            p(strong("First column:"),"species code abbreviated in CEP names format, retrievable from the downloaded csv file"),
                                            p(strong("Second column:"),"abundance of each species. Abundance must be a number bounded between 0 and 100, which can be either a species relative abundance or a species cover (sensu Pittarello et al., 2016; Verdinelli et al., 2022)"),
                                            p("Save the file in 'csv' format, semicolon separated"),
                                            p("The total abundance of the seed mixture or donor grassland composition should not necessarily amount to 100%.")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 5"),style="color:red"),
                                     column(5,
                                            img(src = "def_step5.PNG", height = 175, width = 350)
                                     ),
                                     column(5,
                                            p("Upload the seed mixture or donor grassland composition in a 'csv' format")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 6"),style="color:red"),
                                     column(5,
                                            img(src = "def_step6.PNG", height = 400, width = 350)
                                     ),
                                     column(5,
                                            p("Set the topographical values of the restoration site and then press the RUN button")
                                     )
                                   )
                                   ),#end tabpanel
                          
                          ## Tutorial 2 ----           
                          tabPanel("Tutorial 2: CUSTOMIZED training database",
                                   fluidRow(
                                     h3(strong("Step 1"),style="color:red"),
                                     column(5,
                                            img(src = "def_step1.PNG", height = 80, width = 350)
                                     ),
                                     column(5,
                                            p("Select the 'WebApp' panel",
                                              style="text-align:left;
                                            color:black;
                                            background-color:white;
                                            padding:15px;
                                            border-radius:10px")
                                     )#end column
                                   ),#end fluid row
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 2"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step2.PNG", height = 105, width = 350)
                                     ),
                                     column(5,
                                            p("Select 'Customized'"),
                                            p("When this setting is the customized one, the training database that will be created refers to the vegetation surveys provided by the user")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 3"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step3.PNG", height = 175, width = 350)
                                            ),
                                     column(5,
                                            p("Upload the database with vegetation and topographical variables structured as follow:"),
                                            p(strong("1. "),"Rows of the dataframe are the surveys"),
                                            p(strong("2. "),"The first column reports the survey codes"),
                                            p(strong("3. "),"The second, third and fourth columns report the topographical variables, respectively:"),
                                            p(strong("    3.1. "),"Elevation: expressed in meters above sea level (m a.s.l.)"),
                                            p(strong("    3.2. "),"Slope: expressed in degrees (°)"),
                                            p(strong("    3.3. "),"Aspect: expressed in degrees from North (°N)"),
                                            p(strong("4. "),"From the fifth column onwards, the plant species names. Plant species names do not necessarily have to be coded, they can be left in full. Afterwards, abbreviation codes will be automatically created in CEP names format: an abbreviation of species names according to the Cornell Ecology Programs (CEP), which uses eight-letter abbreviations for species. The CEP names code will be used to formulate the seed mixture or donor grassland composition.")
                                            )
                                   ),
                                   fluidRow(
                                     column(12,
                                            img(src = "ex_VegTopoVar.PNG", height = 500, width = 1100)
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 4"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step4.PNG", height = 175, width = 350)
                                     ),
                                     column(5,
                                            p("Set the following parameters to create the training database:"),
                                            p(strong("species frequency: "), "this argument allows to set a threshold of minimum frequency of each species in the surveys. It is advisable to set values greater than or equal to 30 to allow appropriate statistical modeling."),
                                            p(strong("minimum species abundance"),"this argument allows to set a threshold of minimum abundance (greater than) of each species in each survey.")
                                            )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 5"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step5.PNG", height = 105, width = 350)                                     ),
                                     column(5,
                                            p("Click the Generate button to create a 'csv' file with the list of species suitable for modeling and their species codes (CEP names) to be used for the seed mixture or donor grassland composition")
                                            )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 6"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step6.PNG", height = 105, width = 350),
                                            tags$hr(),
                                            br(),
                                            img(src = "exampleDefCep.PNG", height = 200, width = 350)
                                     ),
                                     column(5,
                                            p("Click the Download button to download the 'csv' file with the list of species suitable for modeling and their species codes (CEP names) to be used for the seed mixture or donor grassland composition")
                                            )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 7"),style="color:red"),
                                     column(5,
                                            img(src = "exampleSeedMix.PNG", height = 210, width = 350)
                                     ),
                                     column(5,
                                            p("In a excel sheet create the mixture or donor grassland composition database, which is characterized by two columns:"),
                                            p(strong("First column:"),"species code abbreviated in CEP names format, retrievable from the downloaded csv file"),
                                            p(strong("Second column:"),"abundance of each species. Abundance must be a number bounded between 0 and 100, which can be either a species relative abundance or a species cover (sensu Pittarello et al., 2016; Verdinelli et al., 2022"),
                                            p("Save the file in 'csv' format, semicolon separated"),
                                            p("The total abundance of the seed mixture or donor grassland composition should not necessarily amount to 100%.")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 8"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step8.PNG", height = 175, width = 350)
                                     ),
                                     column(5,
                                            p("Upload the seed mixture or donor grassland composition in a 'csv' format")
                                     )
                                   ),
                                   tags$hr(),
                                   fluidRow(
                                     h3(strong("Step 9"),style="color:red"),
                                     column(5,
                                            img(src = "cus_step9.PNG", height = 400, width = 350)
                                     ),
                                     column(5,
                                            p("Set the topographical values of the restoration site and then press the RUN button")
                                     )
                                   )
                                   )#end tabpanel
                        )#end tabset panel
                      )#end fluid row
                      ),#end tabpanel
             
             # 3 - WEB APP ----           
             
             tabPanel("Web App",
                      fluidRow(
                        # SIDE BAR PANEL -------------------------------
                        column(4,
                               wellPanel(
                                 # question 1 ----
                                 radioButtons("radio",label = h4(strong("1 - Select training database:")), 
                                              choices = list("Default (Piedmont, Italy)" = "default", "Customized" = "custom"), 
                                              selected = "default"),
                                 
                                 ## question 1 = default ----
                                 conditionalPanel(
                                   condition = "input.radio == 'default'",
                                   h4(strong("2 - Download species codes (CEP names)")),
                                   downloadButton("PiemonteCodes", "Download"),
                                   tags$hr(),
                                   h4(strong("3 - Upload Seed mixture or donor grassland composition")),
                                   fileInput("uploadMiscela", "CSV File (semicolon separated)",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),
                                   checkboxInput("header", "Header", TRUE),
                                   
                                   tags$hr(),
                                   h4(strong("4 - Topographic factors of restoration site")),
                                   numericInput("altitudine", label = "Elevation (m)", value=1500,min = 0,max=3000),
                                   numericInput("esposizione", label = "Aspect (°)",value=180, min = 0,max=360),
                                   numericInput("pendenza", label = "Slope (°)", value=15,min = 0,max=100),
                                   
                                   tags$hr(),
                                   actionButton("bottone", "Run")
                                 ),#conditional panel
                                 
                                 ## question 1 = custom -------
                                 conditionalPanel(
                                   condition = "input.radio == 'custom'",
                                   h4(strong("2 - Upload a database with vegetation and topographical variables")),
                                   fileInput("uploadDB", "CSV File (semicolon separated)",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),
                                   checkboxInput("header", "Header", TRUE),
                                   tags$hr(),
                                   h4(strong("3 - Set the following parameters:")),
                                   numericInput("freqSpecies", label = "Minimum species frequency", value=50,min = 30,max=10000),
                                   numericInput("minAbundSpecies", label = "Minimum species abundance", value=0.3,min = 0,max=100),
                                   tags$hr(),
                                   h4(strong("4 - Generate species codes (CEP names)")),
                                   actionButton("bottoneOwn", "Generate")
                                   ,
                                   tags$hr(),
                                   h4(strong("5 - Download species codes (CEP names)")),
                                   downloadButton("OwnSpeCodes", "Download"),
                                   tags$hr(),
                                   h4(strong("6 - Upload Seed mixture or donor grassland composition")),
                                   fileInput("uploadMiscela1", "CSV File (semicolon separated)",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),
                                   checkboxInput("header", "Header", TRUE),
                                   tags$hr(),
                                   h4(strong("7 - Topographic factors of restoration site")),
                                   numericInput("altitudine1", label = "Elevation (m)", value=1500,min = 0,max=3000),
                                   numericInput("esposizione1", label = "Aspect (°)",value=180, min = 0,max=360),
                                   numericInput("pendenza1", label = "Slope (°)", value=15,min = 0,max=100),
                                   tags$hr(),
                                   actionButton("bottone1", "Run")
                                 )#conditional panel
                                 
                               ) #wellPanel
                        ),#column
                        # MAIN PANEL -------------------------------
                        
                        column(8,
                               ## question 1 = default ----
                               conditionalPanel(condition = "input.radio == 'default'",
                                                tabsetPanel(
                                                  type="tabs",
                                                  tabPanel("Data preview",
                                                           h4(strong("Seed mixture or donor grassland composition")),
                                                           tableOutput("mixture_input")
                                                  ),
                                                  tabPanel("Analysis output",
                                                           h4(strong("Descriptives")),
                                                           tableOutput("descrittive"),
                                                           h4(strong("Species abundances")),
                                                           tableOutput("valori_predetti"),
                                                           plotOutput("barplot",width = 800,height = 350),
                                                           h4(strong("Indexes")),
                                                           fluidRow(
                                                             column(2,
                                                                    tableOutput("indici"),
                                                                    ),
                                                             column(4,
                                                                    plotOutput("plot",width = 350,height = 400)
                                                                  )
                                                           
                                                           )#end fluid row
                                                  ),#end tab panel
                                                  tabPanel("Output glossary",
                                                           h4(strong("Descriptives")),
                                                           h6(strong("cep.names:"),p("Species name in CEP format")),
                                                           h6(strong("species:"),p("Full species name")),
                                                           h6(strong("n.obs:"),p("Number of observations of each species selected for modeling")),
                                                           h6(strong("min.ele:"),p("Minimum elevation at which a species occurred")),
                                                           h6(strong("max.ele:"),p("Maximum elevation at which a species occurred")),
                                                           h6(strong("min.slope:"),p("Minimum slope at which a species occurred")),
                                                           h6(strong("max.slope:"),p("Maximum slope at which a species occurred")),
                                                           h6(strong("min.south:"),p("Minimum southness value at which a species occurred")),
                                                           h6(strong("max.south:"),p("Maximum southness value at which a species occurred")),
                                                           
                                                           h4(strong("Species abundances")),
                                                           h6(strong("PMA:"),p("Predicted Maximum Abundance. It is the maximum achievable abundance of a species in the restoration site, as predicted by the best model")),
                                                           h6(strong("POA:"),p("Predicted Optimal Abundace. It is the maximum achievable abundance of a species in its optimal ecological condition, based on all possible combinations of elevation, slope and southness.")),
                                                           h6(strong("ratio:"),p("Ratio between the PMA and POA. This ratio indicates how far (ratio = 0) or close (ratio = 1) a species is from its ecological optimum.")),
                                                           h6(strong("R2.adj:"),p("R square adjusted of the best Generalized Additive Model")),
                                                           h6(strong("RMSE:"),p("Root Mean Squared Error of the best Generalized Additive Model")),
                                                           h6(strong("SmDgA:"),p("Seed mixture or Donor grassland Abundance. Abundance of a species listed in the seed mixture or donor grassland composition imported by the user and eligible for modeling")),
                                                           h6(strong("EA:"),p("Expected Abundance. The highest achievable abundance of a species in a restoration site, based on how far the species is from the ecological optimum (i.e. computed from the multiplication of SmDgA by the ratio)")),
                                                           
                                                           h4(strong("Indexes")),
                                                           h6(strong("SI:"),p("Suitability Index (SI). Suitability of a seed mixture or donor grassland to restore a site with specific topographic characteristics. It ranges between 0 and 1. When SI=0 the restoration site is totally beyond the optimal ecological ranges of all species of the seed mixture or donor grassland, which is therefore not appropriate for the site restoration. Conversely, when SI=1 the restoration site has the optimal ecological conditions for all species of the seed mixture or donor grassland, which is therefore perfectly appropriate for the site restoration.")),
                                                           h6(strong("RI:"),p("Reliability Index (RI). Index of the reliability of the Suitability Index (SI). The RI ranges between 0 and 1. When RI is close to 0 it means that few to none species contribute to the computation of the SI, whereas when RI is close to 1 the SI is computed with most to all species. Therefore, the higher is the RI, the most reliable is the SI. Not all the species of the seed mixture and donor grassland composition may modeled as i) they can be missing from the training database or ii) the values of the topographic factors of the restoration site are beyond their ecological ranges (e.g. if the elevation of the restoration site is 250 m and a species as an elevation range bounded between 1000 and 3000 m, such a species cannot be modeled)"))
                                                            ),#end tab panel
                                                           
                                                )#tabset panel
                               ),#conditional panel
                               ## question 1 = custom -------
                               conditionalPanel(condition = "input.radio == 'custom'",
                                                tabsetPanel(
                                                  type="tabs",
                                                  tabPanel("Data preview",
                                                           column(6,
                                                                  h4(strong("Species codes (CEP names)")),
                                                                  tableOutput("codici.anteprima")
                                                           ),
                                                           column(6,
                                                                  h4(strong("Seed mixture or donor grassland composition")),
                                                                  tableOutput("mixture_input1")
                                                           )
                                                  ),#tabpanel
                                                  tabPanel("Analysis output",
                                                           h4(strong("Descriptives")),
                                                           tableOutput("descrittive1"),
                                                           h4(strong("Species abundances")),
                                                           tableOutput("valori_predetti1"),
                                                           plotOutput("barplot1",width = 800,height = 350),
                                                           h4(strong("Indexes")),
                                                           fluidRow(
                                                             column(2,
                                                                    tableOutput("indici1"),
                                                             ),
                                                             column(4,
                                                                    plotOutput("plot1",width = 350,height = 400)
                                                             )
                                                             
                                                           )#end fluid row
                                                           ),#tabpanel
                                                  tabPanel("Output glossary",
                                                           h4(strong("Descriptives")),
                                                           h6(strong("cep.names:"),p("Species name in CEP format")),
                                                           h6(strong("species:"),p("Full species name")),
                                                           h6(strong("n.obs:"),p("Number of observations of each species selected for modeling")),
                                                           h6(strong("min.ele:"),p("Minimum elevation at which a species occurred")),
                                                           h6(strong("max.ele:"),p("Maximum elevation at which a species occurred")),
                                                           h6(strong("min.slope:"),p("Minimum slope at which a species occurred")),
                                                           h6(strong("max.slope:"),p("Maximum slope at which a species occurred")),
                                                           h6(strong("min.south:"),p("Minimum southness value at which a species occurred")),
                                                           h6(strong("max.south:"),p("Maximum southness value at which a species occurred")),
                                                           
                                                           h4(strong("Species abundances")),
                                                           h6(strong("PMA:"),p("Predicted Maximum Abundance. It is the maximum achievable abundance of a species in the restoration site, as predicted by the best model")),
                                                           h6(strong("POA:"),p("Predicted Optimal Abundace. It is the maximum achievable abundance of a species in its optimal ecological condition, based on all possible combinations of elevation, slope and southness.")),
                                                           h6(strong("ratio:"),p("Ratio between the PMA and POA. This ratio indicates how far (ratio = 0) or close (ratio = 1) a species is from its ecological optimum.")),
                                                           h6(strong("R2.adj:"),p("R square adjusted of the best Generalized Additive Model")),
                                                           h6(strong("RMSE:"),p("Root Mean Squared Error of the best Generalized Additive Model")),
                                                           h6(strong("SmDgA:"),p("Seed mixture or Donor grassland Abundance. Abundance of a species listed in the seed mixture or donor grassland composition imported by the user and eligible for modeling")),
                                                           h6(strong("EA:"),p("Expected Abundance. The highest achievable abundance of a species in a restoration site, based on how far the species is from the ecological optimum (i.e. computed from the multiplication of SmDgA by the ratio)")),
                                                           
                                                           h4(strong("Indexes")),
                                                           h6(strong("SI:"),p("Suitability Index (SI). Suitability of a seed mixture or donor grassland to restore a site with specific topographic characteristics. It ranges between 0 and 1. When SI=0 the restoration site is totally beyond the optimal ecological ranges of all species of the seed mixture or donor grassland, which is therefore not appropriate for the site restoration. Conversely, when SI=1 the restoration site has the optimal ecological conditions for all species of the seed mixture or donor grassland, which is therefore perfectly appropriate for the site restoration.")),
                                                           h6(strong("RI:"),p("Reliability Index (RI). Index of the reliability of the Suitability Index (SI). The RI ranges between 0 and 1. When RI is close to 0 it means that few to none species contribute to the computation of the SI, whereas when RI is close to 1 the SI is computed with most to all species. Therefore, the higher is the RI, the most reliable is the SI. Not all the species of the seed mixture and donor grassland composition may modeled as i) they can be missing from the training database or ii) the values of the topographic factors of the restoration site are beyond their ecological ranges (e.g. if the elevation of the restoration site is 250 m and a species as an elevation range bounded between 1000 and 3000 m, such a species cannot be modeled)"))
                                                  )#end tab panel
                                                  )#tabset panel
                               )#conditional panel
                               
                        )#column
                      )#fluid page
             ),#tab panel 1
             
             # 4 - APP in R Studio ----           
             
             tabPanel("App in R studio",
                      h1(strong("Internet connection needed")),
                      br(),
                      fluidRow(
                        column(5,
                               p("In R Studio run the following code:"),
                               p(code("install.packages('shiny')"),
                                 br(),
                                 code("library(shiny)"),
                                 br(),
                                 code("runGitHub('ResNatSeed_ShinyApp','MarcoPittarello')"),
                                 style="text-align:left;
                           color:black;
                           background-color:grey90;
                           padding:5px;
                           border-radius:2px"
                               )#paragraph
                        )#end column
                      ),#end fluid row
                      h1(strong("Internet connection NOT needed")),
                      br(),
                      fluidRow(
                        column(9,
                               p("The internet connection is not needed except for the 
                                 first two steps, which however are required only once to download locally the app."),
                               p("The steps are:"),
                               p("1 - go", a(href="https://github.com/MarcoPittarello/ResNatSeed_ShinyApp", "here"),
                                 br(),
                                 "2 - click on 'CODE' (green button) and then 'DOWNLOAD ZIP' file, named ‘ResNatSeed_ShinyApp-master’",
                                 br(),
                                 "3 - unzip the downloaded file (from now on, it will no longer be necessary to have an internet connection)",
                                 br(),
                                 "4 - open the unzipped folder and click on ‘app.R’, R studio will open",
                                 br(),
                                 "5 – click ‘Run App’ on the right-upper corner of source panel and enjoy the App!!"
                               )#end paragraph
                               )#end column
                      )#end fluidrow
                      ),#tabpanel
             
             # 5 - CONTACTS ----           
             
             tabPanel("Contacts",
                      # fluidRow(
                      #   column(5,
                      #          img(src = "pitta.jpg", height = 120, width = 110)
                      #   ),
                      #   column(5,
                      #          img(src = "disafa.jpg", height = 120, width = 110)
                      #   )
                      # ),
                      fluidRow(
                        h1(strong("Web app and R package developers"),style="text-align:center;color:red")),
                      fluidRow(
                        column(5,
                               h2("Marco Pittarello"),
                               p("", a(href="mailto:marco.pittarello@unito.it","marco.pittarello@unito.it")),
                               br(),
                               p("Affiliation: ",a(href="https://www.veterinaria.unito.it/do/home.pl","Department of Veterinary Sciences, University of Torino, Largo Paolo Braccini 2 - Grugliasco (Torino)")),
                               br(),
                               p("Useful links:"),
                               p(a(href="https://www.clmveterinaria.unito.it/persone/marco.pittarello","Personal Institutional Web-page",
                                   target="_blank",
                                   style="text-align:center;color:blue")),
                               p(a(href="https://github.com/MarcoPittarello","Github",
                                   target="_blank",
                                   style="text-align:center;color:blue")),
                               p(a(href="https://www.researchgate.net/profile/Marco-Pittarello","Researchgate",
                                   target="_blank",
                                   style="text-align:center;color:blue"))
                               
                        ),
                        column(5,
                               h2("Davide Barberis"),
                               p("", a(href="mailto:d.barberis@unito.it","d.barberis@unito.it")),
                               br(),
                               p("Affiliation: ",a(href="https://www.disafa.unito.it/do/home.pl","Department of Agricultural, Forest and Food Sciences, University of Torino, Largo Paolo Braccini 2 - Grugliasco (Torino)")),
                               br(),
                               p("Useful links:"),
                               p(a(href="https://www.disafa.unito.it/persone/d.barberis","Personal Institutional Web-page",
                                   target="_blank",
                                   style="text-align:center;color:blue")),
                               p(a(href="https://github.com/davidebarberis","Github",
                                   target="_blank",
                                   style="text-align:center;color:blue")),
                               p(a(href="https://www.researchgate.net/profile/Davide-Barberis","Researchgate",
                                   target="_blank",
                                   style="text-align:center;color:blue"))
                        )
                        ),
                      tags$hr(),
                      fluidRow(
                        h1(strong("Coordinators"),style="text-align:center;color:red")
                      ),
                      fluidRow(
                        column(5,
                               h2("Michele Lonati"),
                               p("", a(href="mailto:michele.lonati@unito.it","michele.lonati@unito.it")),
                               
                               br(),
                               p("Affiliation: ",a(href="https://www.disafa.unito.it/do/home.pl","Department of Agricultural, Forest and Food Sciences, University of Torino, Largo Paolo Braccini 2 - Grugliasco (Torino)")),
                               br(),
                               p("Useful links:"),
                               p(a(href="https://www.disafa.unito.it/persone/michele.lonati","Personal Institutional Web-page",
                                   target="_blank",
                                   style="text-align:center;color:blue")
                               )
                               ),
                        column(5,
                             h2("Giampiero Lombardi"),
                             p("", a(href="mailto:giampiero.lombardi@unito.it","giampiero.lombardi@unito.it")),
                             br(),
                             p("Affiliation: ",a(href="https://www.disafa.unito.it/do/home.pl","Department of Agricultural, Forest and Food Sciences, University of Torino, Largo Paolo Braccini 2 - Grugliasco (Torino)")),
                             br(),
                             p("Useful links:"),
                             p(a(href="https://www.disafa.unito.it/persone/giampiero.lombardi","Personal Institutional Web-page",
                                 target="_blank",
                                 style="text-align:center;color:blue")
                      )
                      )
                      )
                      )#tabpanel
  )#nav page
)


# ^^^^^ SERVER - shiny app ------------------------------------------

server <- function(input, output, session) {
  
  # question 1 = default ----
  # side bar panel --------------------------------------
  
  ceppiem<-ResNatSeed::cep.piem
  
  ## 2 - Download species codes (CEP names)
  output$PiemonteCodes <- downloadHandler(
    filename = function() {
      paste("PiemonteItaly-Species-CepNames" ,".csv", sep="")
    },
    content = function(file) {
      write.table(ceppiem,file,sep = ";",row.names = F)
    }
  )
  
  # after clicking run (bottone) do this:
  output.funzione<-eventReactive(input$bottone, {
    progress <- Progress$new(session, min=1, max=30)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    for (i in 1:30) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    
    ## 3 - Upload Seed mixture or donor grassland composition
    inFile2 <- input$uploadMiscela
    composizione<-read.csv(inFile2$datapath, header = input$header,sep = ";")
    
    ## 4 - Topographic factors of restoration site
    a<-RestInd(trainingDB=NULL,
               composition = composizione,
               elevation = input$altitudine,
               aspect = input$esposizione,
               slope = input$pendenza)
    
    descrittive<-a$DESCRIPTIVES
    valori_predetti<-a$SPECIES_ABUNDANCES
    indici<-a$INDEXES
    
    plot<-data.frame(a$INDEXES)
    
    list(descrittive=descrittive,valori_predetti=valori_predetti,indici=indici,plot=plot)
    
  })
  
  # main panel --------------------------------------
  ## Data preview  - Species codes (CEP names)
  output$mixture_input <- renderTable({
    inFile1 <- input$uploadMiscela
    
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header = input$header,sep = ";")
  })
  
  ## Data analysis
  output$descrittive<-renderTable({output.funzione()$descrittive})
  output$indici<-renderTable({output.funzione()$indici})
  output$valori_predetti<-renderTable({output.funzione()$valori_predetti})
  output$plot<-renderPlot({
    
    db<-output.funzione()$plot
    
    grafico1<-ggplot(db,aes(y=db[1,1],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="darkgreen")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Suitability Index (SI)")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "grey90",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90))
    
    grafico2<-ggplot(db,aes(y=db[1,2],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="cyan3")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Reliability Index (RI)")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "grey90",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90))
    
    ggarrange(grafico1, grafico2,ncol = 2, nrow = 1)
  })
  
  output$barplot<-renderPlot({
    
    db.barplot<-output.funzione()$valori_predetti
    sel<-db.barplot[,c(1,8,9)]
    sel<-sel[complete.cases(sel), ] 
    
    sel %>% pivot_longer(cols = 2:3,names_to = "mix.exp",values_to = "abundance") %>% 
      ggplot(.,aes(x=as.factor(cep.names),y=abundance,fill=mix.exp)) +
      geom_col(position=position_identity(),color="black")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(color="grey10",size=12,angle = 0),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "white",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90),
            legend.position = "bottom")+
      scale_fill_manual(values = c( "darkgreen", "darkolivegreen1"),
                        labels=c("EA", "SmDgA"))+
      labs(fill = "")
  }) 
  
  # question 1 = custom ----
  # side bar panel --------------------------------------
  
  ## when clicking "generate" button (bottoneOwn) : 4 - Generate species codes (CEP names) 
  button.generate<-eventReactive(input$bottoneOwn,{
    progress <- Progress$new(session, min=1, max=30)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    
    ### 2 - Upload customized training database
    custom.training <- input$uploadDB
    own.sourceDB<-read.csv(custom.training$datapath, header = input$header,sep = ";")
    
    
    ### 3 - Set the following parameters:
    sourceDB.output<-trainingDB(data = own.sourceDB,spe.freq = input$freqSpecies,min.spe.abundance = input$minAbundSpecies)
    
    Own.cep.names<-data.frame(sourceDB.output$cep.names)
    Own.ResNatSeedDB<-data.frame(sourceDB.output$trainingDB.ResNatSeed)
    
    list(cep.names=Own.cep.names,ResNatSeedDB=Own.ResNatSeedDB)
  })
  
  
  ## 5 - Download species codes (CEP names)
  output$OwnSpeCodes <- downloadHandler(
    filename = "Custom_Species-CepNames.csv",
    content = function(file) {
      write.table(button.generate()$cep.names,file,sep = ";",row.names = F)
    })
  
  
  
  # bottone run (bottone1)
  output.funzione1<-eventReactive(input$bottone1, {
    progress1 <- Progress$new(session, min=1, max=30)
    on.exit(progress1$close())
    
    progress1$set(message = 'Calculation in progress',
                  detail = 'This may take a while...')
    
    for (i in 1:30) {
      progress1$set(value = i)
      Sys.sleep(0.5)
    }
    
    ##6 - Upload Seed mixture or donor grassland composition
    inFile.miscela <- input$uploadMiscela1
    composizione1<-read.csv(inFile.miscela$datapath, header = input$header,sep = ";")
    
    ## 7 - Topographic factors of restoration site"
    a1<-RestInd(trainingDB = button.generate()$ResNatSeedDB,
                composition = composizione1,
                elevation = input$altitudine1,
                aspect = input$esposizione1,
                slope = input$pendenza1)
    
    descrittive1<-a1$DESCRIPTIVES
    valori_predetti1<-a1$SPECIES_ABUNDANCES
    indici1<-a1$INDEXES
    
    plot1<-data.frame(a1$INDEXES)
    
    list(descrittive1=descrittive1,valori_predetti1=valori_predetti1,indici1=indici1,plot1=plot1)
    
  })
  
  # main panel --------------------------------------
  
  ## Data preview  - Species codes (CEP names)
  output$codici.anteprima<-renderTable({button.generate()$cep.names})
  
  ## Data preview  - Seed mixture or donor grassland composition
  output$mixture_input1 <- renderTable({
    inFile <- input$uploadMiscela1
    
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header,sep = ";")
  })
  
  ## Analysis output
  output$descrittive1<-renderTable({output.funzione1()$descrittive1})
  output$indici1<-renderTable({output.funzione1()$indici1})
  output$valori_predetti1<-renderTable({output.funzione1()$valori_predetti1})
  output$plot1<-renderPlot({
    db1<-output.funzione1()$plot1
    
    grafico1a<-ggplot(db1,aes(y=db1[1,1],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="red")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Mixture Suitability index")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "grey90",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90))
    
    grafico2a<-ggplot(db1,aes(y=db1[1,2],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="green")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Model Reliability index")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "grey90",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90))
    
    
    ggarrange(grafico1a, grafico2a,ncol = 2, nrow = 1)
  })
  
  output$barplot1<-renderPlot({
    
    db.barplot1<-output.funzione1()$valori_predetti
    sel1<-db.barplot1[,c(1,8,9)]
    sel1<-sel1[complete.cases(sel1), ] 
    
    sel1 %>% pivot_longer(cols = 2:3,names_to = "mix.exp",values_to = "abundance") %>% 
      ggplot(.,aes(x=as.factor(cep.names),y=abundance,fill=mix.exp)) +
      geom_col(position=position_identity(),color="black")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(color="grey10",size=12,angle = 0),
            axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = "white",colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major.y  = element_line(colour="black",size = rel(0.5),linetype = "dotted"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(color="black", size=12, angle=0),
            axis.title.y = element_text(color="black", size=16, angle=90),
            legend.position = "bottom")+
      scale_fill_manual(values = c( "darkgreen", "darkolivegreen1"),
                        labels=c("EA", "SmDgA"))+
      labs(fill = "")
  }) 
}

shinyApp(ui, server)
