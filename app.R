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

library(ResNatSeed)


#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='marco-pittarello', token='597E84CE5CC18F11E063F82C92A9A105', secret='j2uJO/bWQBYyKs+WLSqKYS45LtMdt/2MeU7TZd8W')

# ^^^^^ UI - shiny app ------------------------------------------
ui <- fluidPage(
  titlePanel("ResNatSeed: a tool for bla bla bla blaalalalalal"),
  theme = shinytheme("cosmo"),
  navbarPage("ResNatSeed",
             
             # 1 - HOME PAGE----           
             tabPanel(icon("home"),
                      fluidRow(
                        column(1,
                               img(src = "disafa.jpg", height = 70, width = 70)
                        ),#end column
                        column(8,
                               h1(strong("Welcome to ResNatSeed Web app",
                                         style="text-align:center;
                                         color:black;
                                         background-color:lightgreen;
                                         padding:15px;
                                         border-radius:10px"))
                        )
                      ),#end fluidrow
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        br(),
                        p("ResNatSeed is......................."),
                        p("Information in the paper..........................."),
                        div("prova prova", style = "color:blue")
                      )
                      
             ),#end tabpanel
             
             # 2 - INSTRUCTIONS----           
             
             tabPanel("Instructions",
                      fluidRow(
                        h1(strong("Step 1")),
                        p("papapdspdspadpsadpsadpasdpasd"),
                        column(6,
                               img(src = "step1.PNG", height = 150, width = 600)
                        ),
                        column(5,
                               p("clicca li e divertiti a manetta",
                                 style="text-align:center;
                                         color:black;
                                         background-color:chartreuse;
                                         padding:15px;
                                         border-radius:10px")
                        )
                      ),
                      tags$hr(),
                      br(),
                      fluidRow(
                        h1("Step 2"),
                        p("papapdspdspadpsadpsadpasdpasd"),
                        column(6,
                               img(src = "step1.PNG", height = 150, width = 600)
                        ),
                        column(4,
                               p("clicca li e divertiti a manetta")
                        )
                      ),
                      tags$hr(),
                      br(),
                      fluidRow(
                        h1("Step 3"),
                        p("papapdspdspadpsadpsadpasdpasd"),
                        column(6,
                               img(src = "step1.PNG", height = 150, width = 600)
                        ),
                        column(4,
                               p("clicca li e divertiti a manetta")
                        )
                      ),
                      tags$hr(),
                      br(),
             ),
             
             # 3 - APP ----           
             
             tabPanel("App",
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
                                   h4(strong("3 - Upload mixture/donor grassland composition")),
                                   fileInput("uploadMiscela", "Choose CSV File",
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
                                   h4(strong("2 - Upload customised training database")),
                                   fileInput("uploadDB", "Choose CSV File",
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
                                   h4(strong("6 - Upload mixture/donor grassland composition")),
                                   fileInput("uploadMiscela1", "Choose CSV File",
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
                                                           h4(strong("Mixture/donor grassland composition")),
                                                           tableOutput("mixture_input")
                                                  ),
                                                  tabPanel("Analysis output",
                                                           h4(strong("Descriptives")),
                                                           tableOutput("descrittive"),
                                                           h4(strong("Surveyed and predicted abundance")),
                                                           tableOutput("valori_predetti"),
                                                           h4(strong("Indexes")),
                                                           tableOutput("indici"),
                                                           plotOutput("plot",width = 500,height = 500))
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
                                                                  h4(strong("Mixture/donor grassland composition")),
                                                                  tableOutput("mixture_input1")
                                                           )
                                                  ),#tabpanel
                                                  tabPanel("Analysis output",
                                                           h4(strong("Descriptives")),
                                                           tableOutput("descrittive1"),
                                                           h4(strong("Surveyed and predicted abundance")),
                                                           tableOutput("valori_predetti1"),
                                                           h4(strong("Indexes")),
                                                           tableOutput("indici1"),
                                                           plotOutput("plot1",width = 500,height = 500)
                                                  )#tabpanel        
                                                )#tabset panel
                               )#conditional panel
                               
                        )#column
                      )#fluid page
             ),#tab panel 1
             
             # 4 - CONTACTS ----           
             
             tabPanel("Contacts",
                      h1("Lista contatti"),
                      br(),
                      a(href="https://www.unito.it/", "Unito.it",target="_blank",
                        style="text-align:center;color:black"),
                      br(),
                      p("Marco Pittarello: ", a(href="mailto:marco.pittarello@unito.it","marco.pittarello@unito.it")),
                      p("Davide Barberis: ", a(href="mailto:d.barberis@unito.it","d.barberis@unito.it")),
                      br(),
                      
                      
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
    
    ## 3 - Upload mixture/donor grassland composition
    inFile2 <- input$uploadMiscela
    composizione<-read.csv(inFile2$datapath, header = input$header,sep = ";")
    
    ## 4 - Topographic factors of restoration site
    a<-RestInd(trainingDB=NULL,
               composition = composizione,
               elevation = input$altitudine,
               aspect = input$esposizione,
               slope = input$pendenza)
    
    descrittive<-a$DESCRIPTIVES
    valori_predetti<-a$SURVEYED_AND_PREDICTED_ABUNDANCE
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
    
    grafico1<-ggplot(db,aes(y=db[1,1],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="red")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Mixture Suitability Index (MSI)")+
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
    
    grafico2<-ggplot(db,aes(y=db[1,2],x=1))+geom_bar(stat = "identity",width = 4,color="black",fill="green")+
      expand_limits(y = c(0, 1))+
      scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05))+
      ylab("Model Reliability Index (MRI)")+
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
    
    ### 2 - Upload customised training database
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
    
    ##6 - Upload mixture/donor grassland composition
    inFile.miscela <- input$uploadMiscela1
    composizione1<-read.csv(inFile.miscela$datapath, header = input$header,sep = ";")
    
    ## 7 - Topographic factors of restoration site"
    a1<-RestInd(trainingDB = button.generate()$ResNatSeedDB,
                composition = composizione1,
                elevation = input$altitudine1,
                aspect = input$esposizione1,
                slope = input$pendenza1)
    
    descrittive1<-a1$DESCRIPTIVES
    valori_predetti1<-a1$SURVEYED_AND_PREDICTED_ABUNDANCE
    indici1<-a1$INDEXES
    
    plot1<-data.frame(a1$INDEXES)
    
    list(descrittive1=descrittive1,valori_predetti1=valori_predetti1,indici1=indici1,plot1=plot1)
    
  })
  
  # main panel --------------------------------------
  
  ## Data preview  - Species codes (CEP names)
  output$codici.anteprima<-renderTable({button.generate()$cep.names})
  
  ## Data preview  - Mixture/donor grassland composition
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
      ylab("Mixture suitability index")+
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
      ylab("Model reliability index")+
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
}

shinyApp(ui, server)
