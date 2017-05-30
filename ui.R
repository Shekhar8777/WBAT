library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "E-Analyzer"
    
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DashBoard", icon = icon("fa fa-tachometer"), tabName = "dashboard"),
      menuItem("Word Count", icon = icon("fa fa-commenting-o"), tabName = "wordcount"),
      menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis")
     
    )
    
   # radioButtons(inputId = 'sep',label = 'Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=''),selected =',' ),
    #HTML('</br>'),
    #HTML('</br>'),
    #radioButtons('format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
    #downloadButton('downloadReport')
    
    ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ),
    tabItems(
      tabItem(tabName = "dashboard")
    ),
  
    tabItems(
      tabItem(tabName = "wordcount",
            fluidRow(
              column(4,
                sliderInput("topn",label="Select top n highest frequency words to display",min=1,max=100,step=1,value=10)
              ),
              column(4,
                actionButton("example",label="Example"),
                actionButton("clear",label="Clear Text")
              )
              ),
            fluidRow(
              tags$p(" Text input:"),
              fluidRow(
                box(width=11,tags$textarea(id="text",rows="5",style="width:100%",""))
              ),
              fluidRow(
                box(width=11,textOutput("char_count"))
              ),
              fluidRow(
                box(width=11,textOutput("total_count"))
              ),
              tags$p(" Seperate word count:"),
              fluidRow(
                box(width=11,plotOutput("seperate_bar",width="100%",height="500px"))
              )
            )
            ),
    tabItem(tabName = "analysis",
      fluidRow(
              column(4,
              selectInput("dataset", h5("Choose a dataset:"), choices = c("cars", "longley","rock", "pressure","Uploaded Data"))       
              ),
              column(4,
              fileInput('file','Choose file to upload.')
              )
            ),
    
    fluidRow(
      tabBox(
        width = "100%",
        tabPanel("Data",DT::dataTableOutput('table')),
        tabPanel("Summary", tableOutput("sum")),
        tabPanel("Clustering",
                 HTML('<br>'),
                 tabBox(
                   width = "100%",
                   tabPanel("Kmeans", 
                            fluidRow(
                              column(4,
                                     selectInput('xcol', 'X Variable',"")
                              ),
                              column(4,
                                     selectInput('ycol', 'Y Variable',"")
                              ),
                              column(4,
                                     numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)
                              )
                            ),
                            # Create a new row for the table.
                            fluidRow(
                              box(title="ScatterPlot", width="100%",background = "light-blue",plotOutput('plot1' ,width = "100%" ))
                            )
                            
                   ),
                   tabPanel("Fuzzy c-means",
                            fluidRow(
                              column(4,
                                     selectInput('fc_xcol', 'X Variable',"")
                              ),
                              column(4,
                                     selectInput('fc_ycol', 'Y Variable',"")
                              ),
                              column(4,
                                     numericInput('fc_clusters', 'Cluster count', 3,min = 1, max = 9)
                              )
                            ),
                            # Create a new row for the table.
                            fluidRow(
                              box(title="ScatterPlot", width="100%",background = "light-blue",plotOutput('fc_plot' ,width = "100%" ))
                            )
                            )
                 )
          
        ),
        
        tabPanel("Regression",
                 
                 HTML('<br>'),
                 fluidRow(
                   column(4,
                          uiOutput('dv')
                   ),
                   column(4,
                          uiOutput('iv')
                   )
                 ),
                 fluidRow(
                 tabBox(width = "100%",
                   tabPanel("Histograms",                   
                            plotOutput("distPlot_dv"),
                            sliderInput("bins_dv", "Number of bins:", min = 1, max = 50, value = 7),
                            #textInput("text_hist_dv", label = "Interpretation", value = "Enter text..."),
                            
                            plotOutput("distPlot_iv"),
                            sliderInput("bins_iv", "Number of bins:", min = 1, max = 50, value = 7)
                            #textInput("text_hist_iv", label = "Interpretation", value = "Enter text...")
                            ),                       
                   
                   tabPanel("Scatter Plot",                   
                            plotOutput("scatter")
                            #textInput("text_scatter", label = "Interpretation", value = "Enter text...")
                            ),  
                   
                   tabPanel("Correlations",                   
                            htmlOutput("corr"),
                            HTML('</br> </br>')
                            #textInput("text_correlation", label = "Interpretation", value = "Enter text...")
                            ),
                   tabPanel("Model",                   
                            verbatimTextOutput("model")
                            #textInput("text_model", label = "Interpretation", value = "Enter text...")
                            ),
                   
                   tabPanel("Residuals",                   
                           plotOutput("residuals_hist"),
                            plotOutput("residuals_scatter")
                            #plotOutput("residuals_qqline")
                            #textInput("text_residuals", label = "Interpretation", value = "Enter text...")
                            )
                   
                 
                 
                 #radioButtons('format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
                 #downloadButton('downloadReport')
                 ))
         ),
        tabPanel("Classification",
                 HTML('<br>'),
                 tabBox(
                   width = "100%",
                   tabPanel("SVM(Support Vector Machine)", 
                            fluidRow(
                              column(4,
                                     uiOutput('svm_dv')
                              ),
                              column(4,
                                     uiOutput('svm_iv')
                              ),
                              column(4,
                                     selectInput("kernel", h5("Kernel"), choices = c("radial","linear", "polynomial","sigmoid"))       
                              )
                            ),
                            # Create a new row for the table.
                            fluidRow(
                              tabBox(width = "100%",
                              tabPanel("Scatter Plot",                   
                                       box(title="ScatterPlot", width="100%",background = "light-blue",plotOutput('svm_plot' ,width = "100%" ))
                                       #textInput("text_scatter", label = "Interpretation", value = "Enter text...")
                              ),  
                              tabPanel("Model",                   
                                       verbatimTextOutput("svm_model")
                                       #textInput("text_model", label = "Interpretation", value = "Enter text...")
                              ),
                              tabPanel("Residual",                   
                                       verbatimTextOutput("residual_svm")
                                       
                                       #textInput("text_model", label = "Interpretation", value = "Enter text...")
                              ),
                              tabPanel("Root Mean Square Error",                   
                                       HTML("<p>Error</p>"),
                                       verbatimTextOutput("error_svm")
                                       #textInput("text_model", label = "Interpretation", value = "Enter text...")
                              )
                              
                            )
                            
                   )
                 )
                 
        )
         
       )
      )
    )
  )
)
)
)