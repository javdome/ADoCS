fluidPage(
  titlePanel("ADoCS:Automatic Scheduling of Conference Papers"),
  tags$div(
    HTML("Diego Vallejo, Paulina Morillo and Cèsar Ferri"),
    tags$br(),
    HTML("<a href='https://github.com/dievalhu/ADoCS'>ADoCS project in github</a>"),
    HTML("Examples:"),
    HTML("<a href='https://github.com/dievalhu/ADoCS/blob/master/aaai13.csv'>AAAI13</a>"),
    HTML("<a href='https://github.com/dievalhu/ADoCS/blob/master/aaai14.csv'>AAAI14</a>"),
    HTML("<a href='https://github.com/dievalhu/ADoCS/blob/master/icmla14.csv'>ICMLA14</a>"),
    tags$hr()
  ),
  sidebarLayout(
    
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),

      fluidRow(
        column(3, checkboxInput('header', 'Header', TRUE)),
        
        column(3,
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',')
        ),
      column(6,
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
      )
      ),
      
 
      
        sliderInput("title", "Title:",  
                    min = 0, max = 1, value = 0.33),
      fluidRow(
        column(width = 6,checkboxInput('tfidf_t', 'TF-IDF', TRUE)),
        column(width = 6,selectInput('metric_t', 'Metric', selected="jaccard",c("cosine","jaccard","euclidean","manhattan"))))
      ,
        sliderInput("keywords", "Keywords:",  
                  min = 0, max = 1, value = 0.33),
      fluidRow(
        column(width = 6,checkboxInput('tfidf_k', 'TF-IDF', TRUE)),
        column(width = 6,selectInput('metric_k', 'Metric' , selected="jaccard", c("cosine","jaccard","euclidean","manhattan"))))
      ,
      sliderInput("abstract", "Abstract:",  
                  min = 0, max = 1, value = 0.33),
      fluidRow(
        column(width = 6,checkboxInput('tfidf_a', 'TF-IDF', TRUE)),
        column(width = 6,selectInput('metric_a', 'Metric', c("cosine","jaccard","euclidean","manhattan"))))
      ,
      
      tags$hr()
      #selectInput(
      #  "plotType", "Plot Type",
      #  c("Case A","Case B")
      #)
    ),

    
    mainPanel(
     conditionalPanel(
        condition = "output.warnstat == 'Done'",
      tabsetPanel(type = "tabs",id="tabs", 
                  
                #  tabPanel("Summary",    verbatimTextOutput("summary")), 
                  tabPanel("Papers", verbatimTextOutput("summary"),checkboxInput('showabstractr', 'Showabstract', FALSE),tableOutput("contents")),
                  tabPanel("Dendrogram", plotOutput("plot")),   
                #  tabPanel("Dendrogram2", plotOutput("plot2")),
                tabPanel("MDS", plotOutput("plotmds")),
                  tabPanel("Wordmap", plotOutput("wordmap")),
                  tabPanel("Schedule",    
                           verbatimTextOutput("summary1"),
                           numericInput("ses", "Number of Sessions:", 10, min = 1, max = 100),
                           textInput("dist", "Size of Sessions", width='200%',value="0"),
                           actionButton("goeq", "Equal Size"),
                           actionButton("gocompute", "Compute"),
                           actionButton("savecsv", "Save csv"),
                           #tableOutput("distpapers")
                           uiOutput("distpapers")
                                  )
                  
                  )
     )
    )
    )
  )
