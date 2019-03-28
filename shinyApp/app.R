library(shiny)
library(shinyFiles)
library(DT)
library(tcR)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  navbarPage(
   
    "AnalyseTCR",
    
    tabPanel("Upload",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose Mixcr File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = "\t"),
                 
                 # Input: Select quotes ----
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = ""),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                 DT::dataTableOutput("contents")
               )
             )
    ),
    
    tabPanel("Shared Clonotype Table",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Select variables to display ----
                 uiOutput("checkbox"), 
               
                 tags$hr(),
                 div(HTML("<b>Choose Output Directory:</b>"), style = "margin-bottom: 5px;"),
                 shinyDirButton('resultsLocation', 'Browse...', title = 'Select a directory'),
                 br(),
                 htmlOutput('receptorDirectory'),

                 #creates field where user can choose what to name their CSV file
                 tags$hr(),
                 textInput("csvtext", label = "Name CSV", value = ".csv"),

                 tags$hr(),
                 actionButton("action1", "Create CSV", class = "btn-primary")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                   DT::dataTableOutput("select_table")
                 
               )
             )   
    ),
    
    tabPanel("Cloneset Summary", 

        "to be added"
    
    ),

    tabPanel("Repseq Summary", 

        "to be added"
    
    ),

    tabPanel("Top Proportions", 

        "to be added"
    
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 

  dataUpload = reactiveValues(
    receptor_folder = NULL
  )
  
  output$contents = DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # Dynamically generate UI input when data is uploaded ----
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var", 
                       label = "Input Files:", 
                       choiceNames = input$file1$name,
                       choiceValues = input$file1$datapath)
  })

  #allows user to choose where to store their new csv file
  volumes <- getVolumes()
  shinyDirChoose(input, 'resultsLocation', roots=volumes, session=session)
  
  observeEvent(input$resultsLocation, {
    dataUpload$receptor_folder = toString(parseDirPath(volumes, input$resultsLocation))
  })
  
  output$receptorDirectory <- renderUI({
    if (dataUpload$receptor_folder != '' && ! is.null(dataUpload$receptor_folder)) {
      helpText(HTML(paste0("<b>Selected directory:</b> ", dataUpload$receptor_folder)))
    }
  })
  
  observeEvent(input$action1, {
    dir.create(file.path(dataUpload$receptor_folder, "tempFile"), recursive = FALSE)

    #change working directory to the tempFile made
    changeDir <- dataUpload$receptor_folder
    newDir = paste(changeDir, "/tempFile", sep="")
    makeNewDir <- newDir
    setwd(makeNewDir)

    print(input$file1$name)
    print(input$select_var)

    for (i in input$select_var) {
      string <- sub(".*/", "", i)
      finalString <- sub(".txt", "", string)
      index <- as.numeric(finalString) + 1
      copyFile <- file.rename(i, input$file1$name[index])
      file.copy(from = input$copyFile, 
                to = getwd(), 
                overwrite = TRUE, 
                recursive = FALSE, 
                copy.mode = TRUE)
    }
    
    directory <- getwd()

    #runs tcR script to generate the csv file
    samples <- parse.folder(directory, 'mixcr')
    imm.shared <- shared.repertoire(.data = samples, .type = 'n0rc', .min.ppl = 1, .verbose = F)
    fileI = input$csvtext
    write.csv(imm.shared, file = fileI)

    file.copy(from = input$csvtext, 
              to = dataUpload$receptor_folder, 
              overwrite = TRUE, 
              recursive = FALSE, 
              copy.mode = TRUE)

    setwd(changeDir)

    unlink(newDir, recursive = TRUE)

    newEnd <- paste("/", input$csvtext, sep="")

    csvFile <- paste(changeDir, newEnd, sep="")

    output$select_table = DT::renderDataTable({
      dframe <- read.csv(csvFile)
    })

  })
  
}

# Create Shiny app ----
shinyApp(ui, server)