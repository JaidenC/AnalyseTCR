library(shiny)
library(shinyFiles)
library(DT)
library(tcR)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
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
                 tableOutput("contents")
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

                 tags$hr(),
                 actionButton("action1", "Create CSV", class = "btn-primary")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                 tableOutput("select_table")
                 
               )
             )   
    ),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 

  dataUpload = reactiveValues(
    receptor_folder = NULL
  )
  
  output$contents <- renderTable({
    
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
  
  output$select_table <- renderTable({
    
    dframe <- read.csv(input$select_var)
    
    return(dframe)
    
  })

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
    print(dataUpload$receptor_folder)
    dir.create(file.path(dataUpload$receptor_folder, "newFile"), recursive = FALSE)

    #emptyData1 = data.frame(matrix(ncol=0,nrow=0))
    #emptyData2 = data.frame(matrix(ncol=0,nrow=0))
    #samples <- list(emptyData1, emptyData2)
   
    #for (i in 1:length(input$select_var)) {
      #parsedFile <- parse.file(input$select_var[i], 'mixcr')
      #samples[[i]] <- parsedFile
      #append(samples, parsedFile)
      #samples <- c(samples, parsedFile)
    #}
    
    #directory <- "/media/bhug/Emporium/bhug/Shiny/TestCase"
    #samples <- parse.folder(directory, 'mixcr')
    #imm.shared <- shared.repertoire(.data = samples, .type = 'n0rc', .min.ppl = 1, .verbose = F)
    #fileI = paste(directory, "/immShared_Buga_CD4.csv", sep="")
    #write.csv(imm.shared, file = fileI)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)