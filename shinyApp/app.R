library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinyApp",
      tabPanel("Navbar 1",

          # Sidebar layout with input and output definitions ----
          sidebarLayout(

            # Sidebar panel for inputs ----
            sidebarPanel(

              # Input: Select a file ----
              fileInput("file1", "Choose CSV File",
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
                           selected = ","),

              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),

              # Horizontal line ----
              tags$hr(),

              # Input: Select number of rows to display ----
              radioButtons("disp", "Display",
                           choices = c(Head = "head",
                                       All = "all"),
                           selected = "head"),

              tags$hr(),
              actionButton("action", "Upload", class = "btn-primary")
            
            ),

            # Main panel for displaying outputs ----
            mainPanel(

              # Output: Data file ----
              tableOutput("contents")
            )
          )
        ),
      
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  options(shiny.maxRequestSize=30*1024^2) 
  
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

}

# Create Shiny app ----
shinyApp(ui, server)