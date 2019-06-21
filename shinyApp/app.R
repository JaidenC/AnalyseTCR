library(shiny)


# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----

  )
)

# Define server logic to read selected file ----

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
