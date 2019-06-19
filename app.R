library(shiny)
library(shinyFiles)
library(DT)
library(tcR)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(immunarch)

# Define UI for data upload app ----
ui <- fluidPage(

  useShinyjs(),
  
  # App title ----
  navbarPage(
   
    "COEVE",
    
    tabPanel("Upload",
             
         fluidRow(        
          column(8, align="center", offset = 2,
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
        )
      )  
    ),

    tabPanel("Choose Files",
       fluidRow(        
          column(8, align="center", offset = 2,         
 
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
          )
        )
      ),
    
    navbarMenu("TCR Package",
      
      tabPanel("Shared Clonotype Table",
        DT::dataTableOutput("select_table"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),
      
      tabPanel("Cloneset Summary",         
        DT::dataTableOutput("clonesetTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),

      tabPanel("Repseq Summary", 
        DT::dataTableOutput("repseqTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),

      tabPanel("Top Proportions", 
        fluidRow(
          column(3,
            selectInput("topChoose", "Choose what you want to display:", 
             c("Top Proportion Graph",
               "Clonal Space Homeostasis"))
          ),
          column(3, offset = 1,
            textInput("topText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('topDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("topGraph")
      ),

      tabPanel("Gene Usage", 
        fluidRow(
          column(3,
            selectInput("geneChoose", "Choose graph to display:", 
                 c("J Usage Graph",
                   "J Usage Column",
                   "V Usage Graph"))
          ),
          column(3, offset = 1,
            textInput("geneText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('geneDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("geneGraph")    
      ),

      tabPanel("Jensen-Shannon Divergence", 
        fluidRow(
          column(3,
            textInput("shannonText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 1,
            downloadButton('shannonDownload', 'Download Graph')
          )
        ),  
        hr(),
        plotOutput("shannonGraph")    
      ),

      tabPanel("Overlap quantification", 
        fluidRow(
          column(3,
            selectInput("overChoose", "Choose graph to display:", 
                 c("Heatmap",
                   "Top Cross"))
          ),
          column(3, offset = 1,
            textInput("overText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('overDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("overGraph")    
      )
    ),
    
    navbarMenu("Immunarch",
      
      tabPanel("Repertoire Overlap", 
        fluidRow(
          column(3,
            selectInput("immuheatChoose", "Choose graph to display:", 
                 c("Repertoire Overlap Heatmap",
                   "Repertoire Analysis"))
          ),
          column(3, offset = 1,
            textInput("immuheatText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('immuheatDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("immuheatGraph")    
      ),

        tabPanel("Gene Usage Analysis", 
        fluidRow(
          column(3,
            selectInput("immugeneChoose", "Choose graph to display:", 
                 c("Gene Usage Histogram",
                   "Gene Usage Boxplot",
                   "Gene Usage Tree",
                   "Gene Usage Jensen-Shannon",
                   "Gene Usage Correlation",
                   "Spectratyping"))
          ),
          column(3, offset = 1,
            textInput("immugeneText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('immugeneDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("immugeneGraph")    
      ),

        tabPanel("Diversity Estimation", 
        fluidRow(
          column(3,
            selectInput("immudivChoose", "Choose graph to display:", 
                 c("Chao1 Estimator",
                   "Hill Numbers",
                   "True Diversity",
                   "D50 Diversity Index",
                   "Rarefaction Analysis"))
          ),
          column(3, offset = 1,
            textInput("immudivText", label = "Name Graph", value = ".png")
          ),
          column(3, offset = 2,
            downloadButton('immudivDownload', 'Download Graph')
          )
        ),
        hr(),
        plotOutput("immudivGraph")    
      )
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

    immdata = repLoad(directory, .format = "mixcr")
    print(directory)

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

    #Cloneset summary tab
    output$clonesetTable <- DT::renderDataTable(
      datatable(cloneset.stats(samples))
    )

    #Repseq summary tab
    output$repseqTable <- DT::renderDataTable(
      datatable(repseq.stats(samples))
    )

    twb.space <- clonal.space.homeostasis(samples)

    #Top proportion Tab
     topInput <- reactive({
      switch(input$topChoose,
          "Top Proportion Graph" = vis.top.proportions(samples),
          "Clonal Space Homeostasis" = vis.clonal.space(twb.space)
          )
    })

      output$topGraph <- renderPlot({
        topInput()
      }, height = 1000)

      output$topDownload <- downloadHandler(
      filename = input$topText,
      content = function(file) {
        ggsave(input$topText, plot = topInput(), device = "png")
      }
    )
    
    #Gene Usage Tab
    imm1.vs <- tcR::geneUsage(samples[[1]], HUMAN_TRBV)

    geneInput <- reactive({
      switch(input$geneChoose,
          "J Usage Graph" = vis.gene.usage(samples, HUMAN_TRBJ, .main = 'twb J-usage dodge', .dodge = T),
          "J Usage Column" = vis.gene.usage(samples, HUMAN_TRBJ, .main = 'twb J-usage column', .dodge = F, .ncol = 2),
          "V Usage Graph" = vis.gene.usage(imm1.vs, NA, .main = 'twb[[1]] V-usage', .coord.flip = F)
          )
    })

    output$geneDownload <- downloadHandler(
      filename = input$geneText,
      content = function(file) {
        ggsave(input$geneText, plot = geneInput(), device = "png")
      }
    )

    output$geneGraph <- renderPlot({ 
      geneInput()
    }, height = 1000) 

    #Jensen-Shannon graph tab
    shannonInput <- reactive({
      imm.js <- js.div.seg(samples, HUMAN_TRBV, .verbose = F) 
      vis.radarlike(imm.js, .ncol = 2)
    })

    output$shannonGraph <- renderPlot({ 
      shannonInput()
    }, height = 1000)

    output$shannonDownload <- downloadHandler(
      filename = input$shannonText,
      content = function(file) {
        ggsave(input$shannonText, plot = shannonInput(), device = "png")
      }
    )

    twb.top <- top.cross(.data = samples, .n = seq(500, 10000, 500), .verbose = F, .norm = T)    
    
    overInput <- reactive({
      switch(input$overChoose,
          "Heatmap" = vis.heatmap(tcR::repOverlap(samples, 'exact', 'aa', .vgene = T, .verbose = F), .title = 'twb - (ave)-intersection', .labs = ''),
          "Top Cross" = top.cross.plot(twb.top)

          )
    })

    output$overDownload <- downloadHandler(
      filename = input$overText,
      content = function(file) {
        ggsave(input$overText, plot = overInput(), device = "png")
      }
    )

    output$overGraph <- renderPlot({ 
      overInput()
    }, height = 1000)

    imm_ov1 = immunarch::repOverlap(immdata, .method = "public", .verbose = F)

    immuheatInput <- reactive({
      switch(input$immuheatChoose,
          "Repertoire Overlap Heatmap" = vis(immunarch::repOverlap(immdata)),
          "Repertoire Analysis" = vis(repOverlapAnalysis(imm_ov1, "mds"))
          )
    })

    output$immuheatDownload <- downloadHandler(
      filename = input$immuheatText,
      content = function(file) {
        ggsave(input$immuheatText, plot = immuheatInput(), device = "png")
      }
    )

    output$immuheatGraph <- renderPlot({ 
      immuheatInput()
    }, height = 1000)

    imm_gu = immunarch::geneUsage(immdata, "hs.trbv", .norm = T, .ambig = "exc")
    imm_gu_js = geneUsageAnalysis(imm_gu, .method = "js", .verbose = F)
    imm_gu_cor = geneUsageAnalysis(imm_gu, .method = "cor", .verbose = F)
    p1 = vis(immunarch::spectratype(immdata[[1]], .quant = "id", .col = "aa", .gene = "v"))
    p2 = vis(immunarch::spectratype(immdata[[1]], .quant = "count", .col = "aa", .gene = "v"))
   
    immugeneInput <- reactive({
      switch(input$immugeneChoose,
          "Gene Usage Histogram" = vis(imm_gu, .plot = "hist", .grid = T),
          "Gene Usage Boxplot" = vis(imm_gu, .by = "Status", .meta = immdata$meta, .plot = "box"),
          "Gene Usage Tree" = vis(imm_gu, .plot = "tree"),
          "Gene Usage Jensen-Shannon" = vis(imm_gu_js, .title = "Gene usage JS-divergence", .leg.title = "JS", .text.size=1.5),
          "Gene Usage Correlation" = vis(imm_gu_cor, .title = "Gene usage correlation", .leg.title = "Cor", .text.size=1.5),
          "Spectratyping" = grid.arrange(p1, p2, ncol = 2)
          )
    })

    output$immugeneDownload <- downloadHandler(
      filename = input$immugeneText,
      content = function(file) {
        ggsave(input$immugeneText, plot = immugeneInput(), device = "png")
      }
    )

    output$immugeneGraph <- renderPlot({ 
      immugeneInput()
    }, height = 1000)

    # Chao1 diversity measure
    div_chao = repDiversity(immdata, "chao1")

    # Hill numbers
    div_hill = repDiversity(immdata, "hill")

    # D50
    div_d50 = repDiversity(immdata, "d50")

    # Ecological diversity measure
    div_div = repDiversity(immdata, "div")

    imm_raref = repDiversity(immdata, "raref", .verbose = F)

    immudivInput <- reactive({
      switch(input$immudivChoose,
          "Chao1 Estimator" = vis(div_chao),
          "Hill Numbers" = vis(div_hill, .by=c("Status", "Sex"), .meta=immdata$meta),
          "True Diversity" = vis(div_div),
          "D50 Diversity Index" = vis(div_d50),
          "Rarefaction Analysis" = grid.arrange(vis(imm_raref), vis(imm_raref, .by="Status", .meta=immdata$meta), ncol=2)
          )
    })

    output$immudivDownload <- downloadHandler(
      filename = input$immudivText,
      content = function(file) {
        ggsave(input$immudivText, plot = immudivInput(), device = "png")
      }
    )

    output$immudivGraph <- renderPlot({ 
      immudivInput()
    }, height = 1000)

  })
}

# Create Shiny app ----
shinyApp(ui, server)