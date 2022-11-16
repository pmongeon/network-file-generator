#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shiny)
library(shinycssloaders)
library(dplyr)
library(stringr)
library(readxl)
library(openalexR)
library(tidyr)
library(tibble)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("BiblioBox"),
  h4(" - Normalization is not being normal"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 6, fluid = TRUE,
                 
                 # Input: Selector for choosing dataset ------------------------------
                 h4("Step1: Upload your template file"),
                 fileInput("file1", "Choose your Excel Template file",
                           multiple = TRUE,
                           accept = c(".csv",
                                      ".xlsx")),
         
                  radioButtons("entity", "Entity:",
                                       choiceNames = list(
                                         "Articles",
                                         "Authors",
                                         "Institutions",
                                         "Countries",
                                         "Journals",
                                         "Concepts"
                                       ),
                                       choiceValues = list(
                                         "art", "aut", "ins","cou","jou","con"
                                       )),
                          radioButtons("network_type", "Network type:",
                                       choiceNames = list(
                                         "Co-occurence/co-authorship",
                                         "Direct citations",
                                         "Co-citations",
                                         "Bibliographic coupling"
                                       ),
                                       choiceValues = list(
                                         "co", "dc", "cc","bc"
                                       )),

                 
                 # Download Button-------------------------------------------------------------

                  downloadButton("downloadData", "Compute and download")
                 
    ),#close sidebarPanel
    
    # mainPanel--------------------------------------------------------------
    mainPanel()
  )
)

# Define server logic----------------------------------------------------------
#source files; https://stackoverflow.com/questions/68976268/r-shiny-upload-csv-calculate-values-in-table-and-then-download-results-as-a
server <- function(input, output) {

  #create and modify dataframe-------------------------------------------------------
  rawData <- eventReactive(input$file1, {
    req(input$file1)
    template <- read_excel(input$file1$datapath, sheet = 2) %>% 
      rownames_to_column("temp_id")
    
    nodes <- template %>% 
      select(temp_id, authors) %>% 
      separate_rows(auhtors, ", ") %>% 
      group_by(authors) %>% 
      summarize(n_papers = n())
    
    edges <- template %>% 
      select(temp_id, authors) %>% 
      separate_rows(auhtors, ", ")
    
    edges<-inner_join(edges, by="temp_id") %>% 
      filter(authors.x < authors.y) %>%
      group_by(authors.x, authors.y) %>%
      summarize(weight = n()) %>% 
      mutate(type = "undirected") %>% 
      rename(source = authors.x, target = authors.y)

    
    nodes 
    
  }  
  )#close eventReactive
  
  rawData
  #downloadHandler----------------------------------------------------------
  output$downloadData <- downloadHandler(
    
    filename = function() {paste("modified template_",  Sys.Date(),".xlsx")},
    content = function(file){
         writexl::write_xlsx(rawData(), file)
     
    }#close function
  )#close downloadHandler
  
}#close server
# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)