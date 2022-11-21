#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Authors: Philippe Mongeon & Poppy Riddle
#Date: Nov 17, 2022
#CC-BY license
#


library(shiny)
library(shinycssloaders)
library(dplyr)
library(stringr)
library(readxl)
library(openalexR)
library(tidyr)
library(tibble)
library(writexl)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("BiblioNet"),
  h4(" - Keep your frields close...yeah, just keep your friends close."),
  
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
                              choices = list("Articles" = "article",
                                             "Authors" = "author",
                                             "Institutions" = "institution",
                                             "Countries" = "country",
                                             "Journal" = "journal",
                                             "Concepts" = "concept")),
                 radioButtons("network_type", "Network type:",
                              choices = list("Co-occurence/co-authorship" = "co-occurence",
                                             "Direct citations" = "direct_citation",
                                             "Co-citations" = "co-citations",
                                             "Bibliographic coupling" = "bibliographic_coupling"),
                              ),
                 
                 
                 # Download Button-------------------------------------------------------------
                 
                 #downloadButton("downloadData", "Compute and download")
                 
                 # Download Button-------------------------------------------------------------
    #             tags$hr(style = "border-top: 1.5px solid #000000;"),
    #             h4("Files to calculate and download"),
    #             br(),
                 
                 downloadButton("downloadNode", "Compute"),
    #             downloadButton("downloadEdges", "Compute edges")
                 
                 
    ),#close sidebarPanel
    
    # mainPanel--------------------------------------------------------------
    mainPanel()
  )
)

# Define server logic----------------------------------------------------------
#source files; https://stackoverflow.com/questions/68976268/r-shiny-upload-csv-calculate-values-in-table-and-then-download-results-as-a
server <- function(input, output) {
  
  #create and modify dataframe-------------------------------------------------------
  nodesData <- eventReactive(input$file1, {
    req(input$file1)


    if(input$entity == "author"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(temp_id, authors) %>%
        separate_rows(authors, sep=", ", convert=TRUE) %>%
        group_by(authors) %>%
        summarize(n_papers = n()) %>%
        mutate(authors = str_remove_all(authors, '"')) %>%
        rownames_to_column("id") %>%
        rename(label = authors) %>%
        filter(!is.na(label))
      if(input$network_type == "co-occurence") {
      edges = read_excel(input$file1$datapath, sheet = 1) %>%
        select(temp_id, authors) %>%
        separate_rows(authors, sep=", ", convert=TRUE) %>%
        mutate(authors=str_remove_all(authors,'"')) %>%
        inner_join(nodes, by=c("authors"="label")) %>%
        select(temp_id, id) %>%
        inner_join(read_excel(input$file1$datapath) %>%
                     select(temp_id, authors) %>%
                     separate_rows(authors, sep=", ", convert=TRUE) %>%
                     mutate(authors=str_remove_all(authors,'"')) %>%
                     inner_join(nodes, by=c("authors"="label")) %>%
                     select(temp_id, id),
                   by="temp_id") %>%
        filter(id.x < id.y) %>%
        group_by(id.x, id.y) %>%
        summarize(weight = n()) %>%
        mutate(type = "undirected") %>%
        rename(source = id.x, target = id.y)
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, authors, cited_ids) %>%
          separate_rows(authors, sep=", ", convert=TRUE) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          mutate(authors=str_remove_all(authors,'"')) %>%
          inner_join(nodes, by=c("authors"="label")) %>%
          select(openalex_id, id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, authors) %>%
                       separate_rows(authors, sep=", ", convert=TRUE) %>%
                       mutate(authors=str_remove_all(authors,'"')) %>%
                       inner_join(nodes, by=c("authors"="label")) %>%
                       select(openalex_id, id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, authors, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, authors, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          separate_rows(authors.x, sep=", ", convert=TRUE) %>%
          mutate(authors.x = str_remove_all(authors.x, '"')) %>% 
          separate_rows(authors.y, sep=", ", convert=TRUE) %>%
          mutate(authors.y = str_remove_all(authors.y, '"')) %>%
          inner_join(select(nodes, id, label), by=c("authors.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("authors.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
        }
        if(input$network_type == "bibliographic_coupling"){
          edges = read_excel(input$file1$datapath) %>%
            select(openalex_id, authors, cited_ids) %>%
            separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
            inner_join(read_excel(input$file1$datapath) %>%
                         select(openalex_id, authors, cited_ids) %>%
                         separate_rows(cited_ids, sep=", ", convert=TRUE),
                       by="cited_ids") %>% 
            separate_rows(authors.x, sep=", ", convert=TRUE) %>%
            mutate(authors.x = str_remove_all(authors.x, '"')) %>% 
            separate_rows(authors.y, sep=", ", convert=TRUE) %>%
            mutate(authors.y = str_remove_all(authors.y, '"')) %>%
            inner_join(select(nodes, id, label), by=c("authors.x" = "label")) %>% 
            inner_join(select(nodes, id, label), by=c("authors.y" = "label")) %>% 
            select(source = id.x, target = id.y) %>% 
            filter(source < target) %>% 
            group_by(source, target) %>%
            summarize(weight = n()) %>% 
            mutate(type = "undirected")
          }
    }

    if(input$entity == "institution"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(temp_id, institutions) %>%
        separate_rows(institutions, sep=", ", convert=TRUE) %>%
        group_by(institutions) %>%
        summarize(n_papers = n()) %>%
        mutate(institutions = str_remove_all(institutions, "'")) %>%
        rownames_to_column("id") %>%
        rename(label = institutions) %>%
        filter(!is.na(label))
      
      if(input$network_type == "co-occurence") {
        edges = read_excel(input$file1$datapath) %>%
          select(temp_id, institutions) %>%
          separate_rows(institutions, sep=", ", convert=TRUE) %>%
          mutate(institutions=str_remove_all(institutions,"'")) %>%
          inner_join(nodes, by=c("institutions"="label")) %>%
          select(temp_id, id) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(temp_id, institutions) %>%
                       separate_rows(institutions, sep=", ", convert=TRUE) %>%
                       mutate(institutions=str_remove_all(institutions,"'")) %>%
                       inner_join(nodes, by=c("institutions"="label")) %>%
                       select(temp_id, id),
                     by="temp_id") %>%
          filter(id.x < id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "undirected") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, institutions, cited_ids) %>%
          separate_rows(institutions, sep=", ", convert=TRUE) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          mutate(institutions=str_remove_all(institutions,"'")) %>%
          inner_join(nodes, by=c("institutions"="label")) %>%
          select(openalex_id, id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, institutions) %>%
                       separate_rows(institutions, sep=", ", convert=TRUE) %>%
                       mutate(institutions=str_remove_all(institutions,"'")) %>%
                       inner_join(nodes, by=c("institutions"="label")) %>%
                       select(openalex_id, id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, institutions, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, institutions, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          separate_rows(institutions.x, sep=", ", convert=TRUE) %>%
          mutate(institutions.x = str_remove_all(institutions.x, "'")) %>% 
          separate_rows(institutions.y, sep=", ", convert=TRUE) %>%
          mutate(institutions.y = str_remove_all(institutions.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("institutions.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("institutions.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
      if(input$network_type == "bibliographic_coupling"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, institutions, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, institutions, cited_ids) %>%
                       separate_rows(cited_ids, sep=", ", convert=TRUE),
                     by="cited_ids") %>% 
          separate_rows(institutions.x, sep=", ", convert=TRUE) %>%
          mutate(institutions.x = str_remove_all(institutions.x, "'")) %>% 
          separate_rows(institutions.y, sep=", ", convert=TRUE) %>%
          mutate(institutions.y = str_remove_all(institutions.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("institutions.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("institutions.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
    }
    
    if(input$entity == "country"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(temp_id, countries) %>%
        separate_rows(countries, sep=", ", convert=TRUE) %>%
        group_by(countries) %>%
        summarize(n_papers = n()) %>%
        mutate(countries = str_remove_all(countries, "'")) %>%
        rownames_to_column("id") %>%
        rename(label = countries) %>%
        filter(!is.na(label))
      
      if(input$network_type == "co-occurence") {
        edges = read_excel(input$file1$datapath) %>%
          select(temp_id, countries) %>%
          separate_rows(countries, sep=", ", convert=TRUE) %>%
          mutate(countries=str_remove_all(countries,"'")) %>%
          inner_join(nodes, by=c("countries"="label")) %>%
          select(temp_id, id) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(temp_id, countries) %>%
                       separate_rows(countries, sep=", ", convert=TRUE) %>%
                       mutate(countries=str_remove_all(countries,"'")) %>%
                       inner_join(nodes, by=c("countries"="label")) %>%
                       select(temp_id, id),
                     by="temp_id") %>%
          filter(id.x < id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "undirected") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, countries, cited_ids) %>%
          separate_rows(countries, sep=", ", convert=TRUE) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          mutate(countries=str_remove_all(countries,"'")) %>%
          inner_join(nodes, by=c("countries"="label")) %>%
          select(openalex_id, id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, countries) %>%
                       separate_rows(countries, sep=", ", convert=TRUE) %>%
                       mutate(countries=str_remove_all(countries,"'")) %>%
                       inner_join(nodes, by=c("countries"="label")) %>%
                       select(openalex_id, id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, countries, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, countries, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          separate_rows(countries.x, sep=", ", convert=TRUE) %>%
          mutate(countries.x = str_remove_all(countries.x, "'")) %>% 
          separate_rows(countries.y, sep=", ", convert=TRUE) %>%
          mutate(countries.y = str_remove_all(countries.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("countries.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("countries.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
      if(input$network_type == "bibliographic_coupling"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, countries, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, countries, cited_ids) %>%
                       separate_rows(cited_ids, sep=", ", convert=TRUE),
                     by="cited_ids") %>% 
          separate_rows(countries.x, sep=", ", convert=TRUE) %>%
          mutate(countries.x = str_remove_all(countries.x, "'")) %>% 
          separate_rows(countries.y, sep=", ", convert=TRUE) %>%
          mutate(countries.y = str_remove_all(countries.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("countries.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("countries.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
    }
    
# Concepts ----
    if(input$entity == "concept"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(temp_id, wikidata_concepts) %>%
        separate_rows(wikidata_concepts, sep=", ", convert=TRUE) %>%
        group_by(wikidata_concepts) %>%
        summarize(n_papers = n()) %>%
        mutate(wikidata_concepts = str_remove_all(wikidata_concepts, "'")) %>%
        rownames_to_column("id") %>%
        rename(label = wikidata_concepts) %>%
        filter(!is.na(label))
      
      if(input$network_type == "co-occurence") {
        edges = read_excel(input$file1$datapath) %>%
          select(temp_id, wikidata_concepts) %>%
          separate_rows(wikidata_concepts, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts=str_remove_all(wikidata_concepts,"'")) %>%
          inner_join(nodes, by=c("wikidata_concepts"="label")) %>%
          select(temp_id, id) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(temp_id, wikidata_concepts) %>%
                       separate_rows(wikidata_concepts, sep=", ", convert=TRUE) %>%
                       mutate(wikidata_concepts=str_remove_all(wikidata_concepts,"'")) %>%
                       inner_join(nodes, by=c("wikidata_concepts"="label")) %>%
                       select(temp_id, id),
                     by="temp_id") %>%
          filter(id.x < id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "undirected") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, wikidata_concepts, cited_ids) %>%
          separate_rows(wikidata_concepts, sep=", ", convert=TRUE) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts=str_remove_all(wikidata_concepts,"'")) %>%
          inner_join(nodes, by=c("wikidata_concepts"="label")) %>%
          select(openalex_id, id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, wikidata_concepts) %>%
                       separate_rows(wikidata_concepts, sep=", ", convert=TRUE) %>%
                       mutate(wikidata_concepts=str_remove_all(wikidata_concepts,"'")) %>%
                       inner_join(nodes, by=c("wikidata_concepts"="label")) %>%
                       select(openalex_id, id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, wikidata_concepts, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, wikidata_concepts, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          separate_rows(wikidata_concepts.x, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts.x = str_remove_all(wikidata_concepts.x, "'")) %>% 
          separate_rows(wikidata_concepts.y, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts.y = str_remove_all(wikidata_concepts.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("wikidata_concepts.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("wikidata_concepts.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
      if(input$network_type == "bibliographic_coupling"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, wikidata_concepts, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, wikidata_concepts, cited_ids) %>%
                       separate_rows(cited_ids, sep=", ", convert=TRUE),
                     by="cited_ids") %>% 
          separate_rows(wikidata_concepts.x, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts.x = str_remove_all(wikidata_concepts.x, "'")) %>% 
          separate_rows(wikidata_concepts.y, sep=", ", convert=TRUE) %>%
          mutate(wikidata_concepts.y = str_remove_all(wikidata_concepts.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("wikidata_concepts.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("wikidata_concepts.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
    }
    
    # Articles ----
    if(input$entity == "article"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(openalex_id, title) %>%
        separate_rows(openalex_id, sep=", ", convert=TRUE) %>%
        group_by(openalex_id, title) %>%
        rename(id = openalex_id, label = title) %>%
        filter(!is.na(id))
      
      if(input$network_type == "co-occurence") {
        edges = tibble()
          
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          select(id = openalex_id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id) %>% 
                       mutate(id = openalex_id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(id = openalex_id, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(id = openalex_id, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
      if(input$network_type == "bibliographic_coupling"){
        edges = read_excel(input$file1$datapath) %>%
          select(id = openalex_id, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(id = openalex_id, cited_ids) %>%
                       separate_rows(cited_ids, sep=", ", convert=TRUE),
                     by="cited_ids") %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
    }
    
    
    # Journals ----
    if(input$entity == "journal"){
      nodes<-read_excel(input$file1$datapath) %>% 
        select(temp_id, source) %>%
        separate_rows(source, sep=", ", convert=TRUE) %>%
        group_by(source) %>%
        summarize(n_papers = n()) %>%
        mutate(source = str_remove_all(source, "'")) %>%
        rownames_to_column("id") %>%
        rename(label = source) %>%
        filter(!is.na(label))
      
      if(input$network_type == "co-occurence") {
        edges = read_excel(input$file1$datapath) %>%
          select(temp_id, source) %>%
          separate_rows(source, sep=", ", convert=TRUE) %>%
          mutate(source=str_remove_all(source,"'")) %>%
          inner_join(nodes, by=c("source"="label")) %>%
          select(temp_id, id) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(temp_id, source) %>%
                       separate_rows(source, sep=", ", convert=TRUE) %>%
                       mutate(source=str_remove_all(source,"'")) %>%
                       inner_join(nodes, by=c("source"="label")) %>%
                       select(temp_id, id),
                     by="temp_id") %>%
          filter(id.x < id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "undirected") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "direct_citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, source, cited_ids) %>%
          separate_rows(source, sep=", ", convert=TRUE) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          mutate(source=str_remove_all(source,"'")) %>%
          inner_join(nodes, by=c("source"="label")) %>%
          select(openalex_id, id, cited_ids) %>% 
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, source) %>%
                       separate_rows(source, sep=", ", convert=TRUE) %>%
                       mutate(source=str_remove_all(source,"'")) %>%
                       inner_join(nodes, by=c("source"="label")) %>%
                       select(openalex_id, id),
                     by=c("cited_ids" = "openalex_id")) %>%
          filter(id.x != id.y) %>%
          group_by(id.x, id.y) %>%
          summarize(weight = n()) %>%
          mutate(type = "directed") %>%
          rename(source = id.x, target = id.y)
      }
      if(input$network_type == "co-citation"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, source, citing_ids) %>%
          separate_rows(citing_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, source, citing_ids) %>%
                       separate_rows(citing_ids, sep=", ", convert=TRUE),
                     by="citing_ids") %>% 
          separate_rows(source.x, sep=", ", convert=TRUE) %>%
          mutate(source.x = str_remove_all(source.x, "'")) %>% 
          separate_rows(source.y, sep=", ", convert=TRUE) %>%
          mutate(source.y = str_remove_all(source.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("source.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("source.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
      if(input$network_type == "bibliographic_coupling"){
        edges = read_excel(input$file1$datapath) %>%
          select(openalex_id, source, cited_ids) %>%
          separate_rows(cited_ids, sep=", ", convert=TRUE) %>%
          inner_join(read_excel(input$file1$datapath) %>%
                       select(openalex_id, source, cited_ids) %>%
                       separate_rows(cited_ids, sep=", ", convert=TRUE),
                     by="cited_ids") %>% 
          separate_rows(source.x, sep=", ", convert=TRUE) %>%
          mutate(source.x = str_remove_all(source.x, "'")) %>% 
          separate_rows(source.y, sep=", ", convert=TRUE) %>%
          mutate(source.y = str_remove_all(source.y, "'")) %>%
          inner_join(select(nodes, id, label), by=c("source.x" = "label")) %>% 
          inner_join(select(nodes, id, label), by=c("source.y" = "label")) %>% 
          select(source = id.x, target = id.y) %>% 
          filter(source < target) %>% 
          group_by(source, target) %>%
          summarize(weight = n()) %>% 
          mutate(type = "undirected")
      }
    }
    
        export<-list("nodes" = nodes, "edges" = edges)
    

  }  
  )#close eventReactive
  
 edgesData <- eventReactive(input$file1, {
    req(input$file1)
    template <- read_excel(input$file1$datapath)
    

})#cose eventReacgtive
  

  #downloadHandler----------------------------------------------------------
output$downloadNode <- downloadHandler(
  #change filename to include entity, network, files selected
  filename = function() {str_c("network_files_", input$entity, "_",input$network_type,"_", Sys.Date(), ".xlsx")},
  content = function(file){
      writexl::write_xlsx(nodesData(), file)}
  )#close downloadHandler
  
#  output$downloadEdges <- downloadHandler(
#    #change filename to include entity, network, files selected
#    filename = function() {paste("edges_", input$entity, "_",input$network_type,"_", Sys.Date(), ".csv")},
#    content = function(file){
#      write.csv(edgesData(), file, row.names = FALSE)}
#  )#close downloadHandler

}#close server
# Run the application ----------------------------------------------------------

shinyApp(ui = ui, server = server)

#****************************************************************
