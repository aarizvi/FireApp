library(shiny)
library(ggmap)
library(tidyverse)

topMA.new <- readRDS("topMA.new.rds")
merged.all <- readRDS("merged.all.rds")
mmap <- readRDS("mmap.rds")

source("fireIncidentPlot.R")
source("natBarPlot.R")

year <- unique(topMA.new$year)
category <- c("AREA_ORIG", "CAUSE_IGN", "HEAT_SOURC")

ui <- shinyUI(pageWithSidebar(
        headerPanel("Residential Fire Incidences"),
        sidebarPanel(
                conditionalPanel(condition="input.conditionedPanels==1",
                                 selectInput(inputId="years",
                                             label="Select Year:",
                                             choices=year),
                                 selectInput(inputId="categories",
                                             label="Select Category:",
                                             choices=category),
                                 actionButton("barbutton", "Generate Plot")
                                
                ),
                conditionalPanel(condition="input.conditionedPanels==2",
                                 selectInput(inputId="years2",
                                        label="Select Year:",
                                        choices=year),
                                 selectInput(inputId="categories2",
                                        label="Select Category:",
                                        choices=category),
                     actionButton("plotbutton", "Show Plot")
                )
        ),
        mainPanel(
                tabsetPanel(
                        id = "conditionedPanels",
                        tabPanel("National Summary Plots",
                                fluidRow(width=12,
                                         h3("Causes of Fire Incidences in the United States from 2012-2015"),
                                         h6("CAUSE_IGN = Cause of Ignition"),
                                         h6("HEAT_SOURC = Source from which the fire started"),
                                         h6("AREA_ORIG = Area fire originated"),
                                         plotOutput("bar_plot", width="100%")
                                         ),
                                value=1),
                        tabPanel("Boston, MA Fire Incidences",
                                 fluidRow(width=12,
                                          h3("Map of fire incidents from the fire department with most incidences in Massachusetts. Each dot represents an incident. Different categories (cause of ignition, area of origin, and heat source) can be selected to descripitively visualize the incidents in Massachusetts. "),
                                          plotOutput("ui_plot", width="100%")
                                          ),
                                 value=2)
                )
        )
        
)
)

server <- shinyServer(function(input, output){
        output$bar_plot <- renderPlot({
                input$barbutton
                isolate(natBarPlot(merged.all, input$years, input$categories))
        }, height=600, width=800)
        
        output$ui_plot <- renderPlot({
                input$plotbutton
                isolate(fireIncidentPlot(topMA.new, input$years2, input$categories2, mmap))
        }, height=600, width=800)
})


shinyApp(ui = ui, server = server)