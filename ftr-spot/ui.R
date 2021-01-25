library(shiny)
library(tidyverse)

ftr_nodes <- read_csv("data/nodes.csv")
ftr_years <- 2016:2020

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("FTRs & Spots"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput("source_node", label = "Source", choices = ftr_nodes, selected = "BEN"),
          selectInput("sink_node",   label = "Sink",   choices = ftr_nodes, selected = "OTA"),
          selectInput("ftr_year",    label = "Year",   choices = ftr_years, selected = "2018")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("FTR-Spot-Hedge",
                     plotOutput("plot_combination")
            ),
            tabPanel("FTR-Spot",
                     plotOutput("plot_scaled"),
                     plotOutput("plot_ftrs")
            ),
            tabPanel("Tables",
                     DT::dataTableOutput("table_ftrs"),
                     DT::dataTableOutput("table_final"),
                     DT::dataTableOutput("table_asx")
            )
          )
        )
    )
))
