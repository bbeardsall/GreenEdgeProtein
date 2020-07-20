# Shiny app for GreenEdgeProtein
library(tidyverse)
library(stats)
library(shinydashboard)

##### Change to shiny directory before deploying!! #####

choiceVariables <- c("Sp", "Inhibitor", "CultureId", "BlotId", "None")

data <- readRDS("data/GreenEdge_TargetProteinReport_.Rds") %>%
  filter(!is.na("Stress_h"))

# User interface ----
ui <- dashboardPage(
  
  dashboardHeader(title = "GreenEdgeProtein"),
  
  dashboardSidebar(
    width = 500,
    fluidRow(
      box(
        background = "black",
        selectInput(
          "xaxis",
          label = "X axis",
          choices = c("Growth_uE", "Stress_h"),
          selected = "Stress_h"
        )
      ),
      box(
        background = "black",
        selectInput(
          "yaxis",
          label = "Y axis",
          choices = c("fmol_ugTotalProtein", "fmol_mL", "fmol_ugChla"),
          selected = "fmol_ugTotalProtein"
        )
      )
    ),
    
    fluidRow(
      box(
        background = "black",
        sliderInput(
          "size",
          label = "Size",
          min = 0, max = 5, value = 0.5,
          step = 0.5
        )
      ),
      box(
        background = "black",
        selectInput(
          "target",
          label = "Target",
          choices = unique(data$Target),
          selected = "PsbA"
        )
      ),
      box(
        background = "black",
        selectInput(
          "color",
          label = "Choose color",
          choices = choiceVariables,
          selected = "None"
        )
      ),
      box(
        background = "black",
        selectInput(
          "facetWrap",
          label = "Choose facet wrap",
          choices = choiceVariables,
          selected = "None"
        )
      ),
      box(
        background = "black",
        selectInput(
          "shape",
          label = "Choose shape",
          choices = choiceVariables,
          selected = "None"
        )
      ),
      box(
        background = "black",
        selectInput(
          "facetX",
          label = "Choose facet row",
          choices = choiceVariables,
          selected = "None"
        )
      ),
      box(
        background = "black",
        selectInput(
          "facetY",
          label = "Choose facet column",
          choices = choiceVariables,
          selected = "None"
        )
      )
    ),
    
    fluidRow(
      box(
        background = "black",
        checkboxGroupInput(
          "sheet",
          "ChlA Sheet Name",
          choices = setNames(as.list(unique(
            data$TotalChlaSheetName
          )), unique(data$TotalChlaSheetName)),
          selected = unique(data$TotalChlaSheetName)
        )
      ),
      box(
        background = "black",
        checkboxGroupInput(
          "species",
          "Species",
          choices = setNames(as.list(unique(data$Sp)), unique(data$Sp)),
          selected = unique(data$Sp)
        )
      )
    )
  ),
  
  dashboardBody(plotOutput("speciesPlot"),
                height = "5000px")
)

# Server logic ----
server <- function(input, output) {
  finalData <- reactive({
    data %>%
      filter(Target == input$target,
             Sp %in% input$species,
             TotalChlaSheetName %in% input$sheet)
  })
  
  output$speciesPlot <- renderPlot({
    xaxisSym <- sym(input$xaxis)
    yaxisSym <- sym(input$yaxis)
    colorSym <- sym(input$color)
    shapeSym <- sym(input$shape)
    facetWrapSym <- sym(input$facetWrap)
    facetXSym <- sym(input$facetX)
    facetYSym <- sym(input$facetY)
    shapeSym <- sym(input$shape)
    #sizeSym <- sym(as.character(input$size))
    
    ggplot(finalData()) +
      {
        if (input$color != "None" & input$shape != "None") {
          geom_point(aes(
            x = !!xaxisSym,
            y = !!yaxisSym,
            color = !!colorSym,
            shape = !!shapeSym
          ),
          size = input$size)
        }
        else if (input$color != "None") {
          geom_point(aes(
            x = !!xaxisSym,
            y = !!yaxisSym,
            color = !!colorSym
          ))
        }
        else if (input$shape != "None") {
          geom_point(aes(
            x = !!xaxisSym,
            y = !!yaxisSym,
            shape = !!shapeSym
          ),
          size = input$size)
        }
        else{
          geom_point(aes(x = !!xaxisSym, y = !!yaxisSym),
                     size = input$size)
        }
      } +
      {
        if (input$facetWrap != "None") {
          facet_wrap(vars(!!facetWrapSym))
        }
        else {
          if (input$facetX != "None" & input$facetY != "None") {
            facet_grid(vars(!!facetXSym), vars(!!facetYSym))
          }
          else if (input$facetX != "None") {
            facet_grid(rows = vars(!!facetXSym))
          }
          else if (input$facetY != "None") {
            facet_grid(cols = vars(!!facetYSym))
          }
        }
      } +
      theme_bw() +
      ggtitle(paste("Protein Target:", input$target))
  })
}

# Run app ----
shinyApp(ui, server)