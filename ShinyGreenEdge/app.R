# Shiny app for GreenEdgeProtein
library(tidyverse)
library(stats)
library(shinydashboard)

choiceVariables <- c("Sp", "Inhibitor", "CultureId", "BlotId", "AllNegativeProt", "Flag", "None")
axisVariables <- 
data <- readRDS("data/GreenEdge_TargetProteinReport_.Rds") %>%
  filter(!is.na("Stress_h"))


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
          choices = c("Growth_uE", "Stress_h", "TotalProteinExtract_ug", "TotalChlaExtract_ug", unique(data$Target)),
          selected = "Stress_h"
        )
      ),
      box(
        background = "black",
        selectInput(
          "yaxis",
          label = "Y axis",
          choices = c("fmol_ugTotalProtein", "fmol_mL", "fmol_ugChla","TotalProteinExtract_ug", "TotalChlaExtract_ug", unique(data$Target)),
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
          min = 0, max = 5, value = 3.5,
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
      )
    ),
    fluidRow(
      box(
        background = "black",
        radioButtons("plotType", label = "Plot Type",
                     choices = list("Single Target" = 1, "Compare Targets" = 2), 
                     selected = 1
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
      )
    ),
    fluidRow(
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
  filterData <- reactive({
    data %>%
      filter(Sp %in% input$species,
             TotalChlaSheetName %in% input$sheet)
  })
  
  finalData <- reactive({
    filterData() %>%
      filter(Target == input$target)
  })
  
  compareData <- reactive({
    filterData() %>%
      pivot_wider(id_cols = c(Stress_h, Inhibitor, Sp, CultureId), 
                  names_from = Target, 
                  values_from = fmol_ugChla)
  })
  
  output$speciesPlot <- renderPlot({
    
    aesInput <- list(x = input$xaxis,
                     y = input$yaxis,
                     color = input$color)
    
    aesInputFiltered <- lapply(
      aesInput[aesInput != "None"],
      sym)
    
    facetInput <- c(input$facetX,
                    input$facetY)
    
    facetInputFiltered <- facetInput[facetInput != "None"]
    
    if(input$plotType == 1){
      ggplot(finalData()) +
        geom_point(do.call(aes, aesInputFiltered), 
                   size = input$size) +
        theme_bw() +
        ggtitle(paste("Protein Target:", input$target)) +
        if(length(facetInputFiltered) == 1){
          facet_grid(reformulate(facetInputFiltered))
        } else if(length(facetInputFiltered) == 2){
          facet_grid(reformulate(input$facetX, input$facetY))
        } 
    } else {
      ggplot(compareData()) +
        geom_point(do.call(aes, aesInputFiltered), 
                   size = input$size) +
        theme_bw() +
        theme(aspect.ratio = 1) +
        ggtitle(paste("Compare Targets:", input$yaxis, "vs", input$xaxis)) 
      
    }
  })
}

# Run app ----
shinyApp(ui, server)