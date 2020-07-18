# Shiny app for GreenEdgeProtein
library(tidyverse)

data <- readRDS("ProcessData/GreenEdge_TargetProteinReport_.Rds") %>%
  filter(!is.na("Stress_h"))

# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      selectInput("normalizeMethod", 
                  label = "Choose normalize",
                  choices = c("fmol_ugTotalProtein", "fmol_mL"),
                  selected = "fmol_ugTotalProtein"),
      selectInput("target", 
                  label = "Choose target",
                  choices = c("PsbA", "Fcp6"),
                  selected = "PsbA")
    ),
    
    mainPanel(plotOutput("speciesPlot"))
  )
)

# Server logic ----
server <- function(input, output) {
  finalData <- reactive({
    test <- data %>%
      filter(Target == input$target,
             Sp == "Chaetoceros neogracilis")
    test
  })
  
  output$speciesPlot <- renderPlot({
    normalizeMethodSym <- sym(input$normalizeMethod)
    ggplot(finalData()) +
      geom_point(aes(x = Stress_h, y = !!normalizeMethodSym))+#, colour = as.factor(CultureId))) +
      facet_grid(vars(Sp), vars(Inhibitor))+
      theme_bw()+
      ggtitle("PsbA")
  })
}

# Run app ----
shinyApp(ui, server)