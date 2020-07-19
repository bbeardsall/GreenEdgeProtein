# Shiny app for GreenEdgeProtein
library(tidyverse)
library(stats)
library(shinydashboard)

data <- readRDS("data/GreenEdge_TargetProteinReport_.Rds") %>%
  filter(!is.na("Stress_h"))

# User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "GreenEdgeProtein"),
  dashboardSidebar(
    width = 500,
    fluidRow(
      box(background = "black",
          selectInput("xaxis", 
                      label = "X axis",
                      choices = c("Growth_uE", "Stress_h"),
                      selected = "Stress_h")
      ),
      box(
        background = "black",
        selectInput("yaxis", 
                    label = "Y axis",
                    choices = c("fmol_ugTotalProtein", "fmol_mL", "fmol_ugChla"),
                    selected = "fmol_ugTotalProtein")
      )
      
    ),
    
    fluidRow(
      box(
          background = "black",
          selectInput("target", 
                      label = "Target",
                      choices = unique(data$Target),
                      selected = "PsbA")
          ),
      
      box(title = "Species",
          background = "black",
          checkboxGroupInput("species", 
                             NULL, 
                             choices = setNames(as.list(unique(data$Sp)), unique(data$Sp)),
                             selected = unique(data$Sp))
      )
          
      ),
    fluidRow(
      box(
        background = "black",
      
      
      checkboxGroupInput("sheet", 
                         h3("ChlA Sheet Name"), 
                         choices = setNames(as.list(unique(data$TotalChlaSheetName)), unique(data$TotalChlaSheetName)),
                         selected = unique(data$TotalChlaSheetName))
      ),
      box(
        background = "black",
        selectInput("color",
                    label = "Choose color",
                    choices = c("Sp", "Inhibitor", "None"),
                    selected = "None")
        ),
      box(
        background = "black",
      selectInput("facet",
                  label = "Choose facet",
                  choices = c("Sp", "Inhibitor", "None"),
                  selected = "None")
      )
    )
      
        
      ),
      
      
      
      
    
    dashboardBody(plotOutput("speciesPlot"),
                  height = "5000px"
                  )
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
    yaxisSym <- sym(input$yaxis)
    ColorSym <- sym(input$color)
    FacetSym <- sym(input$facet)
    xaxisSym <- sym(input$xaxis)
    
    ggplot(finalData()) +
      {if(input$color != "None"){
        geom_point(aes(x = !!xaxisSym, y = !!yaxisSym, color = !!ColorSym))
        }
        else{
          geom_point(aes(x = !!xaxisSym, y = !!yaxisSym))
        }}+
      #geom_point(aes(x = Stress_h, y = !!yaxisym, color = !!ColorSym))+#, colour = as.factor(CultureId))) +
      {if(input$facet != "None"){
        facet_wrap(vars(!!FacetSym))
      }}+
      #facet_wrap(vars(Inhibitor))+
      #facet_grid(vars(Sp), vars(Inhibitor))+
      theme_bw()+
      ggtitle(paste("Protein Target:", input$target))
    
  })
}

# Run app ----
shinyApp(ui, server)