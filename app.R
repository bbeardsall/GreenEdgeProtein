# Shiny app for GreenEdgeProtein
library(tidyverse)

test <- data %>%
  filter(TotalChlaSheetName == "Nf_Growthvs.IceCamp")
test

data <- readRDS("ProcessData/GreenEdge_TargetProteinReport_.Rds") %>%
  filter(!is.na("Stress_h"))

# User interface ----
ui <- fluidPage(
  #titlePanel("GreenEdgeProtein"),
  
  sidebarLayout(
    sidebarPanel(
      #helpText("Interactive plotting of GreenEdgeProtein quantification results."),
      
      selectInput("xaxis", 
                  label = "Choose x axis",
                  choices = c("Growth_uE", "Stress_h"),
                  selected = "Stress_h"),
      
      selectInput("yaxis", 
                  label = "Choose y axis (normalize method)",
                  choices = c("fmol_ugTotalProtein", "fmol_mL", "fmol_ugChla"),
                  selected = "fmol_ugTotalProtein"),
      
      selectInput("target", 
                  label = "Choose target",
                  choices = unique(data$Target),
                  selected = "PsbA"),
      
      checkboxGroupInput("species", 
                         h3("Choose species"), 
                         choices = setNames(as.list(unique(data$Sp)), unique(data$Sp)),
                         selected = unique(data$Sp)),
      checkboxGroupInput("sheet", 
                         h3("ChlA Sheet Name"), 
                         choices = setNames(as.list(unique(data$TotalChlaSheetName)), unique(data$TotalChlaSheetName)),
                         selected = unique(data$TotalChlaSheetName)),
      
      selectInput("color",
                  label = "Choose color",
                  choices = c("Sp", "Inhibitor", "None"),
                  selected = "None"),
      
      selectInput("facet",
                  label = "Choose facet",
                  choices = c("Sp", "Inhibitor", "None"),
                  selected = "None"),
      ),
    
    
    
    mainPanel(plotOutput("speciesPlot"))
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