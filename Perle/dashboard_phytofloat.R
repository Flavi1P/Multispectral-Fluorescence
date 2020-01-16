## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(selectInput("dataset", "Campagne", choices = c("Perle 0" = "perle0.csv", "Perle 1" = "perle1.csv", "Perle 2" = "perle2.csv")),
                   selectInput("scales", "Scales", choices = c("Fixed" = "fixed", "Free X" = "free_x", "Free Y" = "free_y", "Free X and Y" = "free")),
                   selectInput("variable", "Variable Barplot 1",
                               choices = c("Prochloroccoccus/mL" = "Proc/mL",
                                           "Synechococcus/mL" = "Syn/mL",
                                           "Prochlorococcus (mg.Chla)" = "Proc_Chl",
                                           "Synechococcus (mg.Chla)" = "Syn_Chl",
                                           "Pico/mL" = "Pico/mL",
                                           "Nano/mL" = "Nano/mL",
                                           "Chla fluo CTD" = "ctdfchl",
                                           "Chla" = "chlorophyll_a")),
                   selectInput("variable2", "Variable Barplot 2",
                               choices = c("Synechococcus/mL" = "Syn/mL",
                                           "Prochloroccoccus/mL" = "Proc/mL",
                                           "Prochlorococcus (mg.Chla)" = "Proc_Chl",
                                           "Synechococcus (mg.Chla)" = "Syn_Chl",
                                           "Pico/mL" = "Pico/mL",
                                           "Nano/mL" = "Nano/mL",
                                           "Chla fluo CTD" = "ctdfchl",
                                           "Chla" = "chlorophyll_a")),
                   selectInput("variableX", "X axis Scatterplot",
                               choices = c("Pico/mL" = "Pico/mL",
                                           "Prochloroccoccus/mL" = "Proc/mL",
                                           "Synechococcus/mL" = "Syn/mL",
                                           "Prochlorococcus (mg.Chla)" = "Proc_Chl",
                                           "Synechococcus (mg.Chla)" = "Syn_Chl",
                                           "Nano/mL" = "Nano/mL",
                                           "Chla fluo CTD" = "ctdfchl",
                                           "Chla" = "chlorophyll_a")),
                   selectInput("variableY", "Y axis Scatterplot",
                               choices = c("Nano/mL" = "Nano/mL",
                                           "Prochloroccoccus/mL" = "Proc/mL",
                                           "Synechococcus/mL" = "Syn/mL",
                                           "Prochlorococcus (mg.Chla)" = "Proc_Chl",
                                           "Synechococcus (mg.Chla)" = "Syn_Chl",
                                           "Pico/mL" = "Pico/mL",
                                           "Chla fluo CTD" = "ctdfchl",
                                           "Chla" = "chlorophyll_a"))),
  dashboardBody(
    fluidRow(
      box(title = "Barplot 1", plotOutput("plot1", height = 400)),
      box(title = "Barplot 2", plotOutput("plot2", height = 400))),
    fluidRow(
      box(title = "Map", plotOutput("plot3", height = 400)),
      box(title = "Scatterplot", plotOutput("plot4", height = 400)))
  )
)

server <- function(input, output) { 

  map <- read_csv("map_vec")
  
  data_00 <- read_csv("Perle/Process/perle0.csv")
  
  dat <- reactive({
    path <- paste("Perle/Process/", input$dataset, sep = "")
    test <- read_csv(path) %>% select(pressure, station, longitude, latitude, input$variable, input$variable2, input$variableX, input$variableY)
    names(test) <- c("pressure", "station", "longitude", "latitude", "variable", "variable2", "variableX", "variableY")
    return(test)})
    
  output$plot1 <- renderPlot({
    ylab <- input$variable
    ggplot(dat(), aes(x = as.factor(- round(pressure)), y = variable, fill = station))+
    geom_col()+
    xlab("Depth")+
    ylab(ylab)+
    coord_flip()+
    facet_wrap(.~ station, scales = input$scales)+
    scale_fill_viridis_d()})
  
  output$plot2 <- renderPlot({
    ylab <- input$variable2
    ggplot(dat(), aes(x = as.factor(- round(pressure)), y = variable2, fill = station))+
      geom_col()+
      ylab(ylab)+
      xlab("Depth")+
      coord_flip()+
      facet_wrap(.~ station, scales = input$scales)+
      scale_fill_viridis_d()})

  output$plot3 <- renderPlot({
    t <- dat()
    xmin <- min(t$longitude) - 5
    xmax <- max(t$longitude) + 5
    ymin <- min(t$latitude) - 5
    ymax <- max(t$latitude) + 5
    ggplot(dat())+
      geom_label(aes(x = longitude, y = latitude, label = station, colour = station))+
      geom_polygon(aes(x = long, y = lat, group = group), data = map)+
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin,ymax))+
      scale_color_viridis_d()
  })
  
  output$plot4 <- renderPlot({
    xname <- input$variableX
    yname <- input$variableY
    ggplot(dat(), aes(x = round(variableX), y = round(variableY), colour = pressure))+
      geom_point()+
      xlab(xname)+
      ylab(yname)+
      scale_colour_viridis_c()})

}

shinyApp(ui, server)

