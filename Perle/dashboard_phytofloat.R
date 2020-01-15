## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 500)),
      box(title = "Variable",
          selectInput("variable", "Variable",
                      choices = c("Prochloroccoccus par mL" = "Proc/mL", "Synechococcus par mL" = "Syn/mL", "Prochlorococcus (mg)" = "Proc_Chl", "Synechococcus (mg)" = "Syn_Chl")))
    ),
    fluidRow(
      box(title = "Map", plotOutput("plot2", height = 500))
    )
  )
)

server <- function(input, output) { 

  map <- read_csv("map_vec")
  
  ref_ctd0 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx")
  ref_ctd0 <- clean_names(ref_ctd0)
  perle_00 <- read_excel("Perle/Perle0_juil2018_CYTO.xlsx")
  ref_ctd0 <- filter(ref_ctd0, type %in% c("PHYTOFLOAT", "PHYTODEEP"))
  perle_00$Description <- gsub("-", "_", perle_00$Description)
  perle_00$Description <- gsub("^(P0_)([0-9]{2}_[0-9]{2})$", "\\10\\2", perle_00$Description) #uniformise le code d'echantillon
  ref_ctd0$cyto <- substr(ref_ctd0$cyto, 1, 9)
  data_00 <- left_join(perle_00, ref_ctd0, by = c("Description" = "cyto"))
  
  dat <- reactive({
    test <- select(data_00, pressure, Station, longitude, latitude, input$variable)
    names(test) <- c("pressure", "Station", "longitude", "latitude", "variable")
    return(test)})
    
  output$plot1 <- renderPlot({
    ggplot(dat(), aes(x = as.factor(- pressure), y = variable, fill = as.factor(Station)))+
    geom_col()+
    xlab("Depth")+
    coord_flip()+
    facet_wrap(.~ Station, scales = "free_y")+
    scale_fill_viridis_d()})

  output$plot2 <- renderPlot({
    t <- dat()
    xmin <- min(t$longitude) - 5
    xmax <- max(t$longitude) + 5
    ymin <- min(t$latitude) - 5
    ymax <- max(t$latitude) + 5
    ggplot(data_00)+
      geom_label(aes(x = longitude, y = latitude, label = Station, colour = as.factor(Station)))+
      geom_polygon(aes(x = long, y = lat, group = group), data = map)+
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin,ymax))+
      scale_color_viridis_d()
  })
}

shinyApp(ui, server)
