
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)

# Read data
datadir <- "data/results/processed"

# Load data
data <- readRDS(file.path(datadir, "nite_moves_data.Rds"))

# Format data
################################################################################

# Subset data
aqua <- filter(data, event=="Aquathon")
run5 <- filter(data, event=="5km run")
swim1 <- filter(data, event=="1km swim")
swim2 <- filter(data, event=="2km swim")


# User interface
################################################################################

# User interface
ui <- navbarPage("",
                 
  # welcome page
  tabPanel("Home"),
                 
  # Results table
  tabPanel("All",
           
  # Title
  titlePanel("All results"), 
  
  # Results table
  DT::dataTableOutput("resultsTable")
           
  ),
  
  # Swim plot
  tabPanel("Swim",
           
    # Title
    titlePanel("1 km & 2 km swim results"),
    
    # Select athlete
    selectInput("swim1Athlete",
                "Select athlete:",
                choices=sort(unique(swim1$name)), multiple=F, selectize=F)
           
  ),
  
  # Run plot
  tabPanel("Run",
           
    # Title
    titlePanel("5 km run results"),
    
    # Select athlete
    selectInput("runAthlete",
                "Select athlete:",
                choices=sort(unique(run5$name)), multiple=F, selectize=F)
    
  ),
  
  # Aquathon plot
  tabPanel("Aquathon",
           
    # Title
    titlePanel("Aquathon results"),
    
    # Select athlete
    selectInput("aquaAthlete",
               "Select athlete:",
               choices=sort(unique(aqua$name)), multiple=F, selectize=F),
    
    # Plot aquathon times
    plotOutput(outputId = "aquathonPlot")
    
  ),
  
  # Aquathon plot
  tabPanel("Records")
  
)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # All results table
  # Filter data based on selections
  output$resultsTable <- DT::renderDataTable(DT::datatable({
    data
  }))
  
  # Plot aquathon times
  output$aquathonPlot <- renderPlot({
    
    # Format data
    # athlete <- "Tracey Mangin"
    athlete <- input$aquaAthlete
    sdata <- aqua %>% 
      filter(name==athlete) %>% 
      # Wide-to-long for plotting
      select(date, time, sw_time, rn_time) %>% 
      gather(key="type", value="time", 2:4) %>% 
      mutate(type=plyr::revalue(type, c("time"="Overall",
                                        "sw_time"="Swim", 
                                        "rn_time"="Run"))) %>% 
      # Format time
      mutate(time1=ifelse(nchar(time)==5, paste0("0:", time), time),
             time2=as.POSIXct(strptime(time1, format="%H:%M:%S")))
    
    # Plot data
    g <- ggplot(sdata, aes(x=date, y=time2, col=type)) +
      geom_line() +
      geom_point() +
      labs(x="Date", y="Time") +
      theme_bw()
    g

    
  })
  

  
}

shinyApp(ui = ui, server = server)
