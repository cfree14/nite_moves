
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
library(lubridate)

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
swim3 <- filter(data, event == "500m swim")


# Get racers
# Sorted, unique, unidentified removed
get_racers <- function(x){
  racers <- sort(unique(x$name))
  racers <- racers[!racers %in% c("*** Unidentified", "Unidentified", "Unknown")]
  return(racers)
}

# Vectors of racers
racers <- get_racers(data)
racers_aqua <- get_racers(aqua)
racers_run5 <- get_racers(run5)
racers_swim1 <- get_racers(swim1)
racers_swim2 <- get_racers(swim2)
racers_swim3 <- get_racers(swim3)


# User interface
################################################################################

# User interface
ui <- navbarPage("",
                 
  # welcome page
  tabPanel("Home",
           
    # title
    titlePanel("Working on our Nite Moves"),
    
    ## 
    sidebarLayout(
      
      sidebarPanel(
        checkboxGroupInput(inputId = "seasons", "Choose season(s):",
                           choices = as.character(unique(data$season)), selected = "2019", inline = FALSE),
        
        selectInput(inputId = "event", "Choose event:",
                    choices = unique(data$event), selected = "5km run", multiple = FALSE),
      
        selectizeInput(inputId = "participant", label = "Choose racer:", 
                       choices = c("None", racers), selected = "None", multiple = FALSE,
                       options = NULL)),
      
        # Plot homepage figure
        mainPanel(plotOutput(outputId = "homepage_fig"))
        
      )),
      
                 
  # Results table
  tabPanel("All",
           
    # Title
    titlePanel("All results"), 
    
    # Select athlete
    selectInput("allAthlete",
                "Select athlete:",
                choices=sort(unique(data$name)), multiple=F, selectize=F),
    
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
    
    # Format data for display
    allAthlete <- "Tracey Mangin" 
    sdata <- data %>% 
      filter(name==allAthlete) %>% 
      select(date, event, place, time, gplace, dplace, sw_time, sw_place, rn_time, rn_place)
    
  }))
  
  ## homepage plot
  output$homepage_fig <- renderPlot({
    
    # Parameters
    # event_name <- "Aquathon"; season_vec <- 2019; racer_name <- "Chris Free"
    event_name <- input$event
    season_vec <- as.numeric(input$seasons)
    racer_name <- input$participant
    
    
    # Reshape data for plotting
    sdata <- aqua %>%
      select(season, date, name, time, sw_time, rn_time) %>% 
      filter(season==2019) %>% 
      gather(key="segment", value="time", 4:6) %>% 
      mutate(segment=recode(segment, 
                            "time"="Overall", 
                            "rn_time"="Run", 
                            "sw_time"="Swim"))
    
    # Plot data
    g <- ggplot(sdata, aes(x=date, y=time, group="name")) +
      geom_line() +
      facet_wrap(~segment, ncol=1, scale="free") +
      # Labels
      labs(x="", y="") +
      theme_bw()
    g
    
    
    # Aquathon plot
    g <- ggplot(aqua, aes(x=date, y=time))
    
    
    
    
    # Plot data
    ## each unitentified racer needs unique name.
    
    hpfig <- ggplot(data = data %>% filter(event == event_name,
                                    season %in% season_vec), aes(x = date, y = time)) +
      geom_point(size = 0.5, alpha = 0, color = "#CCCCCC") +
      geom_line(data = data %>% filter(event == event_name,
                                       season %in% season_vec), aes(x = date, y = time, group = name), color = "#CCCCCC") +
      geom_line(data = data %>% filter(event == event_name,
                                      season %in% season_vec,
                                      name == racer_name), aes(x = date, y = time), color = "#28A072", alpha = 1, size = 1) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~season, nrow = 1, scales = "free") +
      # ggtitle(paste(input$event, "Results", sep = " ")) +
      labs(x="Date", y="Time") +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            title = element_text(size = 16),
            strip.text = element_text(size = 14),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    hpfig
    
    
  })
  

  
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
