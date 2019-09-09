
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
data_orig <- readRDS(file.path(datadir, "nite_moves_data.Rds")) 

# Format data for aesthetics
data <- data_orig %>% 
  mutate(event=recode(event, 
                      "500m swim"="500 m swim",
                      "1km swim"="1 km swim",
                      "2km swim"="2 km swim",
                      "5km run"="5 km run"),
         event=factor(event, levels=c("Aquathon", "5 km run", "1 km swim", "2 km swim", "500 m swim"))) 


# Format data
################################################################################

# Subset data
aqua <- filter(data, event=="Aquathon")
run5 <- filter(data, event=="5km run")
swim1 <- filter(data, event=="1km swim")
swim2 <- filter(data, event=="2km swim")
swim3 <- filter(data, event =="500m swim")

# Get racers
# Unique, unidentified removed, and sorted
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

# Setup theme
my_theme <- theme(axis.text=element_text(size=11),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=13),
                  axis.title=element_text(size=13),
                  strip.text=element_text(size=13),
                  plot.title=element_text(size=16),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))



# User interface
################################################################################

# User interface
ui <- navbarPage("",
                 
  # Welcome page
  tabPanel("Home",
           
    # Title
    titlePanel("Working On Our Nite Moves"),
    
    # Layout
    sidebarLayout(
      
      # Sidebar: plotting options
      sidebarPanel(
        
        # Select season
        checkboxGroupInput(inputId = "seasons", "Choose season(s):",
                           choices = as.character(unique(data$season)), selected = "2019", inline = FALSE),
        
        # Select event
        selectInput(inputId = "event", "Choose event:",
                    choices = levels(data$event), selected = "5km run", multiple = FALSE),
      
        # Select athletes
        selectizeInput(inputId = "participant", label = "You, your friends, and your nemeses:", 
                       choices = c(racers),  multiple = T, options = NULL)),
      
        # Main panel: main plot
        mainPanel(
          plotOutput(outputId = "mainPlot")
        )
        
      )),
  
  # Individual results
  tabPanel("Individual results",
  
    # Title
    titlePanel("Individual results"),
    
    # Select athlete
    selectizeInput(inputId = "participant1", label = "Select athlete:", 
                   choices = c(racers), multiple = F, options = NULL),
    
    # Plot individual results
    plotOutput(outputId = "indivEventPlot"),
    br(), 
    plotOutput(outputId = "indivAquaPlot")
    
    
           
  ),
  
  # Records
  tabPanel("Records",
           
    # Title
    titlePanel("Nite Moves Course Records"),
    
    # Running records
    h3("5 km run records"),
    h5("Top-ten runs"),
    tableOutput("top10runs"),
    h5("Top-ten women's runs"),
    tableOutput("top10runs_f"),
    h5("Top-ten men's runs"),
    tableOutput("top10runs_m"),
    br(),
    
    # Swim records
    h3("1 km swim records"),
    h5("Top-ten swims"),
    tableOutput("top10swims"),
    h5("Top-ten women's swims"),
    tableOutput("top10swims_f"),
    h5("Top-ten men's swims"),
    tableOutput("top10swims_m"),
    br(),
    
    # Aquathon records
    h3("Aquathon records"),
    h5("Top-ten aquathons"),
    tableOutput("top10aquathons"),
    h5("Top-ten women's aquathons"),
    tableOutput("top10aquathons_f"),
    h5("Top-ten men's aquathons"),
    tableOutput("top10aquathons_m"),
    br()
           
  )
  
  
)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Main plot
  #########################################
  
  output$mainPlot <- renderPlot({
    
    # Parameters
    # event_name <- "Aquathon"; season_vec <- 2019; racer_vec <- "Chris Free"
    event_name <- input$event
    season_vec <- as.numeric(input$seasons)
    racer_vec <- input$participant
    
    # Aquathon plot
    if(event_name=="Aquathon"){
    
      # Subset and reshape data for plotting
      sdata <- data %>% 
        filter(event=="Aquathon" & season %in% season_vec) %>% 
        select(season, date, name, time, sw_time, rn_time) %>% 
        gather(key="segment", value="time", 4:6) %>% 
        mutate(segment=recode(segment, 
                              "time"="Overall", 
                              "rn_time"="Run (5.3 km)", 
                              "sw_time"="Swim (1 km)"),
               segment=factor(segment, levels=c("Overall", "Swim (1 km)", "Run (5.3 km)")))
      
      # Plot data
      g <- ggplot(sdata, aes(x=date, y=time, group=name)) +
        # Facet on segment
        # facet_wrap(~ segment, ncol=1, scale="free") +
        facet_grid(segment ~ season, scale="free") +
        # Add all racers
        geom_line(color="grey80") +
        # Add racer subset
        geom_line(filter(sdata, name %in% racer_vec), 
                  mapping=aes(x=date, y=time, col=name)) +
        geom_point(filter(sdata, name %in% racer_vec), 
                   mapping=aes(x=date, y=time, col=name)) +
        # Add legend
        scale_color_discrete(name="Athlete") +
        # Add season trend
        # geom_smooth(method = "lm", se = FALSE) +
        # Labels
        labs(x="Week", y="Time") +
        theme_bw() + my_theme
      g
    
    # Single event plots
    }else{
      
      # Subset and reshape data for plotting
      sdata <- data %>% 
        filter(event==event_name & season %in% season_vec) %>% 
        select(season, date, name, time)
      
      # Plot data
      g <- ggplot(sdata, aes(x=date, y=time, group=name)) +
        # Facet on segment
        facet_wrap(~ season, nrow=1, scale="free") +
        # Add all racers
        geom_line(color="grey80") +
        # Add racer subset
        geom_line(filter(sdata, name %in% racer_vec), 
                  mapping=aes(x=date, y=time, col=name)) +
        geom_point(filter(sdata, name %in% racer_vec), 
                   mapping=aes(x=date, y=time, col=name)) +
        # Add legend
        scale_color_discrete(name="Athlete") +
        # Add season trend
        # geom_smooth(method = "lm", se = FALSE) +
        # Labels
        labs(x="Week", y="Time") +
        theme_bw() + my_theme
      g
      
    }
    
    
  })
  
  # Individual results
  #########################################
  
  # Individual event results
  output$indivEventPlot <- renderPlot({
    
    # Subset/format data
    racer <- input$participant1
    sdata <- data %>% 
      filter(name==racer) %>% 
      select(season, date, name, event, time)
    
    # Plot data
    g <- ggplot(sdata, aes(x=date, y=time, color=event)) +
      facet_wrap(~ season, scales="free_x") +
      geom_line() +
      geom_point() +
      labs(x="Week", y="Time", title="Event results") +
      scale_color_discrete(name="Event") +
      theme_bw() + my_theme
    g
    
  })
  
  # Individual aquathon plots
  output$indivAquaPlot <- renderPlot({
    
    # Subset/format data
    racer <- input$participant1
    sdata <- data %>% 
      filter(event=="Aquathon" & name==racer) %>% 
      select(season, date, name, time, sw_time, rn_time) %>% 
      gather(key="segment", value="time", 4:6) %>% 
      mutate(segment=recode(segment, 
                            "time"="Overall", 
                            "rn_time"="Run (5.3 km)", 
                            "sw_time"="Swim (1 km)"),
             segment=factor(segment, levels=c("Overall", "Swim (1 km)", "Run (5.3 km)")))
    
    # Plot data
    g <- ggplot(sdata, aes(x=date, y=time, color=segment)) +
      facet_wrap(~ season, scales="free_x") +
      geom_line() +
      geom_point() +
      labs(x="Week", y="Time", title="Aquathon segment results") +
      scale_color_discrete(name="Segment") +
      theme_bw() + my_theme
    g
    
  })
  
  
  
  # Records
  #########################################
  
  # Build records function
  # event1 <- "5 km run"; gender1 <- "M"
  build_records <- function(event1, gender1){
    if(gender1=="all"){genders <- c("M", "F")}else{genders <- gender1}
    records <- data %>% 
      # Reduce to runs
      filter(event==event1 & gender %in% genders) %>% 
      arrange(time) %>% 
      slice(1:10) %>%
      # Format columns
      mutate(rank=1:n(),
             time=format(time, "%M:%S"),
             date=format(date, "%m/%d/%Y"),
             age=as.character(age)) %>% 
      # Reduce and rename columns 
      select(rank, time, name, gender, age, date) %>% 
      setNames(stringr::str_to_title(colnames(.))) 
    return(records)
  }
  
  # Running records
  output$top10runs <- renderTable({runs_m <- build_records("5 km run", "all")})
  output$top10runs_m <- renderTable({runs_m <- build_records("5 km run", "M")})
  output$top10runs_f <- renderTable({runs_m <- build_records("5 km run", "F")})
  
  # Swimming records
  output$top10swims <- renderTable({runs_m <- build_records("1 km swim", "all")})
  output$top10swims_m <- renderTable({runs_m <- build_records("1 km swim", "M")})
  output$top10swims_f <- renderTable({runs_m <- build_records("1 km swim", "F")})
  
  # Aquathon records
  output$top10aquathons <- renderTable({runs_m <- build_records("Aquathon", "all")})
  output$top10aquathons_m <- renderTable({runs_m <- build_records("Aquathon", "M")})
  output$top10aquathons_f <- renderTable({runs_m <- build_records("Aquathon", "F")})

}

shinyApp(ui = ui, server = server)
