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