
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(purrr)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/results/processed"

# Read data
data <- readRDS(file.path(datadir, "nite_moves_data.Rds"))


# Plot data
################################################################################

# Subset data
tm <- filter(data, name=="Tracey Mangin" & event=="Aquathon" & !is.na(time)) %>% 
  select(date, time) %>% 
  mutate(time1=ifelse(nchar(time)==5, paste0("0:", time), time),
         time2=hms(time1),
         time5=as.duration(time2),
         time3=second(time2)/60,
         time4=as.POSIXct(strptime(time1, format="%H:%M:%S")))
         

strptime("1:32:16", format="%H:%M:%S")

  mutate(time1=strftime(time, "%H:%M"))
  # mutate(time=ms(time))

  
g <- ggplot(tm, aes(x=date, y=time4)) + 
  geom_line() +
  geom_point() +
  labs(x="Date", y="Time") +
  theme_bw()
g







