
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(purrr)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/results/raw"
outdir <- "data/results/processed"

# 4 sections to each text file:
# 5k run: place, bib, name, age, city, state, time, pace, gplace, dplace
# 1k swim: place, bib, name, age, city, state, time, gplace, dplace
# 2k swim: place, bib, name, age, city, state, time, gplace, dplace
# Aquathon: place, bib, name, age, city, state, 
#           sw_time, sw_place, rn_time, rn_place, time, gplace, dplace


# Functions
################################################################################

# Function to format Nite Moves results text file
filename <- "data/results/raw/Nite Moves Results 2018/Nite Moves 7_25_18.txt"
format_data <- function(filename){
  
  # Read text file into lines
  text <- readLines(filename)
  
  # Extract date
  date <- mdy(gsub(".txt", "", gsub("Nite Moves ", "", basename(filename))))

  # Identify "line" and "blank" rows
  # Use to find row numbers of event names, headers, and the first/last rows of data
  rows_line <- which(grepl("===", text))
  rows_blank <- which(text=="")
  rows_blank1 <- rows_blank[-c(1:2)]
  
  # Build key listing event names and rows of headers and the first/last rows of data
  rows_head <- rows_line - 1
  rows_data1 <- rows_line + 1
  rows_event <- rows_line - 3
  rows_data2 <- c(rows_event[-1] - 2, length(text)) # 2 rows before the header (except nothing before first header) plus last row
  events <- trimws(text[rows_event])
  key <- data.frame(event=events, 
                    row_head=rows_head,
                    row_data1=rows_data1,
                    row_data2=rows_data2,
                    stringsAsFactors = F)

  # Format 5km run
  ############################################

  # Format 5km run data if available
  if("5K RUN" %in% key$event){
    
    # Determine number of characters associated with each column
    # This is necessary for reading in the file using fixed width columns
    col_names <- c("place", "bib", "name", "age", "city", "state", "time", "pace", "gplace", "dplace")
    col_widths <- c(5, 6, 23, 5, 19, 3, 7, 6, 5, 10)
    
    # Read data using line references and fixed widths
    row1 <- key$row_data1[key$event=="5K RUN"]
    row2 <- key$row_data2[key$event=="5K RUN"]
    skip <- row1 - 1
    nrows <- row2 - row1 + 1
    run5km_orig <- read.fwf(file=filename,
                          widths=col_widths, fill=T, na.strings=NA,
                          header=F, skip=skip, nrows=nrows, col.names=col_names, as.is=T)
    
    # Format data
    run5km <- run5km_orig %>% 
      # Trim white space
      mutate(name=trimws(name),
             city=trimws(city),
             state=trimws(state),
             time=trimws(time),
             pace=trimws(pace),
             dplace=trimws(dplace),
             # Format gender/age columns
             gender=ifelse(grepl("F", age), "F", "M"),
             age=as.numeric(gsub("F", "", age)),
             # Add date/event columns
             event="5km run",
             date=date) %>% 
      select(date, event, place, bib, name, age, gender, everything())
    
  }
  
  
  # Format 500m swim
  ############################################
  
  # Format 500m swim data if available
  if("500m SWIM" %in% key$event){
  
    # Determine number of characters associated with each column
    # This is necessary for reading in the file using fixed width columns
    col_names <- c("place", "bib", "name", "age", "city", "state", "time", "gplace", "dplace")
    header_row <- key$row_head[key$event=="500m SWIM"]
    header_text <- text[header_row]
    header_nchar <- nchar(header_text)
    col_widths <- c(5, 6, 23, 5, 18, 3, 7, 5, 11)
    sum(col_widths) # 89
    
    # Read data using line references and fixed widths
    row1 <- key$row_data1[key$event=="500m SWIM"]
    row2 <- key$row_data2[key$event=="500m SWIM"]
    skip <- row1 - 1
    nrows <- row2 - row1 + 1
    swim500m_orig <- read.fwf(file=filename,
                             widths=col_widths,
                             header=F, skip=skip, nrows=nrows, col.names=col_names, as.is=T)
    
    # Format data
    swim500m <- swim500m_orig %>%
      # Trim white space
      mutate(name=trimws(name),
             city=trimws(city),
             state=trimws(state),
             time=trimws(time),
             dplace=trimws(dplace),
             # Format gender/age columns
             gender=ifelse(grepl("F", age), "F", "M"),
             age=as.numeric(gsub("F", "", age)),
             # Add date/event columns
             event="500m swim",
             date=date) %>%
      select(date, event, place, bib, name, age, gender, everything())
    
  }
  
  
  # Format 1km swim
  ############################################

  # Format 1km swim data if available
  if("1K SWIM" %in% key$event){
    
    # Determine number of characters associated with each column
    # This is necessary for reading in the file using fixed width columns
    col_names <- c("place", "bib", "name", "age", "city", "state", "time", "gplace", "dplace")
    header_row <- key$row_head[key$event=="1K SWIM"]
    header_text <- text[header_row]
    header_nchar <- nchar(header_text)
    col_widths <- c(5, 6, 23, 5, 18, 3, 7, 5, 11)
    sum(col_widths) # 89
  
    # Read data using line references and fixed widths
    row1 <- key$row_data1[key$event=="1K SWIM"]
    row2 <- key$row_data2[key$event=="1K SWIM"]
    skip <- row1 - 1
    nrows <- row2 - row1 + 1
    swim1km_orig <- read.fwf(file=filename,
                          widths=col_widths,
                          header=F, skip=skip, nrows=nrows, col.names=col_names, as.is=T)
  
    # Format data
    swim1km <- swim1km_orig %>%
      # Trim white space
      mutate(name=trimws(name),
             city=trimws(city),
             state=trimws(state),
             time=trimws(time),
             dplace=trimws(dplace),
             # Format gender/age columns
             gender=ifelse(grepl("F", age), "F", "M"),
             age=as.numeric(gsub("F", "", age)),
             # Add date/event columns
             event="1km swim",
             date=date) %>%
      select(date, event, place, bib, name, age, gender, everything())
    
  }


  # Format 2km swim
  ############################################

  # Format 2km swim data if available
  if("2K SWIM" %in% key$event){
    
    # Determine number of characters associated with each column
    # This is necessary for reading in the file using fixed width columns
    col_names <- c("place", "bib", "name", "age", "city", "state", "time", "gplace", "dplace")
    header_row <- key$row_head[key$event=="2K SWIM"]
    header_text <- text[header_row]
    header_nchar <- nchar(header_text)
    col_widths <- c(5, 6, 23, 5, 18, 3, 7, 5, 11)
    sum(col_widths) # 89
  
    # Read data using line references and fixed widths
    row1 <- key$row_data1[key$event=="2K SWIM"]
    row2 <- key$row_data2[key$event=="2K SWIM"]
    skip <- row1 - 1
    nrows <- row2 - row1 + 1
    swim2km_orig <- read.fwf(filename,
                          widths=col_widths,
                          header=F, skip=skip, nrows=nrows, col.names=col_names, as.is=T)
  
    # Format data
    swim2km <- swim2km_orig %>%
      # Trim white space
      mutate(name=trimws(name),
             city=trimws(city),
             state=trimws(state),
             time=trimws(time),
             dplace=trimws(dplace),
             # Format gender/age columns
             gender=ifelse(grepl("F", age), "F", "M"),
             age=as.numeric(gsub("F", "", age)),
             # Add date/event columns
             event="2km swim",
             date=date) %>%
      select(date, event, place, bib, name, age, gender, everything())
    
  }

  # Format aquathon
  ############################################

  # Format 1km swim data if available
  if("AQUATHON" %in% key$event){
  
    # Column names
    col_names <- c("place", "bib", "name", "age", "city", "state", "sw_time",
                   "sw_place", "rn_time", "rn_place", "time", "gplace", "dplace")
  
    # Determine number of characters associated with each column
    # This is necessary for reading in the file using fixed width columns
    header_row <- key$row_head[key$event=="AQUATHON"]
    header_text <- text[header_row]
    header_nchar <- nchar(header_text)
    col_widths <- c(5, 6, 23, 5, 15, 3,
                    7, 6,
                    7, 6,
                    7, 5, 11)
    sum(col_widths) # 106
  
    # Read data using line references and fixed widths
    row1 <- key$row_data1[key$event=="AQUATHON"]
    row2 <- key$row_data2[key$event=="AQUATHON"]
    skip <- row1 - 1
    nrows <- row2 - row1 + 1
    aquathon_orig <- read.fwf(file=filename,
                          widths=col_widths,
                          header=F, skip=skip, nrows=nrows, col.names=col_names, as.is=T)
  
    # Format data
    aquathon <- aquathon_orig %>%
      # Trim white space
      mutate(name=trimws(name),
             city=trimws(city),
             state=trimws(state),
             sw_time=trimws(sw_time),
             rn_time=trimws(rn_time),
             time=trimws(time),
             dplace=trimws(dplace),
             # Format gender/age columns
             gender=ifelse(grepl("F", age), "F", "M"),
             age=as.numeric(gsub("F", "", age)),
             # Add date/event columns
             event="Aquathon",
             date=date) %>%
      select(date, event, place, bib, name, age, gender, everything())
    
  }
  
  # Merge data
  ############################################
  
  # Which races happened?
  events_happening <- sapply(c("run5km", "swim500m", "swim1km", "swim2km", "aquathon"), function(x) exists(x))
  events_happening <- names(events_happening[events_happening==T])
  df_list <- mget(events_happening)

  
  # Merge data
  data <- plyr::rbind.fill(df_list)
  
  # Return
  return(data)

  
}

# Functions
################################################################################

# Loop through and merge
files <- file.path(datadir, list.files(datadir, recursive=T))

for(i in 1:length(files)){
  
  file_do <- files[i]
  data <- format_data(file_do)
  if(i==1){data1 <- data}else{data1 <- rbind(data1, data)}
  
}

# Format time function
# x <- data2$time
format_time <- function(x){
  
  # Format times
  time_df <- tibble(time_orig=x) %>% 
    # Calculate number of characters
    mutate(nchar=nchar(time_orig)) %>% 
    #arrange(desc(nchar)) %>% # only when testing!
    # Convert to HH:MM:SS character
    mutate(time_char=ifelse(nchar==7, paste0("0", time_orig), 
                       ifelse(nchar==5, paste0("00:", time_orig),
                              ifelse(nchar==4, paste0("00:0", time_orig), NA)))) %>% 
    # Convert to lubridate time object (HMS) and POSIX time object
    mutate(#time_hms=hms(time_char), 
           time_posix=as.POSIXct(strptime(time_char, format="%H:%M:%S")))
  
  # Return the POSIX times
  times <- time_df$time_posix
  return(times)

}

# Final formatting
data2 <- data1 %>% 
  arrange(date, event, place) %>% 
  # Add season column
  mutate(season=as.numeric(year(date)), 
         time=format_time(time),
         sw_time=format_time(sw_time),
         rn_time=format_time(rn_time)) %>% 
  select(season, everything())

# Export
saveRDS(data2, file=file.path(outdir, "nite_moves_data.Rds"))


# Play with plotting
################################################################################

# Subset and reshape data for plotting
sdata <- data2 %>% 
  filter(event=="Aquathon" & season==2019) %>% 
  select(season, date, name, time, sw_time, rn_time) %>% 
  gather(key="segment", value="time", 4:6) %>% 
  mutate(segment=recode(segment, 
                        "time"="Overall", 
                        "rn_time"="Run", 
                        "sw_time"="Swim"))

# Plot data
g <- ggplot(sdata, aes(x=date, y=time, group=name)) +
  # Add all racers
  geom_line(color="grey80") +
  # Add season trend
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ segment, ncol=1, scale="free") +
  # Labels
  labs(x="", y="") +
  theme_bw()
g





