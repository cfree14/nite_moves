
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
filename <- "data/results/raw/Nite Moves 5_1_19.txt"
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

  # Determine number of characters associated with each column
  # This is necessary for reading in the file using fixed width columns
  col_names <- c("place", "bib", "name", "age", "city", "state", "time", "pace", "gplace", "dplace")
  # header_row <- key$row_head[key$event=="5K RUN"]
  # header_text <- text[header_row]
  # header_text1 <- gsub(" ", "_", header_text)
  # header_nchar <- nchar(header_text)
  # 
  # # Format #1: names as Xs and spaces as _s
  # header_text2 <- gsub("\\S", "X", header_text)
  # header_text2 <- gsub(" ", "_", header_text2)
  # space_blocks <- unlist(strsplit(header_text2, "X"))
  # space_blocks <- space_blocks[space_blocks!=""]
  # 
  # # Count spaces to add
  # nchar_spaces <- nchar(space_blocks)
  # nchar_spaces1 <- c(sum(nchar_spaces[1:2]), nchar_spaces[3:length(nchar_spaces)], 0) # try 1
  # nchar_spaces1 <- c(nchar_spaces[1], sum(nchar_spaces[2:3]), nchar_spaces[4:length(nchar_spaces)], 0) # try 2
  # # nchar_spaces1 <- c(nchar_spaces[1:2], sum(nchar_spaces[3:4]), nchar_spaces[5:length(nchar_spaces)], ) # try 3
  # # length(nchar_spaces1)
  # 
  # # Format #2: isolate titles
  # name_blocks <- unlist(strsplit(header_text, " "))
  # name_blocks <- name_blocks[name_blocks!=""]
  # 
  # # Data width key
  # key1 <- data.frame(name=col_names,
  #                    name_orig=name_blocks,
  #                    nchar_name=nchar(name_blocks),
  #                    nchar_spaces=nchar_spaces1, stringsAsFactors = F) %>% 
  #   mutate(nchar = nchar_name + nchar_spaces)

  # Column widths
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
  
  
  # Format 1km swim
  ############################################

  # Determine number of characters associated with each column
  # This is necessary for reading in the file using fixed width columns
  col_names <- c("place", "bib", "name", "age", "city", "state", "time", "gplace", "dplace")
  header_row <- key$row_head[key$event=="1K SWIM"]
  header_text <- text[header_row]
  header_nchar <- nchar(header_text)
  # headers <- unlist(strsplit(header_text, "[**UPPER**]")) # WORK ON THIS
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


  # Format 2km swim
  ############################################

  # Determine number of characters associated with each column
  # This is necessary for reading in the file using fixed width columns
  col_names <- c("place", "bib", "name", "age", "city", "state", "time", "gplace", "dplace")
  header_row <- key$row_head[key$event=="2K SWIM"]
  header_text <- text[header_row]
  header_nchar <- nchar(header_text)
  # headers <- unlist(strsplit(header_text, "[**UPPER**]")) # WORK ON THIS
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

  # Format aquathon
  ############################################

  # Column names
  col_names <- c("place", "bib", "name", "age", "city", "state", "sw_time",
                 "sw_place", "rn_time", "rn_place", "time", "gplace", "dplace")

  # Determine number of characters associated with each column
  # This is necessary for reading in the file using fixed width columns
  header_row <- key$row_head[key$event=="AQUATHON"]
  header_text <- text[header_row]
  header_nchar <- nchar(header_text)
  # headers <- unlist(strsplit(header_text, "[**UPPER**]")) # WORK ON THIS
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
  
  # Merge data
  ############################################
  
  # Merge data
  data <- plyr::rbind.fill(run5km, swim1km, swim2km, aquathon)
  
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

# Final formatting
data2 <- data1 %>% 
  arrange(date, event, place) %>% 
  mutate(time=ms(time),
         pace=ms(pace),
         sw_time=ms(sw_time),
         rn_time=ms(rn_time))

# Export
saveRDS(data2, file=file.path(outdir, "nite_moves_data.Rds"))


# Test plot
################################################################################

# 
tm <- filter(data2, name=="Tracey Mangin")


g <- ggplot(tm, aes(x=date, y=minute(time))) + 
  geom_line() + 
  theme_bw()
g
str(tm)







