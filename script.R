library(sf)
library(fs)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gt)


# Bring in the raw data. The rds data is easy because we can just read it in
# directly.
hartford <- read_rds(url("https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ct_hartford_2019_02_25.rds"))

# The shape file is trickier because it is compressed on the webpage.
download.file(url = "https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ct_hartford_shapefiles_2019_02_25.tgz", destfile = "shapes.tgz", 
              quiet = TRUE)

# unzip the file
untar("shapes.tgz")

shapes_data <- read_sf("ct_hartford_shapefiles/Hartford_Neighborhoods.shp")

file_delete(c("shapes.tgz", "ct_hartford_shapefiles/"))


# Who?: Race and Gender
##
##


# looking for percent arrests by race and gender
race_gender <- hartford %>%
  select(subject_sex, subject_race, arrest_made) %>%
  # want arrests made percentage, so group_by var first
  group_by(arrest_made) %>% 
  # subsections of sex and race to get subcategories in chart
  group_by(subject_sex, subject_race) %>%
  mutate(total = n()) %>%
  # filter out arrests to make arrest variable
  filter(arrest_made == FALSE) %>%
  # gets the percentage of arrests by calculated arrests / total stops ("total" variable)
  mutate(arrest = total - n()) %>%
  mutate(freq = arrest/total) %>%
  summarize(arrest_rate = mean(freq)) %>%
  # makes the sex per each race displayed horizontally rather than as different rows
  spread(key = subject_sex, value=arrest_rate) %>% 
  # renames the row labels for the graph
  mutate(subject_race = fct_recode(subject_race,
                                   "White" = "white",
                                   "Asian/Pacific Islander" = "asian/pacific islander",
                                   "Black" = "black",
                                   "Hispanic" = "hispanic",
                                   "Other or Unknown" = "other/unknown"
  ))

# create table
# percetange formatting all taken care of here
gt(race_gender) %>% 
  # fixing column names
  cols_label(
    subject_race = "Subject Race",
    male = "Male",
    female = "Female") %>% 
  # formats percentages as percents, when passed in columns in data
  fmt_percent(columns = vars(male, female)) %>% 
  # labels
  tab_header(title = "Arrest by Race and Gender",
             subtitle = "As Percentage") %>% 
  tab_source_note(source_note = 
                    "Source: Stanford Open Policing Project")

