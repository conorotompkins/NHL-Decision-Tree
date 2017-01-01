
library(tidyverse)
library(FFTrees)
library(intubate)

df <- read_csv("https://raw.githubusercontent.com/conorotompkins/NHL-Coaches/master/NHH_coach_data.csv") %>%
        mutate(season = as.character(season)) %>%
  filter(TOI >= 10)

#20052006 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2006.html
playoff_0506_data <- read.delim("clipboard")

set_season <- "20052006"

playoffs_0506 <- playoff_0506_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_0506$full_team_names[playoffs_0506$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_0506, paste(set_season, "playoffs.csv"))

#20062007 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2007.html
playoff_0607_data <- read.delim("clipboard")

set_season <- "20062007"

playoffs_0607 <- playoff_0607_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_0607$full_team_names[playoffs_0607$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_0607, paste(set_season, "playoffs.csv"))

#20072008 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2008.html
playoff_0708_data <- read.delim("clipboard")

set_season <- "20072008"

playoffs_0708 <- playoff_0708_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_0708$full_team_names[playoffs_0708$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_0708, paste(set_season, "playoffs.csv"))

#20082009 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_0809_data <- read.delim("clipboard")

set_season <- "20082009"

playoffs_0809 <- playoff_0809_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_0809$full_team_names[playoffs_0809$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_0809, paste(set_season, "playoffs.csv"))

#20092010 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_0910_data <- read.delim("clipboard")

set_season <- "20092010"

playoffs_0910 <- playoff_0910_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_0910$full_team_names[playoffs_0910$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_0910, paste(set_season, "playoffs.csv"))

#20102011 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_1011_data <- read.delim("clipboard")

set_season <- "20102011"

playoffs_1011 <- playoff_1011_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_1011$full_team_names[playoffs_1011$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1011, paste(set_season, "playoffs.csv"))

#20112012playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_1112_data <- read.delim("clipboard")

set_season <- "20112012"

playoffs_1112 <- playoff_1112_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_1112$full_team_names[playoffs_1112$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1112, paste(set_season, "playoffs.csv"))

#20122013playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_1213_data <- read.delim("clipboard")

set_season <- "20122013"

playoffs_1213 <- playoff_1213_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_1213$full_team_names[playoffs_1213$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1213, paste(set_season, "playoffs.csv"))

#20132014playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_1314_data <- read.delim("clipboard")

set_season <- "20132014"

playoffs_1314 <- playoff_1314_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_1314$full_team_names[playoffs_1314$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1314, paste(set_season, "playoffs.csv"))

#20142015playoffs
#http://www.hockey-reference.com/playoffs/NHL_2009.html
playoff_1415_data <- read.delim("clipboard")

set_season <- "20142015"

playoffs_1415 <- playoff_1415_data %>%
  mutate(full_team_names = as.character(Team),
         playoff = TRUE,
         season = set_season) %>%
  select(full_team_names, season, playoff) %>%
  as_tibble()

playoffs_1415$full_team_names[playoffs_1415$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1415, paste(set_season, "playoffs.csv"))



#20152016 playoffs
#http://www.hockey-reference.com/playoffs/NHL_2016.html
playoff_1516_data <- read.delim("clipboard")

set_season <- "20152016"

playoffs_1516 <- playoff_1516_data %>%
        mutate(full_team_names = as.character(Team),
               playoff = TRUE,
               season = "20152016") %>%
        select(full_team_names, season, playoff)

playoffs_1516$full_team_names[playoffs_1516$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

write_csv(playoffs_1516, paste(set_season, "playoffs.csv"))


#combine playoff data files
getwd()
my_list <- list.files(getwd(), "playoffs")

my_data <- lapply(my_list, read_csv)
my_data_df <- bind_rows(my_data) %>%
  mutate(season = as.character(season))

my_data_df$full_team_names[my_data_df$full_team_names == "Mighty Ducks of Anaheim"] <- "Anaheim Ducks"

#combine full team data with playoff data

test <- df %>%
        select(season, full_team_names, team, CF60, CA60, OSh.per, OSv.per, FO.per, PN, PN.) %>%
        left_join(., my_data_df, by = c("full_team_names", "season")) %>%
        group_by(team, season, playoff) %>%
        summarize(CF60 = mean(CF60, na.rm = TRUE),
                  CA60 = mean(CA60, na.rm = TRUE),
                  OSh.per = mean(OSh.per, na.rm = TRUE),
                  OSv.per = mean(OSv.per, na.rm = TRUE),
                  FO.per = mean(FO.per, na.rm = TRUE),
                  PN = sum(PN, na.rm = TRUE),
                  PN. = sum(PN., na.rm = TRUE)) %>%
        ungroup() %>%
  arrange(team, season)

test$playoff[is.na(test$playoff)] <- FALSE

ffplayoff<- test %>%
        select(-c(team, season)) %>%
        ntbt(FFTrees, playoff ~ .)

plot(ffplayoff, 
     main = "NHL Playoffs 2005-2016 (5v5)", decision.names = c("Is Not Playoff Team", "Is a Playoff Team"))