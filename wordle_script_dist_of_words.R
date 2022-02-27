#### SETUP #####
setwd("~/Dropbox/Brown/Fun writing/Wordle/Public code")
library(tidyverse)
library(tictoc)
library(data.table)
library(stringr)
library(doMC)

# parallelization
detectCores()
doMC::registerDoMC(cores = detectCores())
foreach::getDoParWorkers()

#### LOAD DATA ####
df = read.csv("wordle.csv") 
solutions = data.table(str_split(unique(df$solutions), pattern = "", simplify = TRUE))
guesses = data.table(str_split(unique(c(df$solutions, df$herrings)), pattern = "", 
                               simplify = TRUE))
solutions_used = solutions[1:242,]
solutions_used

#### STARTING LETTER FREQUENCY ####
c = bind_rows(solutions %>% mutate(id = "All solutions"), 
              guesses %>% mutate(id = "All guesses"),
              solutions_used %>% mutate(id = "Used solutions")) %>%
  filter(V1!="") %>%
  mutate(V1 = tolower(V1), V2 = tolower(V2), V3 = tolower(V3),
         V4 = tolower(V4), V5 = tolower(V5))


fl = c %>% group_by(id, V1) %>% summarize(freq = length(V1)) %>%
  group_by(id) %>% mutate(n = sum(freq), perc = freq/n)


ggplot(fl, aes(x = reorder(V1, perc), y = perc, fill = id, group = id)) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "dodge") 

#### OVERALL LETTER FREQUENCY ####
all = c %>% gather(var, value, -id) %>% group_by(id, value) %>% summarize(freq = length(value)) %>%
  group_by(id) %>% mutate(n = sum(freq), perc = freq/n)

ggplot(all, aes(x = reorder(value, perc), y = perc, fill = id, group = id)) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "dodge") 


#### TESTS ####
