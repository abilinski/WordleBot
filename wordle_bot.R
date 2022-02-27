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
guesses = data.table(str_split(unique(c(df$solutions, df$herrings)), pattern = "", simplify = TRUE))

#### FUNCTIONS #####

# check matches
whittle = function(current_solution, current_guess, current_list){
  
  # find green squares
  green.pos = which(current_solution==current_guess)
  for(i in green.pos){
    current_list = current_list[current_list[[i]]==current_solution[[i]],]
  }
  
  # find yellow squares
  yellow.pos = which(current_guess%in%current_solution & current_solution!=current_guess)
  for(i in yellow.pos){
    
    # force yellow squares to be in list
    current_list = current_list[V1==current_guess[[i]] | V2==current_guess[[i]] | 
                                V3==current_guess[[i]] | V4==current_guess[[i]] | 
                                V5==current_guess[[i]] ,]

    # but not in their current position
    current_list = current_list[current_list[[i]]!=current_guess[[i]],]
  }
  
  # reduce down red squares
  red.pos = which(!current_guess%in%current_solution)
  for(i in red.pos){
    no = current_guess[[i]]
    current_list = current_list[V1!=no & V2!=no & V3!=no & V4!=no & V5!=no ,]
  }
  
  # return list
  return(current_list)
}

# whittle down the list manually
whittle_manual = function(green.pos = NULL, green = NULL, 
                          yellow.pos = NULL, yellow = NULL, 
                          red = NULL, current_list){
  
  # find green squares
  for(i in green.pos){
    current_list = current_list[current_list[[i]]==green[green.pos==i],]
  }
  
  # find yellow squares
  for(i in yellow.pos){
    
    # force yellow squares to be in list
    current_list = current_list[V1==yellow[yellow.pos==i] | V2==yellow[yellow.pos==i] | 
                                  V3==yellow[yellow.pos==i] | V4==yellow[yellow.pos==i] | 
                                  V5==yellow[yellow.pos==i] ,]
    
    # but not in their current position
    current_list = current_list[current_list[[i]]!=yellow[yellow.pos==i],]
  }
  
  # find red squares
  for(i in red){
    current_list = current_list[V1!=i & V2!=i & V3!=i & V4!=i & V5!=i ,]
  }
  
  # return list
  return(current_list)
}

# check 1-step forward set for a given set of guesses and solutions
check_set = function(guesses, solutions, current_list = NULL, keep = F){
  if(is.null(current_list)) current_list = solutions
  full_mat = data.table(expand_grid(g = 1:nrow(guesses), s = 1:nrow(solutions)))
  tic()
  out = foreach(i=1:nrow(full_mat)) %dopar% {
    
      # set current word and solution
      current_solution = solutions[full_mat$s[i],]
      current_guess = guesses[full_mat$g[i],]
      
      # update list
      new_list = whittle(current_solution, current_guess, current_list)
      #current_list = new_list
      
      if(keep) write.table(new_list, file = paste0("list_", full_mat$g[i], ".csv"),
                         append = TRUE, sep = ",")

      # save dimensionality
      return(nrow(new_list))
  }
  toc()
  
  return(full_mat %>% mutate(set = unlist(out)))
}

#### BEST START #####
# takes a few hours to run
opts = check_set(guesses, solutions)

#### COMPARE COMMON OPENERS ####
opener_list = sort(c("stare", "stear", "adieu", "adios", "aeros", "soare", "saree",
                     "sooey", "soree", "saine", "seare", "roate", "share", "shear",
                     "tears", "tares", "teams", "arise", "arose"))

# compile list of starting guesses
guess_set = data.table(str_split(opener_list,
                                  pattern = "", simplify = TRUE))

# check remaining set after first guess
opts = check_set(guess_set, solutions, current_list = solutions, keep = F) 
report = opts %>% group_by(g) %>% summarize(mean = mean(set), median = median(set), 
                                   q25 = quantile(set, .25), q75 = quantile(set, .75)) %>%
  mutate(g = opener_list) %>% arrange(mean)

#### GROUT VS. TROUT #####
# given R in wrong position from starting STARE
gt = c("grout", "trout")
guess_set = data.table(str_split(gt, pattern = "", simplify = TRUE))
solution_set = whittle_manual(yellow = c("t", "r"), yellow.pos = c(2,4), red = c("s", "e", "a"),
                              current_list = solutions)

opts2 = check_set(guess_set, solution_set, keep = T)
report2 = opts2 %>% group_by(g) %>% summarize(mean = mean(set), median = median(set), 
                                             q25 = quantile(set, .25), q75 = quantile(set, .75)) %>% 
  mutate(g = gt)
report2

# with robot
solution_set2 = data.table(str_split("robot", pattern = "", simplify = TRUE))
opts2 = check_set(guess_set, solution_set2, current_list = solution_set, keep = T)

#### PRICK BOARD #####
solution_set = whittle_manual(yellow = c("p"), yellow.pos = c(5), red = c("s", "t", "e", "a"),
                              green = "r", green.pos = 2,
                              current_list = solutions)

solution_set = whittle_manual(green = c("p", "i"), green.pos = c(1,3),
                              red = c("a", "d", "e", "u", "o", "n", "t"),
                              current_list = solutions)

#### PERKY BOARD ####
solution_set_N = whittle_manual(green = c("s", "a", "r"), green.pos = c(1,3,4),
                                red = c("e", "c"),
                              current_list = solutions)
guess_set_N = whittle_manual(green = c("s", "a", "r"), green.pos = c(1,3,4),
                             red = c("e", "c"),
                                current_list = guesses)

solution_set_N = whittle_manual(yellow = c("t", "a"), yellow.pos = c(2,3),
                                current_list = solutions, red = c("s", "r", "e"))
guess_set_N = whittle_manual(yellow = c("t", "a"), yellow.pos = c(2,3),
                             current_list = guesses, red = c("s", "r", "e"))

guess_set_N_collapse = sapply(1:nrow(guess_set_N), function(a) paste(guess_set_N[a,], sep = "", collapse = ""))

opts2 = check_set(guess_set_N, solution_set_N, keep = F)
out = opts2 %>% group_by(g) %>% summarize(mean = mean(set), median = median(set), 
                                    q25 = quantile(set, .25), q75 = quantile(set, .75)) %>% 
  mutate(g = guess_set_N_collapse) %>% arrange(mean)
View(out)
