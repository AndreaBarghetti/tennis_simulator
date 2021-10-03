library(tidyverse)
library(rlang)

# point ####
play_point <- function(game, p) {
  invisible(
    ifelse (rbinom(1,1, prob = p), 
            {
              env_poke(game, "p1_pt", game$p1_pt+1)
              },
            {
              env_poke(game, "p2_pt", game$p2_pt+1)
              })
  )
}

# Game ####
start_game <- function() {
  game <-rlang::env(
    p1_pt=0,
    p2_pt=0
  )
  game
}

play_game <- function(set,p) {
  game <- start_game()
  
  while (game$p1_pt<4 & 
         game$p2_pt<4 | 
         abs(diff(c(game$p1_pt,game$p2_pt)))<2
         ) {
    play_point(game=game, p=p)
  }
  
  ifelse(game$p1_pt>game$p2_pt,
         yes = env_poke(set, "p1_games", set$p1_games+1),
         no = env_poke(set, "p2_games", set$p2_games+1))
  
}

# Set ####
start_set <- function() {
  set <- rlang::env(
    p1_games = 0,
    p2_games = 0,
    tiebreak=F
  )
  set
}

start_tiebreak <- function() {
  tiebreak <- rlang::env(
    p1_pt = 0,
    p2_pt = 0
  )
  tiebreak
}

play_tiebreak <- function(match,p) {
  tiebreak <- start_tiebreak()

  while (tiebreak$p1_pt<7 & 
         tiebreak$p2_pt<7 | 
         abs(diff(c(tiebreak$p1_pt,tiebreak$p2_pt)))<2
  ) {
    play_point(game=tiebreak, p=p)
  }
  
  ifelse(tiebreak$p1_pt>tiebreak$p2_pt,
         yes = env_poke(match, "p1_set", match$p1_set+1),
         no = env_poke(match, "p2_set", match$p2_set+1))
  }

play_set <- function(match,p, games_to_win) {
  set <- start_set()
  
  while (set$p1_games<games_to_win & 
         set$p2_games<games_to_win | 
         abs(diff(c(set$p1_games,set$p2_games)))<2 
         ) {
    
    # tiebreak
    if (set$p1_games==games_to_win & set$p2_games==games_to_win) {env_poke(set,"tiebreak",T)}
    if (set$tiebreak) {break}
    
    play_game(set=set,p=p)
   
  }
  
  if (set$tiebreak) {
    play_tiebreak(match=match,
                  p=p)
    message("tiebreak!")
  }
  
  if(!set$tiebreak) {
    ifelse(set$p1_games>set$p2_games,
           yes = env_poke(match, "p1_set", match$p1_set+1),
           no = env_poke(match, "p2_set", match$p2_set+1))
  }
  
}


# Match ####
start_match <- function() {
  match <- rlang::env(
    p1_set = 0,
    p2_set = 0
  )
  match
}

play_match <- function(p=.5,
                       sets_to_win=3,
                       games_to_win=6) {
  p1_prob = p #probability of winning a single point for player1

  match <- start_match()
  
  while (match$p1_set<sets_to_win & 
         match$p2_set<sets_to_win 
  ) {
    play_set(match = match,p=p, games_to_win=games_to_win)
  }

  env_get_list(match, nms = c("p1_set","p2_set"))  %>% 
    unlist()
}

play_match(sets_to_win = 3,
           games_to_win = 6,
           p = .5)

lots_of_matches <- replicate(10,play_match(sets_to_win = 3,
                                            games_to_win = 6,
                                            p = .55))



t(lots_of_matches) %>%
  as_tibble() %>%
  rownames_to_column("match_n") %>%
  summarise(win = mean(p1_set>p2_set))