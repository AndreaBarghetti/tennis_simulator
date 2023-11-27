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
    
    play_game(set=set,p=p)
    
    # tiebreak
    if (set$p1_games==games_to_win & set$p2_games==games_to_win) {
      env_poke(set,"tiebreak",T)
      break
    }
    
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

play_match <- function(p=.5, #probability of winning a single point for player1
                       sets_to_win=3,
                       games_to_win=6) {
  
  match <- start_match()
  
  while (match$p1_set<sets_to_win & 
         match$p2_set<sets_to_win 
  ) {
    play_set(match = match,
             p=p, 
             games_to_win=games_to_win)
  }
  
  env_get_list(match, nms = c("p1_set","p2_set"))  %>% 
    unlist()
}

# play a match ! ####

play_match(sets_to_win = 3,
           games_to_win = 6,
           p = .5)


# Visualization
# Probaility of winning a match given the probability of winning a point
res = map_dfr(seq(0.3,.7,length.out=100), function(p) {
  res = replicate(100,suppressMessages(play_match(sets_to_win = 3,
                                                  games_to_win = 6,
                                                  p = p))) %>% 
    apply(1,function(x) {mean(x==3)}) 
  c(p=p,res)
})

mod = glm(data = res,
          p1_set ~ p, 
          family=binomial(link = 'probit'),
          weights = rep(100,100))

plot = res %>% 
  mutate(p1 = predict(mod, type="response")) %>% 
  ggplot(aes(x=p)) +
  geom_line(aes(y=p1), size=1) +
  geom_point(aes(y=p1_set), size=.5) +
  theme_linedraw() +
  labs(x='Probability of winning a point',
       y="Probability of winning the match")

ggsave(plot = plot, device = "png",
       width = 10,height = 10,
       units = 'cm',
       filename = "tennis_p.png")
