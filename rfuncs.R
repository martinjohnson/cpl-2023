# Includes list of functions to summarise the cricket scores

tab_batting <- function(d1, inn){
d2 <- d1 %>% filter(Innings == inn)

d3 <- d2 %>%
  filter(Type == 'Batting') %>%
  select(Batter,how_out,Bowler,Runs, Balls, `4s`,`6s`) %>%
  #mutate(`Runs Scored` = paste0(Runs, ' (',Balls,')')) %>%
  rename('How Out '= 'how_out') %>%
  mutate_all(~replace_na(., ''))

out <- d3 %>%
  kbl() %>%
  kable_styling(bootstrap_options = "striped" , full_width = TRUE)

return(out)
}

tab_bowling <- function(d1, inn){

d2 <- d1 %>% filter(Innings == inn)

d4 <- d2 %>%
  filter(Type == 'Bowling') %>%
  filter(flag !='') %>%
  #filter(!is.na(flag)) %>%
  select(Batter,how_out,Bowler,Runs, Balls, `4s`,`6s`) %>%
  mutate_all(~replace_na(., ''))
names(d4) <- c('Bowler',          'Overs' , 'Maiden' ,'Runs',  'Wickets', 'Wides', 'NB')

out <- d4 %>%
  kbl() %>%
  kable_styling(bootstrap_options = "striped" ,full_width = TRUE)

return(out)
}

tab_output <- function(df) {

  df %>%
    kbl() %>%
    kable_styling(bootstrap_options = "striped", full_width = TRUE)
}



lb_bat <- function(i){
  d1 <- readxl::read_excel('./ScoreCards.xlsx', sheet = i)

  bat_1 <- d1 %>%
    filter(Type == 'Batting') %>%
    filter(flag < 99) %>%
    filter(!is.na(flag)) %>%
    filter(how_out != 'DNB') %>%
    select(Match, Innings, Batter,Runs, Balls, `4s`,`6s`) %>%
    #  mutate_all(~replace_na(., '')) %>%
    rename('Player' = 'Batter')%>%
    mutate(Runs = as.numeric(Runs)) %>%
    mutate(Balls = as.numeric(Balls)) %>%
    mutate(`4s` = as.numeric(`4s`)) %>%
    mutate(`6s` = as.numeric(`6s`))

  return(bat_1)
}

lb_bowl <- function(i){
  d1 <- readxl::read_excel('./ScoreCards.xlsx', sheet = i)
  bowl_1 <- d1 %>%
    filter(Type == 'Bowling') %>%
    filter(flag < 99) %>%
    filter(flag !='') %>%
    filter(!is.na(flag)) %>%
    select(Match, Innings, Batter,how_out, Bowler, Runs,  Balls, `4s`, `6s`) %>%
    mutate(Overs = ceiling(as.numeric(how_out))) %>%
    mutate(Maidens = as.numeric(Bowler)) %>%
    mutate(Runs = as.numeric(Runs)) %>%
    mutate(Wickets = as.numeric(Balls)) %>%
    mutate(Wides = as.numeric(`4s`)) %>%
    mutate(NB = as.numeric(`6s`)) %>%
    select('Match','Innings', 'Batter', 'Overs',   'Maidens', 'Runs', 'Wickets', 'Wides', 'NB') %>%
    rename('Player' = 'Batter')

  return(bowl_1)
}
