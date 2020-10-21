library(tidyverse)
library(ggplot2)
signup_rank <- read.csv("Industry Rank .csv")


## A table of ppl's first choice
head(signup_rank)
choice1 <- signup_rank %>% select(Response.., Industry.choice.1) %>% 
  group_by(Industry.choice.1) %>% 
  arrange(Industry.choice.1) %>% 
  as_tibble() %>% 
  rename(Industry = Industry.choice.1)
choice1 <- choice1 %>% mutate(score = 3)

## A table of ppl's second choice
choice2 <- signup_rank %>% select(Response.., Industry.choice.2) %>% 
  group_by(Industry.choice.2) %>% 
  arrange(Industry.choice.2) %>% 
  as_tibble() %>% 
  rename(Industry = Industry.choice.2)
choice2 <- choice2 %>% mutate(score = 2)


## A table of people's third choice
choice3 <- signup_rank %>% select(Response.., Industry.Choice.3) %>% 
  group_by(Industry.Choice.3) %>% 
  arrange(Industry.Choice.3) %>% 
  as_tibble() %>% 
  rename(Industry = Industry.Choice.3) %>% 
  mutate(score = 1) 


## Row_bind 3 tables together
preference <- rbind(choice1,choice2, choice3)

##See the result of preference 
result <- aggregate(preference$score, 
                    by = list(Industry = preference$Industry), 
                    FUN = sum) %>% 
  arrange(desc(x)) %>% 
  rename(preference_score = x)
View(result)
