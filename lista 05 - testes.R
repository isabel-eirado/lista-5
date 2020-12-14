library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(infer)
library(readxl)


States_and_Abbreviations <- read_excel("States and Abbreviations.xlsx")
State_Constitutions_Word_Count <- read_excel("State Constitutions Word Count.xlsx", 
                                             +     sheet = "Base Tidy")


constituicoes_testehipotese <-  State_Constitutions_Word_Count %>% 
  select(state, constitution_year, current_constitution, total_words, declaration_rights, discrimination) %>% 
  left_join(States_and_Abbreviations, constituicoes_poderes, by = "state")


constituicoes_testehipotese$state <- as.factor(constituicoes_testehipotese$state)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(infer)
library(readxl)


States_and_Abbreviations <- read_excel("States and Abbreviations.xlsx")
State_Constitutions_Word_Count <- read_excel("State Constitutions Word Count.xlsx", 
                                             +     sheet = "Base Tidy")


constituicoes_testehipotese <-  State_Constitutions_Word_Count %>% 
  select(state, constitution_year, current_constitution, total_words, declaration_rights, discrimination) %>% 
  left_join(States_and_Abbreviations, constituicoes_poderes, by = "state")


constituicoes_testehipotese$state <- as.factor(constituicoes_testehipotese$state)


constituicoes_testehipotese <- constituicoes_testehipotese %>% 
  group_by(state) %>%
  mutate(media_total_words = mean(total_words)) 

constituicoes_antigas <- constituicoes_testehipotese %>% 
  filter(current_constitution == 0) %>%
  group_by(state) %>%
  mutate(media_total_words = mean(total_words)) %>% 
  select(state, media_total_words) %>% 
  group_by(state, media_total_words) %>%
  summarise(n())

media_antigas <- constituicoes_antigas$media_total_words

constituicoes_novas <- constituicoes_testehipotese %>%
  filter(current_constitution == 1) %>%
  select(state, constitution_year, total_words) 

valor_novas <- constituicoes_novas$total_words

t.test(media_antigas, valor_novas, paired = TRUE)
  

#testando com pacote office (NÃO DEU CERTO)
objetofinal <- left_join(constituicoes_antigas, constituicoes_novas, by = "state")

objetofinal %>%
  t_test(formula = total_words ~ media_total_words)
  


