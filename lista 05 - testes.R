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

teste1 <- constituicoes_testehipotese %>% 
  group_by(state, abbreviation) %>%
  mutate(sum_total_words = sum(total_words), 
         sum_declaration = sum(declaration_rights), 
         sum_discrimation = sum(discrimination)) %>% 
  mutate(media_total_words = mean(total_words), 
         media_declaration = mean(declaration_rights), 
         media_discrimation = mean(discrimination)) %>% 
  mutate(desviopadrao_total_words = sd(total_words),
         desviopadrao_declaration = sd(declaration_rights),
         desviopadrao_discrimation = sd(declaration_rights)) %>%
  filter(current_constitution == 1) 

base2 <- constituicoes_testehipotese %>% 
  group_by(state, abbreviation) %>%
  mutate(sum_total_words = sum(total_words), 
         sum_declaration = sum(declaration_rights), 
         sum_discrimation = sum(discrimination)) %>% 
  mutate(media_total_words = mean(total_words), 
         media_declaration = mean(declaration_rights), 
         media_discrimation = mean(discrimination)) %>% 
  mutate(desviopadrao_total_words = sd(total_words),
         desviopadrao_declaration = sd(declaration_rights),
         desviopadrao_discrimation = sd(declaration_rights))


teste1 <- teste1 %>%
  select(abbreviation, state, sum_total_words,  media_total_words, desviopadrao_total_words,
         sum_declaration, media_declaration, desviopadrao_declaration,
         sum_discrimation, media_discrimation, desviopadrao_discrimation)

teste1$abbreviation <- as.factor(teste1$abbreviation)


#T-TEST PARA UMA AMOSTRA (COMPARAÇÃO DA MÉDIA DE UMA AMOSTRA COM A MÉDIA TOTAL)
# Comparar a média de uma amostra com a média de uma população (teste de de uma amostra)

#H0 = A média entre o número de palavras nas constituições é igual
#h1 = A média é diferente

#CALCULANDO A MÉDIA DA POPULAÇÃO 

media_total_população <- teste1$sum_total_words %>%
  mean()


#T=TEST COMPARANDO A MÉDIA DAS OBSERVAÇÕES COM A MÉDIA DA POPULAÇÃO TOTAL
t_test_total_mean <-teste1 %>%
  t_test(formula = media_total_words ~ NULL, mu = media_total_população) #USANDO INFER

#H0 = SEM O ALABAMA, A MEDIAS DE PALAVRAS É IGUAIS  
#H1 = MESMO COM O ALABAMA, A MÉDIA DA VARIAÇÃO ENTRE AS PALAVRAS É IGUAL

base_sem_alabama<- teste1 %>% 
  filter(state != "Alabama")

media_total_sem_alabama <- t_test_sem_alabama$sum_total_words %>%
  mean()

t_test_sem_alabama <- base_sem_alabama %>%
  t_test(formula = media_total_words ~ NULL, mu = media_total_sem_alabama)


#T-TEST COMPARAR MÉDIAS DA VARIAÇÃO DE DISCRIMINATION E DECLARATON 
media_discrimation <- teste1$sum_discrimation 
media_declaration <- teste1$sum_declaration 

t.testepareado <- t.test(media_declaration, media_discrimation, paired = TRUE)
# O valor do p é muito pequeno. 


