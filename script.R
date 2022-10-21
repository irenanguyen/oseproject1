library(dplyr)
head(neededdata)
#task 1 Zjistěte kolik škol, je v každém kraji
neededdata %>%
  group_by(kraj) %>%
  summarise(totalschool = n()) %>%
  arrange(totalschool)


