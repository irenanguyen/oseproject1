library(dplyr)
head(neededdata)
#1 I.	Zjistěte kolik škol, je v každém kraji
neededdata %>%
  group_by(kraj) %>%
  summarise(totalschool = n()) %>%
  arrange(totalschool)


