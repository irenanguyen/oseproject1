library(dplyr)
head(neededdata)
#task 1 Zjistěte kolik škol, je v každém kraji

task_one <- neededdata %>%
  group_by(kraj) %>%
  summarise(totalschool = n()) %>%
  arrange(totalschool)
#task 2
task_one$kraj <- factor(task_one$kraj, levels = c("Praha", "Středočeský", "Jihočeský", "Plzeňský", "Karlovarský", "Ústecký", "Liberecký","Královéhradecký", "Pardubický", "Vysočina","Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský"))

#task 3
neededdata <- mutate(neededdata, NUTS2 = case_when(neededdata$kraj == "Praha" ~ "Praha", 
                                     neededdata$kraj =="Středočeský" ~ "Střední Čechy",
                                     neededdata$kraj =="Jihočeský" | neededdata$kraj =="Plzeňský" ~ "Jihozápad",
                                     neededdata$kraj =="Karlovarský" | neededdata$kraj =="Ústecký" ~ "Severozápad",
                                     neededdata$kraj =="Olomoucký" | neededdata$kraj =="Zlínský" ~ "Střední Morava",
                                     neededdata$kraj =="Vysočina" | neededdata$kraj =="Jihomoravský" ~ "Jihovýchod",
                                     neededdata$kraj =="Liberecký" | neededdata$kraj =="Královéhradecký" | neededdata$kraj =="Pardubický" ~ "Severovýchod",
                                     neededdata$kraj == "Moravskoslezský" ~ "Moravskoslezsko",TRUE ~ "else"))
print(neededdata)
View(task_one)


