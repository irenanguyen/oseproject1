library(dplyr)
head(neededdata)
#task 1 Zjistěte kolik škol, je v každém kraji

task_one <- neededdata %>%
  group_by(kraj) %>%
  summarise(totalschool = n()) %>%
  arrange(totalschool)

View(task_one)
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
                                     neededdata$kraj == "Moravskoslezský" ~ "Moravskoslezsko",TRUE ~ "N/A"))
print(neededdata)
View(task_one)

#task 4

pocet_zaku_za_kraj <- aggregate(neededdata$pocet_zaku ~ neededdata$kraj, data = neededdata, FUN = sum)
colnames(pocet_zaku_za_kraj) <- c("Kraj", "Pocet zaku")
View(pocet_zaku_za_kraj)  

pocet_zaku_za_NUTS2 <- aggregate(neededdata$pocet_zaku ~ neededdata$NUTS2, data = neededdata, FUN = sum)
colnames(pocet_zaku_za_NUTS2) <- c("NUTS2", "Pocet zaku")
View(pocet_zaku_za_NUTS2)
