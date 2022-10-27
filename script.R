library(dplyr)
library(forcats)
library(ggplot2)
head(neededdata)
#ggplot2, forcat, dplyr
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
                                     neededdata$kraj == "Moravskoslezský" ~ "Moravskoslezsko"))
print(neededdata)
View(task_one)

#task 4

pocet_zaku_za_kraj <- aggregate(neededdata$pocet_zaku ~ neededdata$kraj, data = neededdata, FUN = sum)
colnames(pocet_zaku_za_kraj) <- c("Kraj", "Pocet zaku")
View(pocet_zaku_za_kraj)  
#visualization task 4 a
pocet_zaku_za_kraj %>% ggplot(aes(Kraj, `Pocet zaku`, fill = Kraj))+
  geom_bar(stat="identity", position="dodge")

pocet_zaku_za_NUTS2 <- aggregate(neededdata$pocet_zaku ~ neededdata$NUTS2, data = neededdata, FUN = sum)
colnames(pocet_zaku_za_NUTS2) <- c("NUTS2", "Pocet zaku")
View(pocet_zaku_za_NUTS2)
#visualization task 4 b
pocet_zaku_za_NUTS2 %>% ggplot(aes(NUTS2, `Pocet zaku`, fill = NUTS2))+
  geom_bar(stat="identity", position="dodge")

#task 5 - a kolik žáků připadá na jednoho*u asistentka*u pedagoga*žky za kraj

KRAJpocet_zaku_za_1asistent <- neededdata %>%
  group_by(kraj) %>%
  summarise(total_asistent = sum(pocet_asistentu), total_student = sum(pocet_zaku)) %>%
  mutate(zak.na.jeden.asistent = total_student / total_asistent)

KRAJpocet_zaku_za_1asistent <- KRAJpocet_zaku_za_1asistent[-15,]
task_one <- task_one[-1,]

#task 5 - a kolik žáků připadá na jednoho*u asistentka*u pedagoga*žky za NUTS2
NUTS2pocet_zaku_za_1asistent <- neededdata %>%
  group_by(neededdata$NUTS2) %>%
  summarise(total_asistent = sum(pocet_asistentu), total_student = sum(pocet_zaku)) %>%
  mutate(zak.na.jeden.asistent = total_student / total_asistent)

ls()
remove(pocet_zaku_za_1asistent)
remove(task_four_a)
ls()

#task6

KRAJ_NOPSYCH_total_student <- neededdata %>%
  group_by(kraj) %>%
  filter(psycholog == "ne") %>%
  summarise(total.student.no.psy = sum(pocet_zaku))

KRAJ_YESPSYCH_total_student <- neededdata %>%
  group_by(kraj) %>%
  filter(psycholog == "ano") %>%
  summarise(total.student.with.psy = sum(pocet_zaku))

KRAJ_PSYch_Pocet_studentu <- left_join(KRAJ_YESPSYCH_total_student, KRAJ_NOPSYCH_total_student, by = c('kraj'))
colnames(KRAJ_PSYch_Pocet_studentu) <- c("Kraj", "students have psychologs", "students without psychologs")

NUTS2_NOPSYCH_total_student <- neededdata %>%
  group_by(NUTS2) %>%
  filter(psycholog == "ne") %>%
  summarise(total.student.no.psy = sum(pocet_zaku))

NUTS2_YESPSYCH_total_student <- neededdata %>%
  group_by(NUTS2) %>%
  filter(psycholog == "ano") %>%
  summarise(total.student.with.psy = sum(pocet_zaku))

NUTS2_PSYch_Pocet_studentu <- left_join(NUTS2_YESPSYCH_total_student, NUTS2_NOPSYCH_total_student, by = c('NUTS2'))
colnames(NUTS2_PSYch_Pocet_studentu) <- c("NUTS2", "students have psychologs", "students without psychologs")

