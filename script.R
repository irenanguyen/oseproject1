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
pocet_zaku_za_kraj <- pocet_zaku_za_kraj %>% arrange(desc(`Pocet zaku`))
View(pocet_zaku_za_kraj)  
#graph 4a
pocet_zaku_za_kraj %>% ggplot(aes(Kraj, `Pocet zaku`, fill = Kraj))+
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Kraj", y = "Počet žáku za kraj")

# task4 - b
pocet_zaku_za_NUTS2 <- aggregate(neededdata$pocet_zaku ~ neededdata$NUTS2, data = neededdata, FUN = sum)
colnames(pocet_zaku_za_NUTS2) <- c("NUTS2", "Pocet zaku")
pocet_zaku_za_NUTS2 <- pocet_zaku_za_NUTS2 %>% arrange(desc(`Pocet zaku`))
View(pocet_zaku_za_NUTS2)
#graph 4b
pocet_zaku_za_NUTS2 %>% ggplot(aes(NUTS2, `Pocet zaku`, fill = NUTS2))+
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Kraj", y = "Počet žáku za NUTS2")

#task 5 - a 

KRAJpocet_zaku_za_1asistent <- neededdata %>%
  group_by(kraj) %>%
  summarise(total_asistent = sum(pocet_asistentu), total_student = sum(pocet_zaku)) %>%
  mutate(zak.na.jeden.asistent = total_student / total_asistent) %>%
  arrange(desc(zak.na.jeden.asistent))

#graph 5a

# delete N/A

KRAJpocet_zaku_za_1asistent <- KRAJpocet_zaku_za_1asistent[-15,]
task_one <- task_one[-1,]

#task 5 - b

NUTS2pocet_zaku_za_1asistent <- neededdata %>%
  group_by(neededdata$NUTS2) %>%
  summarise(total_asistent = sum(pocet_asistentu), total_student = sum(pocet_zaku)) %>%
  mutate(zak.na.jeden.asistent = total_student / total_asistent) %>%
  arrange(desc(zak.na.jeden.asistent))
#delete N/A
NUTS2pocet_zaku_za_1asistent <- NUTS2pocet_zaku_za_1asistent [-9,]
# change col names
colnames(NUTS2pocet_zaku_za_1asistent) <- c("NUTS2", "Pocet asistentu", "pocet studentu", "pocet zaku za jeden asistent")
#graph 5b


ls()
remove(pocet_zaku_za_1asistent)
remove(task_four_a)
ls()

#task6a - KRAJ

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

#task 6a- NUTS2

NUTS2_NOPSYCH_total_student <- neededdata %>%
  group_by(NUTS2) %>%
  filter(psycholog == "ne") %>%
  summarise(total.student.no.psy = sum(pocet_zaku)) %>%
  arrange(desc(total.student.no.psy))
colnames(NUTS2_NOPSYCH_total_student) <- c("NUTS2", "pocet zaku bez psycholog")

NUTS2_YESPSYCH_total_student <- neededdata %>%
  group_by(NUTS2) %>%
  filter(psycholog == "ano") %>%
  summarise(total.student.with.psy = sum(pocet_zaku)) %>%
  arrange(desc(total.student.with.psy))

NUTS2_PSYch_Pocet_studentu <- left_join(NUTS2_YESPSYCH_total_student, NUTS2_NOPSYCH_total_student, by = c('NUTS2'))
colnames(NUTS2_PSYch_Pocet_studentu) <- c("NUTS2", "students have psychologs", "students without psychologs")

