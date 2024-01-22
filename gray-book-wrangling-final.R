#######################
# using Challen's data#
#######################
library(tidyverse)
d0 <- read_csv("https://github.com/gchallen/graybooker/raw/main/output/results.csv.gz")

#unique names only if viewing name and title
d2021 <- d0 %>%
  filter(Year==2021) #20178 rows

d2021_0 <- d2021 %>%
  mutate(NameTitle=paste(str_remove_all(Name,"\\s"),str_replace_all(Title,"\\s","_"),sep="_"),
         ID2=openssl::md5(NameTitle)) %>% #20178 successful unique ID
  group_by(Name) %>%
  mutate(ID1=openssl::md5(Name))

d2021_1 <- d2021_0 %>%
  ungroup(Name) %>%
  filter(str_detect(Location,"Urbana")) %>%
  mutate(Acronym = case_when(Category == "Carle Illinois Medicine" ~ "CIMED",
                   Category == "Agr, Consumer, & Env Sciences" ~ "ACES",
                   Category == "Applied Health Sciences" ~ "AHS",
                   Category == "Education" ~ "EDUC",
                   Category == "Fine & Applied Arts" ~ "FAA",
                   Category == "Law" ~ "LAW",
                   Category == "Liberal Arts & Sciences" ~ "LAS",
                   Category == "Veterinary Medicine" ~ "VetMed",
                   Category == "Gies College of Business" ~ "BUS",
                   Category == "Grainger Engineering" ~ "GCOE",
                   Category == "School of Social Work" ~ "SSW",
                   Category == "School of Information Sciences" ~ "iSchool",
                   Category == "School of Labor & Empl. Rel." ~ "LER",
                   Category == "Graduate College" ~ "GC",
                   Category == "Division of General Studies" ~ "DGS",
                   Category == "College of Media" ~ "Media"
                   )) %>%
  filter(!is.na(Acronym))
##UIUC Colleges, Schools & Academic Units
#Carle Illinois College of Medicine (CIMED)
#College of Agricultural, Consumer & Environmental Sciences (ACES)
#College of Applied Health Sciences (AHS)
#College of Education (EDUC)
#College of Fine & Applied Arts (FAA)
#College of Law (LAW)
#College of Liberal Arts & Sciences (LAS)
#College of Media 
#College of Veterinary Medicine (VetMed)
#Division of General Studies
#Gies College of Business (BUS)
#Graduate College
#Grainger College of Engineering (GCOE)
#School of Social Work (SSW)
#School of Information Sciences (iSchool)
#School of Labor and Employment Relations (LER)

#all positions total salaries per college
employees2021 <- d2021_1 %>%
  count(ID1,Acronym) %>%
  count(Acronym)

#teaching faculty at all ranks 2021
teaching_employees2021 <- d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*PROF|TEACH.*PROF")) %>%
  count(ID1,Acronym) %>%
  count(Acronym) %>%
  add_row(Acronym=c("DGS","GC","LER"), n=c(0,0,0)) %>%
  arrange(Acronym)

###############################################################################################################################################################################################################################
# question for data viz: 2021 for each college, how many teaching faculty earn more than median income of college with highest median? blue for bars with at least half?? orange for bars with big number of promoted ranks?? #
###############################################################################################################################################################################################################################

table_teaching_employees<-arrange(teaching_employees2021,n)
table_proportion_teaching_employees<-teaching_employees2021$n/employees2021$n
table01<-tibble(Acronym=teaching_employees2021$Acronym,
                      Employees=employees2021$n,
                      TeachingEmployees=teaching_employees2021$n,
                      Proportion=table_proportion_teaching_employees)

teaching_salaries<-d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*PROF|TEACH.*PROF")) %>%
  distinct(Name, .keep_all = TRUE)
mean(teaching_salaries$PresentTotalSalary)
median0<-median(teaching_salaries$PresentTotalSalary)

table_teaching_median_salaries<-d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*PROF|TEACH.*PROF")) %>%
  distinct(Name, .keep_all = TRUE) %>%
  group_by(Acronym) %>%
  summarise(TotalSalary=sum(PresentTotalSalary),
            AverageSalary=mean(PresentTotalSalary),
            MedianSalary=median(PresentTotalSalary)) %>%
  arrange(desc(MedianSalary)) %>%
  select(Acronym,MedianSalary)

table_employees_above_median_of_median<-d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*PROF|TEACH.*PROF")) %>%
  distinct(Name, .keep_all = TRUE) %>%
  group_by(Acronym) %>%
  summarise(EmployeesAboveMedianOfMedian=sum(PresentTotalSalary>median(table_teaching_median_salaries$MedianSalary)))

table_employees_above_overall_median<-d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*PROF|TEACH.*PROF")) %>%
  distinct(Name, .keep_all = TRUE) %>%
  group_by(Acronym) %>%
  summarise(EmployeesAboveOverallMedian=sum(PresentTotalSalary>median0))

table_assistant_rank<-d2021_1 %>%
  filter(str_detect(toupper(Title),"TCH.*ASST\\sPROF|TEACH.*ASST\\sPROF")) %>%
  distinct(Name, .keep_all = TRUE) %>%
  count(Acronym) %>%
  add_row(Acronym="SSW", n=0) %>%
  arrange(Acronym)

table02<-tibble(table01[-c(5,8,12),],EmployeesAboveOverallMedian=table_employees_above_overall_median$EmployeesAboveOverallMedian)

table03<-table02 %>%
  mutate(OverHalfMedian=if_else(EmployeesAboveOverallMedian/TeachingEmployees>=0.5,1,0))

table04 <- table03 %>%
  mutate(AssistantRank=table_assistant_rank$n,
         AboveAssistantRank=TeachingEmployees-AssistantRank,
         OverHalfAboveAssistantRank=if_else(AboveAssistantRank/TeachingEmployees>=0.5,1,0),
         OverHalfBoth=OverHalfAboveAssistantRank+OverHalfMedian) %>%
  mutate(ColorsTab=case_when( OverHalfAboveAssistantRank+OverHalfMedian == 2 ~ "#FF5F0F",
                              OverHalfMedian == 1 ~ "#13294b",
                              OverHalfMedian == 0  ~ "#A5A8AA"))

###########
# barplot #
###########
par(mar=c(1.5,4,2,2)+0.1);barplot(sort(table04$Proportion), horiz = TRUE,
                                axes=FALSE,
                                col=table04[order(table04$Proportion),"ColorsTab"][[1]],
                                names.arg = "", border = NA,
                                xlim=c(0,0.1), ylim=c(-4,16)
)
axis(2, seq(0.7,15.1,len=13), col.axis="#5E6669", tick = FALSE, hadj = 0.7,
     labels=table04[order(table04$Proportion),"Acronym"][[1]], las=2)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.03,0.04,0.04,0.03)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.05,0.06,0.06,0.05)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.07,0.08,0.08,0.07)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.09-0.005,sort(table04$Proportion)[13],sort(table04$Proportion)[13],0.09-0.005),y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(3.8,3.8,4.8,4.8),col='#13294b', border=FALSE)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(2.6,2.6,3.6,3.6),col='#13294b', border=FALSE)
polygon(x=c(0.01-0.005,sort(table04$Proportion)[1],sort(table04$Proportion)[1],0.01-0.005),y=c(0.2,0.2,1.2,1.2),col='#13294b', border=FALSE)
text(sort(table04$Proportion)-0.002,seq(0.7,15.1,len=13),labels=round(sort(table04$Proportion),3)*100, col="white")
title(main="Teaching Professors (per 100) of All Ranks at UIUC by College in 2021",adj=0, line=0.75, cex.main=1.45)
title(main = "Original data by Board of Trustees Gray Book. Parsed data by G. Challen's graybooker.", line=-0.5, adj=0, cex.main=0.65, col.main="#5E6669")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-1,-1,0,0)-0.55,col='#FF5F0F', border=FALSE)
polygon(x=c(0.015+0.005/2,0.02,0.02,0.015+0.005/2)-0.015,y=c(-1,-1,0,0)-0.55,col='#13294b', border=FALSE)
text(0.006,-1, labels=paste0("At least half of the teaching professors earn a salary over $", median0, " and are beyond the assistant rank."), col=1, cex=1.05, adj=0)
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-2,-2,-1,-1)-0.55,col='#13294b', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-3,-3,-2,-2)-0.55,col='#A5A8AA', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-2,-2,-1.85,-1.85)-0.55,col='white', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-1,-1,-0.85,-0.85)-0.55,col='white', border="white")
text(0.006,-2,labels=paste0("At least half of the teaching professors earn a salary over $", median0, "."), col=1, cex=1.05, adj=0)
text(0.006,-3,labels=paste0("Less than half of the teaching professors earn a salary over $", median0, "."), col=1, cex=1.05, adj=0)
text(0.006,-4,labels=paste0("$", median0, " represents the sample median of all teaching professor salaries."), col=1, cex=1.05, adj=0)

#export to pdf
pdf(width=12,height=7)
par(mar=c(1.5,4,2,2)+0.1);barplot(sort(table04$Proportion), horiz = TRUE,
                                axes=FALSE,
                                col=table04[order(table04$Proportion),"ColorsTab"][[1]],
                                names.arg = "", border = NA,
                                xlim=c(0,0.1), ylim=c(-4,16)
)
axis(2, seq(0.7,15.1,len=13), col.axis="#5E6669", tick = FALSE, hadj = 0.7,
     labels=table04[order(table04$Proportion),"Acronym"][[1]], las=2)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.03,0.04,0.04,0.03)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.05,0.06,0.06,0.05)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.07,0.08,0.08,0.07)-0.005,y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.09-0.005,sort(table04$Proportion)[13],sort(table04$Proportion)[13],0.09-0.005),y=c(14.6,14.6,15.6,15.6),col='#13294b', border=FALSE)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(3.8,3.8,4.8,4.8),col='#13294b', border=FALSE)
polygon(x=c(0.01,0.02,0.02,0.01)-0.005,y=c(2.6,2.6,3.6,3.6),col='#13294b', border=FALSE)
polygon(x=c(0.01-0.005,sort(table04$Proportion)[1],sort(table04$Proportion)[1],0.01-0.005),y=c(0.2,0.2,1.2,1.2),col='#13294b', border=FALSE)
text(sort(table04$Proportion)-0.002,seq(0.7,15.1,len=13),labels=round(sort(table04$Proportion),3)*100, col="white")
title(main="Teaching Professors (per 100) of All Ranks at UIUC by College in 2021",adj=0, line=0.75, cex.main=1.45)
title(main = "Original data by Board of Trustees Gray Book. Parsed data by G. Challen's graybooker.", line=-0.5, adj=0, cex.main=0.65, col.main="#5E6669")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-1,-1,0,0)-0.55,col='#FF5F0F', border=FALSE)
polygon(x=c(0.015+0.005/2,0.02,0.02,0.015+0.005/2)-0.015,y=c(-1,-1,0,0)-0.55,col='#13294b', border=FALSE)
text(0.006,-1, labels=paste0("At least half of the teaching professors earn a salary over $", median0, " and are beyond the assistant rank."), col=1, cex=1.05, adj=0)
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-2,-2,-1,-1)-0.55,col='#13294b', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-3,-3,-2,-2)-0.55,col='#A5A8AA', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-2,-2,-1.85,-1.85)-0.55,col='white', border="white")
polygon(x=c(0.015,0.02,0.02,0.015)-0.015,y=c(-1,-1,-0.85,-0.85)-0.55,col='white', border="white")
text(0.006,-2,labels=paste0("At least half of the teaching professors earn a salary over $", median0, "."), col=1, cex=1.05, adj=0)
text(0.006,-3,labels=paste0("Less than half of the teaching professors earn a salary over $", median0, "."), col=1, cex=1.05, adj=0)
text(0.006,-4,labels=paste0("$", median0, " represents the sample median of all teaching professor salaries."), col=1, cex=1.05, adj=0)
dev.off()