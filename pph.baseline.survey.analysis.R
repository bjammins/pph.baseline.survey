library(VIF)
library(plyr)
library(reshape)
library(reshape2)
library(dplyr)
library(MASS)
library(car)
library(lmtest)
library(ggplot2)
library(pvclust)

setwd("L:/PPH Project/data")
data_aa<-read.csv("QI Learning Collaborative List for Critical Juncture.csv", header=TRUE)
data<-read.csv("Baseline Assessment Survey responses.csv", header=TRUE)
question<-read.csv("baseline assessment survey question table.csv", header=TRUE)
data<-tbl_df(data)
datac<-filter(data,Q49==1)#only get completed survey data
datac<-arrange(datac,Q1)
datac$Q2[datac$Q2==""]<-NA  #convert spaces to NAs
hosp1<-as.data.frame(na.omit(datac$Q2))
names(hosp1)<-"hosp_name"
datac$Q3[datac$Q3==""]<-NA
hosp2<-as.data.frame(na.omit(datac$Q3))
names(hosp2)<-"hosp_name"
datac$Q4[datac$Q4==""]<-NA
hosp3<-as.data.frame(na.omit(datac$Q4))
names(hosp3)<-"hosp_name"
hosp_name<-rbind(hosp1,hosp2,hosp3) #combine hospital names into one column
datac<-cbind(datac,hosp_name) #combine data frames
datac<-select(datac,-c(Q2,Q2.1,Q3,Q3.1,Q4,Q4.1)) #cleanup of hospital names because they have now been combined into hosp_name
datac<-datac %.% mutate(birth_cat = 1*(Q7<1000) + 2 * (Q7 >999 & Q7<3000)+3*(Q7>2999)) #create birth categories
#note original code was datac<-datac[-66,]  I realized that this was an error in the code as I wanted to remove Hackensack UMC because they did not have a birth category.  Instead, I believe I removed hunterdon

datac<-datac[!is.na(datac$birth_cat),] #remove hospital without a birth category Hackensack UMC- Pascack Valley
datac$birth_cat<-as.factor(datac$birth_cat)


#set a student indicator variable to either 1 or 0 if they have a student vs they don't have a student

datac$Q9 <- as.numeric(ifelse(datac$Q9 == "Nursing students", 1, 0))
datac$Q9.1 <- as.numeric(ifelse(datac$Q9.1 == "Medical students", 1, 0))
datac$Q9.2 <- as.numeric(ifelse(datac$Q9.2 == "Midwifery students", 1, 0))
student<-cbind(datac$Q9,datac$Q9.1, datac$Q9.2)
datac$student<-rowSums(student)
datac$student<-ifelse(datac$student>0,1,0)
datac$student<-as.factor(datac$student)

#format cesarean rates
#datac$Q8<-as.numeric(datac$Q8)

datac<-datac %.% mutate(cesarean = 1*(Q8<30) + 2 * (Q8 >29.99 & Q8<40)+3*(Q8 >39.99))
datac$cesarean<-as.factor(datac$cesarean)

#set resident indicator

datac$Q9.3<-as.numeric(ifelse(datac$Q9.3 == "Obstetric residents", 1, 0))
datac$Q9.4<-as.numeric(ifelse(datac$Q9.4 == "Family practice residents who provide OB services", 1, 0))
resident<-cbind(datac$Q9.3,datac$Q9.4)
datac$resident<-rowSums(resident)
datac$resident<-ifelse(datac$resident>0,1,0)
datac$resident<-as.factor(datac$resident)

datac$Q12<-revalue(datac$Q12, c("State and local government hospital"="O"))
datac$Q12<-revalue(datac$Q12, c("Non-government not-for-profit hospital"="NP"))
datac$Q12<-revalue(datac$Q12, c("Non-government investor-owned (for profit) hospital"="FP"))
datac$Q12<-revalue(datac$Q12, c("Other (please specify)"="O"))
datac$Q12<-revalue(datac$Q12, c("Unknown"="O"))

#reordering of the levels to get them in the order that I want
datac$Q14<-factor(datac$Q14, levels=c("Low level of preparedness","Moderate level of preparedness","High level of preparedness","Very high level of preparedness"))
datac$Q14<-revalue(datac$Q14, c("Low level of preparedness"= 0))
datac$Q14<-revalue(datac$Q14, c("Moderate level of preparedness"= 1))
datac$Q14<-revalue(datac$Q14, c("High level of preparedness"= 2))
datac$Q14<-revalue(datac$Q14, c("Very high level of preparedness"= 2))
datac$Q15<-factor(datac$Q15, levels=c("We do not have a standard definition for postpartum hemorrhage","greater than 500 ml of blood loss for all deliveries","greater than 500 ml for vaginal deliveries and greater than 1000 ml for cesarean deliveries","greater than 1000 ml for all deliveries","Other definition (please specify)")) #remove the "" factor
datac$Q16<-factor(datac$Q16, levels=c("Yes","No","Unknown")) #remove the ""factor



#maternal transports indicator
datac$Q10<-as.numeric(ifelse(datac$Q10 == "Yes", 1, 0))
datac$Q10<-as.factor(datac$Q10)

#neonatal transpors indicator
datac$Q11<-as.numeric(ifelse(datac$Q11 == "Yes", 1, 0))
datac$Q11<-as.factor(datac$Q11)

#Magnet
datac$Q13<-as.numeric(ifelse(datac$Q13 == "Yes", 1, 0))
datac$Q13<-as.factor(datac$Q13)


#####create collaborative statistics
#Collaborative only hospitals
collab<-datac[datac$collaborative=="1",]
summary(collab$Q1) #state

#calculate summary statistics by hospitals in the collaborative
#statistics for calculating hospital birth data
summary(collab$Q7)
collab_sum<-collab%>%
  group_by(Q1)%>%
  summarise(n=n(), mean=mean(Q7, na.rm=TRUE), sd=sd(Q7, na.rm=TRUE), `25%`=quantile(Q7, probs=0.25, na.rm=TRUE), median=median(Q7, na.rm=TRUE), `75%`=quantile(Q7, probs=0.75, na.rm=TRUE), min=min(Q7, na.rm=TRUE), max=max(Q7, na.rm=TRUE))

#birth category data
collab_sum_birth_cat<-collab%>%
  group_by(Q1,birth_cat)%>%
  summarise(n=n())



#cesarean data
collab_sum_cesarean<-collab%>%
  group_by(Q1,cesarean)%>%
  summarise(n=n())


#Student data
collab_sum_students<-collab%>%
  group_by(Q1,student)%>%
  summarise(n=n())



#resident data
collab_sum_resident<-collab%>%
  group_by(Q1,resident)%>%
  summarise(n=n())


#magnet data
collab_sum_magnet<-collab%>%
  group_by(Q1,Q13)%>%
  summarise(n=n())


#maternal transport
collab_sum_mat_trans<-collab%>%
  group_by(Q1,Q10)%>%
  summarise(n=n())

#neonatal transport
collab_sum_neo_trans<-collab%>%
  group_by(Q1,Q11)%>%
  summarise(n=n())


#hospital type
collab_sum_hospital_type<-collab%>%
  group_by(Q1,Q12)%>%
  summarise(n=n())



#Q15:  How does your hospital define postpartum hemorrhage
datac$Q15<-as.numeric(ifelse(datac$Q15 == "We do not have a standard definition for postpartum hemorrhage", 0, 1))


#Q16:  Does your hospital have a policy and procedure for management of postpartum hemorrhage
datac$Q16<-as.numeric(ifelse(datac$Q16 == "Yes", 1, 0))


#Q17:  Which of the follow best describes how your hospital defines Massive PPH?  There are numerous answers to this question
datac$Q17<-as.numeric(ifelse(datac$Q17 == "We do not have a standard definition for massive postpartum hemorrhage", 0,1))

#Q18:  Does your hospital have a policy and procedure for the management of Massive PPHs?
datac$Q18<-as.numeric(ifelse(datac$Q18 == "Yes", 1,0))


#Q19:  Does your hospital have a policy and procedure for the management of Massive PPHs?
datac$Q19<-as.numeric(ifelse(datac$Q19 == "Yes", 1,0))

#Q20:  Upon admission, does your hospital have a standard process for aidentifying patients who are at risk for PPH?
datac$Q20<-as.numeric(ifelse(datac$Q20 == "Yes", 1,0))

#Q21:  For vaginal deliveries, within 30 minutes prior to delivery, does your hospital have a standard process for identifying patients who are at risk for PPH?
datac$Q21<-as.numeric(ifelse(datac$Q21 == "Yes", 1,0))


#Q22:  For cesarean deliveries, within 30 minutes prior to delivery, does your hospital have a standard process for identifying patients who are at risk for PPH?
datac$Q22<-as.numeric(ifelse(datac$Q22 == "Yes", 1,0))

#Q23:  For vaginal deliveries, within 30 minutes post-delivery of the placenta, does your hospital have a standard process for identifying patients who are at risk for PPH?
datac$Q23<-as.numeric(ifelse(datac$Q23 == "Yes", 1,0))

#Q24:  For cesarean deliveries, within 30 minutes post-delivery of the placenta, does your hospital have a standard process for identifying patients who are at risk for PPH?
datac$Q24<-as.numeric(ifelse(datac$Q24 == "Yes", 1,0))


#Q25:  Which of the following people at your hospital are identified and activevely engaged in leading QI initiatives (Nurse champion)?
datac$Q25<-as.numeric(ifelse(datac$Q25 == "Nurse champion", 1,0))
#Q25.1:  Which of the following people at your hospital are identified and activevely engaged in leading QI initiatives (OB physician champion)?
datac$Q25.1<-as.numeric(ifelse(datac$Q25.1 == "OB physician champion", 1,0))

#Q25.2:  Which of the following people at your hospital are identified and activevely engaged in leading QI initiatives (Blood bank physician champion)?
datac$Q25.2<-as.numeric(ifelse(datac$Q25.2 == "Blood bank physician champion", 1,0))

#Q25.3:  Which of the following people at your hospital are identified and activevely engaged in leading QI initiatives (Anesthesia physician champion)?
datac$Q25.3<-as.numeric(ifelse(datac$Q25.3 == "Anesthesia physician champion", 1,0))

#Q25.4:  Which of the following people at your hospital are identified and activevely engaged in leading QI initiatives (Other (please specify))?
datac$Q25.4<-as.numeric(ifelse(datac$Q25.4 == "Other (please specify)", 1,0))

datac$Q25.4[datac$Q25.5=="none"]<-0
datac$Q25.4[datac$Q25.5=="None"]<-0


#Q26:  Does your hospital have a policy and procedure that provides guideance about the AMSTL?
datac$Q26<-as.numeric(ifelse(datac$Q26 == "Yes", 1,0))

#Q27:  When were your hospital's PPH Protocols last revised/updated?
datac$Q27<-as.numeric(ifelse(datac$Q27 == "Within the past three years", 1,0))

#Q28:  Which of the choices below primarily describes how maternal blood loss at your hospital is evaluated within the first 1-2 hours after vaginal birth)
datac$Q28<-revalue(datac$Q28, c("Visual estimation of blood loss only"= 0))
datac$Q28<-revalue(datac$Q28, c("Blood collection measured in a container (e.g. calibrated underbuttocks drapes) plus visual estimation of blood loss"= 0))
datac$Q28<-revalue(datac$Q28, c("Other (please specify)"= 0))
datac$Q28<-revalue(datac$Q28, c("Weighing of blood-soaked items & subtracting dry weight and extra fluids"= 1))
datac$Q28<-revalue(datac$Q28, c("Weighing of blood-soaked items plus blood collection measured in a container (e.g. calibrated underbuttocks drapes) and subtracting out extra fluids"= 1))
datac$Q28 <- as.numeric(ifelse(datac$Q28 == "1", 1, 0))

#Q29:  Which of the choices below primarily describes how maternal blood loss at your hospital is evaluated within the first 1-2 hours after cesarean birth)
datac$Q29<-revalue(datac$Q29, c("Visual estimation of blood loss only"= 0))
datac$Q29<-revalue(datac$Q29, c("Blood collection measured in a container (e.g. calibrated underbuttocks drapes) plus visual estimation of blood loss"= 0))
datac$Q29<-revalue(datac$Q29, c("Other (please specify)"= 0))
datac$Q29<-revalue(datac$Q29, c("Weighing of blood-soaked items & subtracting dry weight and extra fluids"= 1))
datac$Q29<-revalue(datac$Q29, c("Weighing of blood-soaked items plus blood collection measured in a container (e.g. calibrated underbuttocks drapes) and subtracting out extra fluids"= 1))
datac$Q29 <- as.numeric(ifelse(datac$Q29 == "1", 1, 0))


#Q30:  Which of the choices below primarily describes how maternal blood loss at your hospital is evaluated 3-12 hours after birth)
datac$Q30<-revalue(datac$Q30, c("Visual estimation of blood loss only"= 0))
datac$Q30<-revalue(datac$Q30, c("Blood collection measured in a container (e.g. calibrated underbuttocks drapes) plus visual estimation of blood loss"= 0))
datac$Q30<-revalue(datac$Q30, c("Other (please specify)"= 0))
datac$Q30<-revalue(datac$Q30, c("Weighing of blood-soaked items & subtracting dry weight and extra fluids"= 1))
datac$Q30<-revalue(datac$Q30, c("Weighing of blood-soaked items plus blood collection measured in a container (e.g. calibrated underbuttocks drapes) and subtracting out extra fluids"= 1))
datac$Q30 <- as.numeric(ifelse(datac$Q30 == "1", 1, 0))

#Q31:  How often does your hospital perform a debriefing of a PPH emergency?
datac$Q31 <- as.numeric(ifelse(datac$Q31 == "We don't perform debriefings of PPH emergencies", 0,1))

#Q32:  How often does your hospital perform a quality review of a PPH emergency?
datac$Q32 <- as.numeric(ifelse(datac$Q32 == "We don't perform quality reviews or root cause analyses of PPH emergencies", 0,1))

#Q34:  Does your hospital run PPH simulation drills?
datac$Q34<-as.numeric(ifelse(datac$Q34 == "Yes", 1,0))

#38: Which of the following medications are available for use at your hospital for management of a PPH?  (misoprostol (Cytotec))?
datac$Q38<-as.numeric(ifelse(datac$Q38 == "Yes", 1,0))

#38.1: Which of the following medications are available for use at your hospital for management of a PPH?  (carboprost tromethamine)?
datac$Q38.1<-as.numeric(ifelse(datac$Q38.1 == "Yes", 1,0))


#38.2: Which of the following medications are available for use at your hospital for management of a PPH?  (prostaglandin)?
datac$Q38.2<-as.numeric(ifelse(datac$Q38.2 == "Yes", 1,0))


##38.4: Which of the following medications are available for use at your hospital for management of a PPH?  (Methylegonovine maleate)?
datac$Q38.4<-as.numeric(ifelse(datac$Q38.4 == "Yes", 1,0))


##39: Which of the following procedures are available for use at your hospital for management of a PPH?  (Uterine artery ligation)?
datac$Q39<-as.numeric(ifelse(datac$Q39 == "Yes", 1,0))

##39.1: Which of the following procedures are available for use at your hospital for management of a PPH?  (B Lynch)?
datac$Q39.1<-as.numeric(ifelse(datac$Q39.1 == "Yes", 1,0))

##39.2: Which of the following procedures are available for use at your hospital for management of a PPH?  (uterine artery embolization)?
datac$Q39.2<-as.numeric(ifelse(datac$Q39.2 == "Yes", 1,0))

##39.3: Which of the following procedures are available for use at your hospital for management of a PPH?  (Hysterectomy)?
datac$Q39.3<-as.numeric(ifelse(datac$Q39.3 == "Yes", 1,0))

##39.4: Which of the following procedures are available for use at your hospital for management of a PPH?  (Uterine balloon tamponade)?
datac$Q39.4<-as.numeric(ifelse(datac$Q39.4 == "Yes", 1,0))


##40: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (scales to weigh blood loss)?
datac$Q40<-as.numeric(ifelse(datac$Q40 == "Yes", 1,0))


##40.1: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (underbuttock calibrated drapes)?
datac$Q40.1<-as.numeric(ifelse(datac$Q40.1 == "Yes", 1,0))

##40.2: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (NPASG)?
datac$Q40.2<-as.numeric(ifelse(datac$Q40.2 == "Yes", 1,0))

##40.3: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (Blood warmer)?
datac$Q40.3<-as.numeric(ifelse(datac$Q40.3 == "Yes", 1,0))

##40.4: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (Fluid warmer)?
datac$Q40.4<-as.numeric(ifelse(datac$Q40.4 == "Yes", 1,0))

##40.5: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (Invasive hemodynamic monitoring)?
datac$Q40.5<-as.numeric(ifelse(datac$Q40.5 == "Yes", 1,0))

##40.6: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (cell saver)?
datac$Q40.6<-as.numeric(ifelse(datac$Q40.6 == "Yes", 1,0))


##40.7: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (rapid fluid infuser)?
datac$Q40.7<-as.numeric(ifelse(datac$Q40.7 == "Yes", 1,0))

##40.8: Which of the following devices/equipment are available for use at your hospital for management of a PPH?  (maternal warming device)?
datac$Q40.8<-as.numeric(ifelse(datac$Q40.8 == "Yes", 1,0))

datac<-datac[-c(1:4),]#Remove the Washington DC Hospitals per the data panel



#group by state and by birth category
by_state_bcat<-group_by(datac,Q1,birth_cat)

################

################ 
#Calculating Table #1
#Calculating Birth volume numbers

#by state by birth category
b_s_c_7<-group_by(by_state_bcat,Q1)

summarise(b_s_c_7, count=n(), pct_of_total=n()/dim(by_state_bcat)[1],median=median(Q7), IQR=IQR(Q7))

#by birth category only
b_s_c<-group_by(by_state_bcat, birth_cat)

summarise(b_s_c, count=n(), pct_of_total=n()/dim(by_state_bcat)[1],median=median(Q7), IQR=IQR(Q7))

###################

#by student
b_s_c<-group_by(by_state_bcat, student)

summarise(b_s_c, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#by resident

b_s_c<-group_by(by_state_bcat, Q1, resident)

summarise(b_s_c, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#by cesarean category
b_s_c_8<-group_by(by_state_bcat,Q1,cesarean)

summarise(b_s_c_8, count=n(), pct_of_total=n()/dim(by_state_bcat)[1],median=median(Q8), IQR=IQR(Q8))

#by maternal transports

b_s_c_10<-group_by(by_state_bcat,Q1,Q10)

summarise(b_s_c_10, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])


#neonatal transports
b_s_c_11<-group_by(by_state_bcat, Q1, Q11)

summarise(b_s_c_11, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])


#Magnet

b_s_c_13<-group_by(by_state_bcat,Q1, Q13)

summarise(b_s_c_13, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])


#Hospital Type
b_s_c_12<-group_by(by_state_bcat, Q1, Q12)

summarise(b_s_c_12, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Collaborative

b_s_c_collab<-group_by(by_state_bcat, collaborative)

summarise(b_s_c_collab, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#How prepared is your hospital for a hemorrhage event
b_s_c_14<-group_by(by_state_bcat,Q14)

summarise(b_s_c_14, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#####################################################################################################################
#Calculating Table 2
######################################################################################################################
b_s_c_15<-group_by(by_state_bcat,Q1, Q15)
sum_q15<-summarise(b_s_c_15, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q15_pct<-sum_q15$count[2]/sum(sum_q15$count[1:2])
NJ_q15_pct<-sum_q15$count[4]/sum(sum_q15$count[3:4])
sum_q15_tbl<-as.data.frame(rbind(GA_q15_pct,NJ_q15_pct))
sum_q15_tbl$state<-c("GA", "NJ")
sum_q15_tbl$Quest<-rep("Q15",dim(sum_q15_tbl)[1])



#Q16
b_s_c_16<-group_by(by_state_bcat,Q1, Q16)
sum_q16<-summarise(b_s_c_16, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q16_pct<-sum_q16$count[2]/sum(sum_q16$count[1:2])
NJ_q16_pct<-sum_q16$count[4]/sum(sum_q16$count[3:4])
sum_q16_tbl<-as.data.frame(rbind(GA_q16_pct,NJ_q16_pct))
sum_q16_tbl$state<-c("GA", "NJ")
sum_q16_tbl$Quest<-rep("Q16",dim(sum_q16_tbl)[1])



#Q17
b_s_c_17<-group_by(by_state_bcat,Q1, Q17)
sum_q17<-summarise(b_s_c_17, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q17_pct<-sum_q17$count[2]/sum(sum_q17$count[1:2])
NJ_q17_pct<-sum_q17$count[4]/sum(sum_q17$count[3:4])
sum_q17_tbl<-as.data.frame(rbind(GA_q17_pct,NJ_q17_pct))
sum_q17_tbl$state<-c("GA", "NJ")
sum_q17_tbl$Quest<-rep("Q17",dim(sum_q17_tbl)[1])



#Q18
b_s_c_18<-group_by(by_state_bcat, Q1, Q18)
sum_q18<-summarise(b_s_c_18, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q18_pct<-sum_q18$count[2]/sum(sum_q18$count[1:2])
NJ_q18_pct<-sum_q18$count[4]/sum(sum_q18$count[3:4])
sum_q18_tbl<-as.data.frame(rbind(GA_q18_pct,NJ_q18_pct))
sum_q18_tbl$state<-c("GA", "NJ")
sum_q18_tbl$Quest<-rep("Q18",dim(sum_q18_tbl)[1])


#Q19: Does your hospital have a policy and procedure for the management of massive postpartum hemorrhages?

b_s_c_19<-group_by(by_state_bcat, Q1,Q19)
sum_q19<-summarise(b_s_c_19, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q19_pct<-sum_q19$count[2]/sum(sum_q19$count[1:2])
NJ_q19_pct<-sum_q19$count[4]/sum(sum_q19$count[3:4])
sum_q19_tbl<-as.data.frame(rbind(GA_q19_pct,NJ_q19_pct))
sum_q19_tbl$state<-c("GA", "NJ")
sum_q19_tbl$Quest<-rep("Q19",dim(sum_q19_tbl)[1])


#Q20
b_s_c_20<-group_by(by_state_bcat,Q1, Q20)
sum_q20<-summarise(b_s_c_20, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q20_pct<-sum_q20$count[2]/sum(sum_q20$count[1:2])
NJ_q20_pct<-sum_q20$count[4]/sum(sum_q20$count[3:4])
sum_q20_tbl<-as.data.frame(rbind(GA_q20_pct,NJ_q20_pct))
sum_q20_tbl$state<-c("GA", "NJ")
sum_q20_tbl$Quest<-rep("Q20",dim(sum_q20_tbl)[1])


#Q21
b_s_c_21<-group_by(by_state_bcat,Q1, Q21)
sum_q21<-summarise(b_s_c_21, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q21_pct<-sum_q21$count[2]/sum(sum_q21$count[1:2])
NJ_q21_pct<-sum_q21$count[4]/sum(sum_q21$count[3:4])
sum_q21_tbl<-as.data.frame(rbind(GA_q21_pct,NJ_q21_pct))
sum_q21_tbl$state<-c("GA", "NJ")
sum_q21_tbl$Quest<-rep("Q21",dim(sum_q21_tbl)[1])


#Q22
b_s_c_22<-group_by(by_state_bcat, Q1, Q22)
sum_q22<-summarise(b_s_c_22, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q22_pct<-sum_q22$count[2]/sum(sum_q22$count[1:2])
NJ_q22_pct<-sum_q22$count[4]/sum(sum_q22$count[3:4])
sum_q22_tbl<-as.data.frame(rbind(GA_q22_pct,NJ_q22_pct))
sum_q22_tbl$state<-c("GA", "NJ")
sum_q22_tbl$Quest<-rep("Q22",dim(sum_q22_tbl)[1])

#Q23
b_s_c_23<-group_by(by_state_bcat, Q1,Q23)
sum_q23<-summarise(b_s_c_23, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q23_pct<-sum_q23$count[2]/sum(sum_q23$count[1:2])
NJ_q23_pct<-sum_q23$count[4]/sum(sum_q23$count[3:4])
sum_q23_tbl<-as.data.frame(rbind(GA_q23_pct,NJ_q23_pct))
sum_q23_tbl$state<-c("GA", "NJ")
sum_q23_tbl$Quest<-rep("Q23",dim(sum_q23_tbl)[1])

#Q24
b_s_c_24<-group_by(by_state_bcat,Q1, Q24)
sum_q24<-summarise(b_s_c_24, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q24_pct<-sum_q24$count[2]/sum(sum_q24$count[1:2])
NJ_q24_pct<-sum_q24$count[4]/sum(sum_q24$count[3:4])
sum_q24_tbl<-as.data.frame(rbind(GA_q24_pct,NJ_q24_pct))
sum_q24_tbl$state<-c("GA", "NJ")
sum_q24_tbl$Quest<-rep("Q24",dim(sum_q24_tbl)[1])


#Q25
b_s_c_25<-group_by(by_state_bcat,Q1,Q25)
sum_q25<-summarise(b_s_c_25, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q25_pct<-sum_q25$count[2]/sum(sum_q25$count[1:2])
NJ_q25_pct<-sum_q25$count[4]/sum(sum_q25$count[3:4])
sum_q25_tbl<-as.data.frame(rbind(GA_q25_pct,NJ_q25_pct))
sum_q25_tbl$state<-c("GA", "NJ")
sum_q25_tbl$Quest<-rep("Q25",dim(sum_q25_tbl)[1])


#Q25.1
b_s_c_25.1<-group_by(by_state_bcat,Q1, Q25.1)
sum_q25.1<-summarise(b_s_c_25.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q25.1_pct<-sum_q25.1$count[2]/sum(sum_q25.1$count[1:2])
NJ_q25.1_pct<-sum_q25.1$count[4]/sum(sum_q25.1$count[3:4])
sum_q25.1_tbl<-as.data.frame(rbind(GA_q25.1_pct,NJ_q25.1_pct))
sum_q25.1_tbl$state<-c("GA", "NJ")
sum_q25.1_tbl$Quest<-rep("Q25.1",dim(sum_q24_tbl)[1])


#Q25.2
b_s_c_25.2<-group_by(by_state_bcat, Q1, Q25.2)
sum_q25.2<-summarise(b_s_c_25.2, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q25.2_pct<-sum_q25.2$count[2]/sum(sum_q25.2$count[1:2])
NJ_q25.2_pct<-sum_q25.2$count[4]/sum(sum_q25.2$count[3:4])
sum_q25.2_tbl<-as.data.frame(rbind(GA_q25.2_pct,NJ_q25.2_pct))
sum_q25.2_tbl$state<-c("GA", "NJ")
sum_q25.2_tbl$Quest<-rep("Q25.2",dim(sum_q25.2_tbl)[1])




#Q25.3
b_s_c_25.3<-group_by(by_state_bcat, Q1, Q25.3)
sum_q25.3<-summarise(b_s_c_25.3, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q25.3_pct<-sum_q25.3$count[2]/sum(sum_q25.3$count[1:2])
NJ_q25.3_pct<-sum_q25.3$count[4]/sum(sum_q25.3$count[3:4])
sum_q25.3_tbl<-as.data.frame(rbind(GA_q25.3_pct,NJ_q25.3_pct))
sum_q25.3_tbl$state<-c("GA", "NJ")
sum_q25.3_tbl$Quest<-rep("Q25.3",dim(sum_q24_tbl)[1])



#Q25.4
b_s_c_25.4<-group_by(by_state_bcat, Q1, Q25.4)
summarise(b_s_c_25.4, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q26: Does your hospital have a P and P about the active management of the third stage of labor?
b_s_c_26<-group_by(by_state_bcat, Q1, Q26)
sum_q26<-summarise(b_s_c_26, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q26_pct<-sum_q26$count[2]/sum(sum_q26$count[1:2])
NJ_q26_pct<-sum_q26$count[4]/sum(sum_q26$count[3:4])
sum_q26_tbl<-as.data.frame(rbind(GA_q26_pct,NJ_q26_pct))
sum_q26_tbl$state<-c("GA", "NJ")
sum_q26_tbl$Quest<-rep("Q26",dim(sum_q26_tbl)[1])


#Q27: When were your hospital's protocls last revised/updated?
b_s_c_27<-group_by(by_state_bcat, Q1, Q27)
sum_q27<-summarise(b_s_c_27, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q27_pct<-sum_q27$count[2]/sum(sum_q27$count[1:2])
NJ_q27_pct<-sum_q27$count[4]/sum(sum_q27$count[3:4])
sum_q27_tbl<-as.data.frame(rbind(GA_q27_pct,NJ_q27_pct))
sum_q27_tbl$state<-c("GA", "NJ")
sum_q27_tbl$Quest<-rep("Q27",dim(sum_q27_tbl)[1])



#Q28: Which of the choices below primarly describes how maternal blood loss at your hospital is evaluated within the first 1-2 hours after vaginal birth?
b_s_c_28<-group_by(by_state_bcat, Q1, Q28)
sum_q28<-summarise(b_s_c_28, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q28_pct<-sum_q28$count[2]/sum(sum_q28$count[1:2])
NJ_q28_pct<-sum_q28$count[4]/sum(sum_q28$count[3:4])
sum_q28_tbl<-as.data.frame(rbind(GA_q28_pct,NJ_q28_pct))
sum_q28_tbl$state<-c("GA", "NJ")
sum_q28_tbl$Quest<-rep("Q28",dim(sum_q28_tbl)[1])

#Q29: Which of the choices below primarly describes how maternal blood loss at your hospital is evaluated within the first 1-2 hours after cesarean birth?
b_s_c_29<-group_by(by_state_bcat,Q1,Q29)
sum_q29<-summarise(b_s_c_29, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q29_pct<-sum_q29$count[2]/sum(sum_q29$count[1:2])
NJ_q29_pct<-sum_q29$count[4]/sum(sum_q29$count[3:4])
sum_q29_tbl<-as.data.frame(rbind(GA_q29_pct,NJ_q29_pct))
sum_q29_tbl$state<-c("GA", "NJ")
sum_q29_tbl$Quest<-rep("Q29",dim(sum_q29_tbl)[1])

#Q30: Which of the choices below primarly describes how maternal blood loss at your hospital is evaluated 3-12 hours after birth?
b_s_c_30<-group_by(by_state_bcat,Q1,Q30)
sum_q30<-summarise(b_s_c_30, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q30_pct<-sum_q30$count[2]/sum(sum_q30$count[1:2])
NJ_q30_pct<-sum_q30$count[4]/sum(sum_q30$count[3:4])
sum_q30_tbl<-as.data.frame(rbind(GA_q30_pct,NJ_q30_pct))
sum_q30_tbl$state<-c("GA", "NJ")
sum_q30_tbl$Quest<-rep("Q30",dim(sum_q30_tbl)[1])


#Q31: How often does your hospital perform a debriefing of a PPH emergency?
b_s_c_31<-group_by(by_state_bcat,Q1,Q31)
sum_q31<-summarise(b_s_c_31, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q31_pct<-sum_q31$count[2]/sum(sum_q31$count[1:2])
NJ_q31_pct<-sum_q31$count[4]/sum(sum_q31$count[3:4])
sum_q31_tbl<-as.data.frame(rbind(GA_q31_pct,NJ_q31_pct))
sum_q31_tbl$state<-c("GA", "NJ")
sum_q31_tbl$Quest<-rep("Q31",dim(sum_q31_tbl)[1])


#Q32: How often does your hospital perform a quality review of a PPH emergency?
b_s_c_32<-group_by(by_state_bcat,Q1, Q32)
sum_q32<-summarise(b_s_c_32, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q32_pct<-sum_q32$count[2]/sum(sum_q32$count[1:2])
NJ_q32_pct<-sum_q32$count[4]/sum(sum_q32$count[3:4])
sum_q32_tbl<-as.data.frame(rbind(GA_q32_pct,NJ_q32_pct))
sum_q32_tbl$state<-c("GA", "NJ")
sum_q32_tbl$Quest<-rep("Q32",dim(sum_q32_tbl)[1])

#Q34: Does your hospital run PPH simulation drills?
b_s_c_34<-group_by(by_state_bcat, Q1, Q34)
sum_q34<-summarise(b_s_c_34, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q34_pct<-sum_q34$count[2]/sum(sum_q34$count[1:2])
NJ_q34_pct<-sum_q34$count[4]/sum(sum_q34$count[3:4])
sum_q34_tbl<-as.data.frame(rbind(GA_q34_pct,NJ_q34_pct))
sum_q34_tbl$state<-c("GA", "NJ")
sum_q34_tbl$Quest<-rep("Q34",dim(sum_q34_tbl)[1])



#Q38:  Which of the following medications are available for use at your hospital for management of a PPH?

b_s_c_38<-group_by(by_state_bcat, Q38)
summarise(b_s_c_38, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q38.1: 
b_s_c_38.1<-group_by(by_state_bcat, Q38.1)
summarise(b_s_c_38.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q38.2: 
b_s_c_38.2<-group_by(by_state_bcat, Q1, Q38.2)
summarise(b_s_c_38.2, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q38.4: 
b_s_c_38.4<-group_by(by_state_bcat, Q38.4)
summarise(b_s_c_38.4, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q39: 
b_s_c_39<-group_by(by_state_bcat, Q1, Q39)
sum_q39<-summarise(b_s_c_39, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q39_pct<-sum_q39$count[2]/sum(sum_q39$count[1:2])
NJ_q39_pct<-sum_q39$count[4]/sum(sum_q39$count[3:4])
sum_q39_tbl<-as.data.frame(rbind(GA_q39_pct,NJ_q39_pct))
sum_q39_tbl$state<-c("GA", "NJ")
sum_q39_tbl$Quest<-rep("Q39",dim(sum_q39_tbl)[1])






#Q39.1: 
b_s_c_39.1<-group_by(by_state_bcat,Q1, Q39.1)
sum_q39.1<-summarise(b_s_c_39.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q39.1_pct<-sum_q39.1$count[2]/sum(sum_q39.1$count[1:2])
NJ_q39.1_pct<-sum_q39.1$count[4]/sum(sum_q39.1$count[3:4])
sum_q39.1_tbl<-as.data.frame(rbind(GA_q39.1_pct,NJ_q39.1_pct))
sum_q39.1_tbl$state<-c("GA", "NJ")
sum_q39.1_tbl$Quest<-rep("Q39.1",dim(sum_q39.1_tbl)[1])




#Q39.2: 
b_s_c_39.2<-group_by(by_state_bcat,Q1, Q39.2)
summarise(b_s_c_39.2, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q39.3: 
b_s_c_39.3<-group_by(by_state_bcat, Q1, Q39.3)
sum_q39.3<-summarise(b_s_c_39.3, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q39.3_pct<-sum_q39.3$count[2]/sum(sum_q39.3$count[1:2])
NJ_q39.3_pct<-sum_q39.3$count[3]/sum(sum_q39.3$count[3])
sum_q39.3_tbl<-as.data.frame(rbind(GA_q39.3_pct,NJ_q39.3_pct))
sum_q39.3_tbl$state<-c("GA", "NJ")
sum_q39.3_tbl$Quest<-rep("Q39.3",dim(sum_q39.3_tbl)[1])



#Q39.4: 
b_s_c_39.4<-group_by(by_state_bcat,Q1, Q39.4)
sum_q39.4<-summarise(b_s_c_39.4, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q39.4_pct<-sum_q39.4$count[2]/sum(sum_q39.4$count[1:2])
NJ_q39.4_pct<-sum_q39.4$count[4]/sum(sum_q39.4$count[3:4])
sum_q39.4_tbl<-as.data.frame(rbind(GA_q39.4_pct,NJ_q39.4_pct))
sum_q39.4_tbl$state<-c("GA", "NJ")
sum_q39.4_tbl$Quest<-rep("Q39.4",dim(sum_q39.4_tbl)[1])


#Q40:
b_s_c_40<-group_by(by_state_bcat,Q1, Q40)
sum_q40<-summarise(b_s_c_40, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40_pct<-sum_q40$count[2]/sum(sum_q40$count[1:2])
NJ_q40_pct<-sum_q40$count[4]/sum(sum_q40$count[3:4])
sum_q40_tbl<-as.data.frame(rbind(GA_q40_pct,NJ_q40_pct))
sum_q40_tbl$state<-c("GA", "NJ")
sum_q40_tbl$Quest<-rep("Q40",dim(sum_q40_tbl)[1])



#Q40.1:
b_s_c_40.1<-group_by(by_state_bcat,Q1, Q40.1)
sum_q40.1<-summarise(b_s_c_40.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q40.1_pct<-sum_q40.1$count[2]/sum(sum_q40.1$count[1:2])
NJ_q40.1_pct<-sum_q40.1$count[4]/sum(sum_q40.1$count[3:4])
sum_q40.1_tbl<-as.data.frame(rbind(GA_q40.1_pct,NJ_q40.1_pct))
sum_q40.1_tbl$state<-c("GA", "NJ")
sum_q40.1_tbl$Quest<-rep("Q40.1",dim(sum_q40.1_tbl)[1])



#Q40.2:
b_s_c_40.2<-group_by(by_state_bcat, Q40.2)
summarise(b_s_c_40.2, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

#Q40.3:
b_s_c_40.3<-group_by(by_state_bcat, Q1, Q40.3)
sum_q40.3<-summarise(b_s_c_40.3, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
GA_q40.3_pct<-sum_q40.3$count[2]/sum(sum_q40.3$count[1:2])
NJ_q40.3_pct<-sum_q40.3$count[4]/sum(sum_q40.3$count[3:4])
sum_q40.3_tbl<-as.data.frame(rbind(GA_q40.3_pct,NJ_q40.3_pct))
sum_q40.3_tbl$state<-c("GA", "NJ")
sum_q40.3_tbl$Quest<-rep("Q40.3",dim(sum_q40.3_tbl)[1])





#Q40.4:
b_s_c_40.4<-group_by(by_state_bcat, Q1, Q40.4)
sum_q40.4<-summarise(b_s_c_40.4, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40.4_pct<-sum_q40.4$count[2]/sum(sum_q40.4$count[1:2])
NJ_q40.4_pct<-sum_q40.4$count[4]/sum(sum_q40.4$count[3:4])
sum_q40.4_tbl<-as.data.frame(rbind(GA_q40.4_pct,NJ_q40.4_pct))
sum_q40.4_tbl$state<-c("GA", "NJ")
sum_q40.4_tbl$Quest<-rep("Q40.4",dim(sum_q40.4_tbl)[1])




#Q40.5:
b_s_c_40.5<-group_by(by_state_bcat, Q1, Q40.5)
sum_q40.5<-sum_q40.5<-summarise(b_s_c_40.5, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40.5_pct<-sum_q40.5$count[2]/sum(sum_q40.5$count[1:2])
NJ_q40.5_pct<-sum_q40.5$count[4]/sum(sum_q40.5$count[3:4])
sum_q40.5_tbl<-as.data.frame(rbind(GA_q40.5_pct,NJ_q40.5_pct))
sum_q40.5_tbl$state<-c("GA", "NJ")
sum_q40.5_tbl$Quest<-rep("Q40.5",dim(sum_q40.5_tbl)[1])






#Q40.6:
b_s_c_40.6<-group_by(by_state_bcat, Q1, Q40.6)
sum_q40.6<-summarise(b_s_c_40.6, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40.6_pct<-sum_q40.6$count[2]/sum(sum_q40.6$count[1:2])
NJ_q40.6_pct<-sum_q40.6$count[4]/sum(sum_q40.6$count[3:4])
sum_q40.6_tbl<-as.data.frame(rbind(GA_q40.6_pct,NJ_q40.6_pct))
sum_q40.6_tbl$state<-c("GA", "NJ")
sum_q40.6_tbl$Quest<-rep("Q40.6",dim(sum_q40.6_tbl)[1])




#Q40.7:
b_s_c_40.7<-group_by(by_state_bcat,  Q1, Q40.7)
sum_q40.7<-summarise(b_s_c_40.7, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40.7_pct<-sum_q40.7$count[2]/sum(sum_q40.7$count[1:2])
NJ_q40.7_pct<-sum_q40.7$count[4]/sum(sum_q40.7$count[3:4])
sum_q40.7_tbl<-as.data.frame(rbind(GA_q40.7_pct,NJ_q40.7_pct))
sum_q40.7_tbl$state<-c("GA", "NJ")
sum_q40.7_tbl$Quest<-rep("Q40.7",dim(sum_q40.7_tbl)[1])





#Q40.8:
b_s_c_40.8<-group_by(by_state_bcat,  Q1, Q40.8)
sum_q40.8<-summarise(b_s_c_40.8, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])

GA_q40.8_pct<-sum_q40.8$count[2]/sum(sum_q40.8$count[1:2])
NJ_q40.8_pct<-sum_q40.8$count[4]/sum(sum_q40.8$count[3:4])
sum_q40.8_tbl<-as.data.frame(rbind(GA_q40.8_pct,NJ_q40.8_pct))
sum_q40.8_tbl$state<-c("GA", "NJ")
sum_q40.8_tbl$Quest<-rep("Q40.8",dim(sum_q40.8_tbl)[1])





#######################################################################################################
#Calculating preparedness score

prepared<-cbind(datac$Q15,datac$Q16, datac$Q17, datac$Q18, datac$Q19, datac$Q20, datac$Q21, datac$Q22, datac$Q23,datac$Q24, datac$Q25, datac$Q25.1, datac$Q25.2, datac$Q25.3, datac$Q25.4, datac$Q26, datac$Q27, datac$Q28,datac$Q29, datac$Q30, datac$Q31, datac$Q32, datac$Q34, datac$Q38, datac$Q38.1,  datac$Q38.4, datac$Q39, datac$Q39.1, datac$Q39.3, datac$Q39.4, datac$Q40, datac$Q40.1, datac$Q40.3, datac$Q40.4, datac$Q40.5, datac$Q40.6, datac$Q40.7, datac$Q40.8)  #removed uterine artery embolization, prostaglandin E2 suppositories, and non-pneumatic anti shock garment
datac$prepared<-rowSums(prepared)
datac$logprepared<-log(datac$prepared)
layout()
hist(datac$prepared, breaks=41, xlim=c(10,40))

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode(datac$prepared)

IQR(datac$prepared)


#########################################################
#### test normality

###QQ Plot
### null hypothesis is that it is normally distributed

shapiro.test(datac$prepared)
#Also do a qq plot to test normality



#Creating the regression model


fit <- lm(prepared ~ Q1+birth_cat+student+collaborative+resident+cesarean+Q10+Q12+Q13+Q14, data=datac)
#step <- stepAIC(fit, direction="both")  #stepwise building of the model

#fit<-lm(prepared~birth_cat+student+Q13+Q14, data=datac)

summary(fit)  #summary of the model
vif(fit) # less than 5 are allowed into the model

#### test regression assumptions

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)


##### Testing heteroskedaticity with the studentized Breusch-Pagan test
bptest(fit)  # null hypothesis is that there is homoskedaticity

outlierTest(fit) # Bonferonni p-value for most extreme obs


####### Analysis of Variance

boxplot(datac$prepared~datac$student*datac$resident)

aov.out<-aov(prepared ~ Q11*Q10, data=datac)
TukeyHSD(aov.out)
plot(TukeyHSD(aov.out))

#separating GA and NJ Hospitals.

datac_ga<-datac[datac$Q1=="Georgia",]  # only Georgia hospitals


datac_nj<-datac[datac$Q1=="New Jersey",]  # only New Jersey hospitals


datac_nj

quantile(datac$Q7, c(0, 0.25 , 0.50, 0.75, 1))

quantile(datac_ga$Q7, c(0, 0.25 , 0.75, 1))
quantile(datac_nj$Q7, c(0, 0.25 , 0.75, 1))


newdata_sorted<-datac[order(datac$prepared),]
newdata_sorted_nj<-datac_nj[order(datac_nj$prepared),]
newdata_sorted_ga<-datac_ga[order(datac_ga$prepared),]

#Testing whether the means of the cesarean sections rates for the two states are equal
#perform a variance test
var.test(datac_ga$Q7,datac_nj$Q7)
#If variances are equal, perform a normality text
shapiro.test(datac_ga$Q7)
shapiro.test(datac_nj$Q7)
#perform a t-test to determine if the means are equal
t.test(datac_ga$Q7,datac_nj$Q7)

#Testing whether the means of the preparedness scoresfor the two states are equal
#perform a variance test
var.test(datac_ga$prepared,datac_nj$prepared)
#If variances are equal, perform a normality text
shapiro.test(datac_ga$prepared)
shapiro.test(datac_nj$prepared)
#perform a t-test to determine if the means are equal
t.test(datac_ga$prepared,datac_nj$prepared)


######## K means clustering  ################

cluster_mat<-cbind(datac$Q15,datac$Q16, datac$Q17, datac$Q18, datac$Q19, datac$Q20, datac$Q21, datac$Q22, datac$Q23,datac$Q24, datac$Q25, datac$Q25.1, datac$Q25.2, datac$Q25.3, datac$Q25.4, datac$Q26, datac$Q27, datac$Q28,datac$Q29, datac$Q30, datac$Q31, datac$Q32, datac$Q34, datac$Q38, datac$Q38.1,  datac$Q38.4, datac$Q39, datac$Q39.1, datac$Q39.3, datac$Q39.4, datac$Q40, datac$Q40.1, datac$Q40.3, datac$Q40.4, datac$Q40.5, datac$Q40.6, datac$Q40.7, datac$Q40.8)  #removed uterine artery embolization, prostaglandin E2 suppositories, and non-pneumatic anti shock garment
colnames(cluster_mat)<-c("Q15","Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23","Q24", "Q25", "Q25.1", "Q25.2", "Q25.3", "Q25.4", "Q26", "Q27", "Q28","Q29", "Q30", "Q31", "Q32", "Q34", "Q38", "Q38.1",  "Q38.4", "Q39", "Q39.1", "Q39.3", "Q39.4", "Q40", "Q40.1", "Q40.3", "Q40.4", "Q40.5", "Q40.6", "Q40.7", "Q40.8")

#kmeans cluster
set.seed(3)
results<-kmeans(cluster_mat, 5, nstart=50)

t_cluster_mat<-t(cluster_mat)


fit <- pvclust(cluster_mat, method.hclust="ward",
               method.dist="euclidean")
layout(1,1)
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 


matrix<-cbind(t_cluster_mat,c("Q15","Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23","Q24", "Q25", "Q25.1", "Q25.2", "Q25.3", "Q25.4", "Q26", "Q27", "Q28","Q29", "Q30", "Q31", "Q32", "Q34", "Q38", "Q38.1",  "Q38.4", "Q39", "Q39.1", "Q39.3", "Q39.4", "Q40", "Q40.1", "Q40.3", "Q40.4", "Q40.5", "Q40.6", "Q40.7", "Q40.8"))




datac$prepared<-rowSums(prepared)




### Histograms######
pdf("L:/PPH Project/Baseline Assessment/histogram.pdf")

c <- ggplot(datac, aes(x=prepared))
c<-c + geom_histogram(colour="dark green", binwidth=1, fill="white")+xlim(0, 40)

plot(c)

dev.off()


####### Graph of preparedness vs. African American percentages
data_aa<-datac[datac$collaborative==1,]
p<-ggplot(data_aa, aes(x=AA_.,y=prepared))+geom_point()+scale_colour_brewer(palette="Set1")
p+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)+xlab("% of mothers who are African American")+
  ylab("Number of Preparedness Elements")+ggtitle("Number of PPH Preparedness Elements vs. % of Mothers who are African American")



fit <- lm(prepared ~ AA_., data=data_aa)

vif(fit) # less than 5 are allowed into the model

#### test regression assumptions

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)


##### Testing heteroskedaticity with the studentized Breusch-Pagan test
bptest(fit)  # null hypothesis is that there is homoskedaticity

outlierTest(fit) # Bonferonni p-value for most extreme obs




data_aa_ga<-data_aa[data_aa$Q1=="Georgia",]  # only Georgia hospitals

fit_aa_ga<-lm(prepared ~ AA_., data=data_aa_ga)

data_aa_nj<-data_aa[data_aa$Q1=="New Jersey",]  # only New Jersey hospitals

fit_aa_nj<-lm(prepared ~ AA_., data=data_aa_nj)


########## for overall analysis of the initial data for the data panel  ##############################################

setwd("L:/PPH Project/data/baseline summary data")

#count of hospitals by state and birth category

write.csv(summarise(by_state_bcat,count=n()),file="birth_summary.csv")


#Q14
b_s_c_14<-group_by(datac,Q1,Q14)
write.csv(summarise(b_s_c_14,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_14.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q14"), measure=c("Q14"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q14,length,margins=TRUE),file="Q14.csv")


#Q15
b_s_c_15<-group_by(datac,Q1,Q15)
write.csv(summarise(b_s_c_15,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_15.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q15"), measure=c("Q15"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q15,length,margins=TRUE),file="Q15.csv")


#Q16
b_s_c_16<-group_by(datac,Q1,Q16)
write.csv(summarise(b_s_c_16,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_16.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q16"), measure=c("Q16"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q16,length,margins=TRUE),file="Q16.csv")

#Q17
b_s_c_17<-group_by(datac,Q1,Q17,Q17.1, Q17.2, Q17.3, Q17.4, Q17.5, Q17.6, Q17.7, Q17.8, Q17.9, Q17.10)
write.csv(summarise(b_s_c_17,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_17.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17"), measure=c("Q17"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17,length,margins=TRUE),file="Q17.csv")

#Q17.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.1"), measure=c("Q17.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.1,length,margins=TRUE),file="Q17_1.csv")

#Q17.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.2"), measure=c("Q17.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.2,length,margins=TRUE),file="Q17_2.csv")

#Q17.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.3"), measure=c("Q17.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.3,length,margins=TRUE),file="Q17_3.csv")

#Q17.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.4"), measure=c("Q17.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.4,length,margins=TRUE),file="Q17_4.csv")

#Q17.5
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.5"), measure=c("Q17.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.5,length,margins=TRUE),file="Q17_5.csv")

#Q17.6
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.6"), measure=c("Q17.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.6,length,margins=TRUE),file="Q17_6.csv")

#Q17.7
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.7"), measure=c("Q17.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.7,length,margins=TRUE),file="Q17_7.csv")

#Q17.8
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.8"), measure=c("Q17.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.8,length,margins=TRUE),file="Q17_8.csv")

#Q17.9
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.9"), measure=c("Q17.9"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.9,length,margins=TRUE),file="Q17_9.csv")

#Q17.10
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q17.10"), measure=c("Q17.10"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q17.10,length,margins=TRUE),file="Q17_10.csv")


#Q18
b_s_c_18<-group_by(datac,Q1,Q18)
write.csv(summarise(b_s_c_18,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_18.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q18"), measure=c("Q18"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q18,length,margins=TRUE),file="Q18.csv")

#Q19
b_s_c_19<-group_by(datac,Q1,Q19)
write.csv(summarise(b_s_c_19,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_19.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q19"), measure=c("Q19"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q19,length,margins=TRUE),file="Q19.csv")

#Q20
b_s_c_20<-group_by(datac,Q1,Q20)
write.csv(summarise(b_s_c_20,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_20.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q20"), measure=c("Q20"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q20,length,margins=TRUE),file="Q20.csv")

#Q21
b_s_c_21<-group_by(datac,Q1,Q21)
write.csv(summarise(b_s_c_21,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_21.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q21"), measure=c("Q21"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q21,length,margins=TRUE),file="Q21.csv")

#Q22
b_s_c_22<-group_by(datac,Q1,Q22)
write.csv(summarise(b_s_c_22,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_22.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q22"), measure=c("Q22"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q22,length,margins=TRUE),file="Q22.csv")

#Q23
b_s_c_23<-group_by(datac,Q1,Q23)
write.csv(summarise(b_s_c_23,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_23.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q23"), measure=c("Q23"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q23,length,margins=TRUE),file="Q23.csv")

#Q24
b_s_c_24<-group_by(datac,Q1,Q24)
write.csv(summarise(b_s_c_24,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_24.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q24"), measure=c("Q24"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q24,length,margins=TRUE),file="Q24.csv")



#Q25
b_s_c_25<-group_by(datac,Q1,Q25,Q25.1, Q25.2, Q25.3, Q25.4)
write.csv(summarise(b_s_c_25,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_25.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q25"), measure=c("Q25"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q25,length,margins=TRUE),file="Q25.csv")

#Q25.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q25.1"), measure=c("Q25.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q25.1,length,margins=TRUE),file="Q25_1.csv")

#Q25.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q25.2"), measure=c("Q25.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q25.2,length,margins=TRUE),file="Q25_2.csv")

#Q25.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q25.3"), measure=c("Q25.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q25.3,length,margins=TRUE),file="Q25_3.csv")

#Q25.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q25.4"), measure=c("Q25.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q25.4,length,margins=TRUE),file="Q25_4.csv")


#Q26
b_s_c_26<-group_by(datac,Q1,Q26)
write.csv(summarise(b_s_c_26,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_26.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q26"), measure=c("Q26"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q26,length,margins=TRUE),file="Q26.csv")

#Q27
b_s_c_27<-group_by(datac,Q1,Q27)
write.csv(summarise(b_s_c_27,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_27.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q27"), measure=c("Q27"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q27,length,margins=TRUE),file="Q27.csv")

#Q28
b_s_c_28<-group_by(datac,Q1,Q28)
write.csv(summarise(b_s_c_28,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_28.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q28"), measure=c("Q28"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q28,length,margins=TRUE),file="Q28.csv")


#Q29
b_s_c_29<-group_by(datac,Q1,Q29)
write.csv(summarise(b_s_c_29,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_29.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q29"), measure=c("Q29"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q29,length,margins=TRUE),file="Q29.csv")


#Q30
b_s_c_30<-group_by(datac,Q1,Q30)
write.csv(summarise(b_s_c_30,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_30.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q30"), measure=c("Q30"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q30,length,margins=TRUE),file="Q30.csv")


#Q31
b_s_c_31<-group_by(datac,Q1,Q31)
write.csv(summarise(b_s_c_31,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_31.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q31"), measure=c("Q31"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q31,length,margins=TRUE),file="Q31.csv")


#Q32
b_s_c_32<-group_by(datac,Q1,Q32)
write.csv(summarise(b_s_c_32,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_32.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q32"), measure=c("Q32"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q32,length,margins=TRUE),file="Q32.csv")

#Q33
b_s_c_33<-group_by(datac,Q1,birth_cat,Q33)
write.csv(summarise(b_s_c_33,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33"), measure=c("Q33"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33,length,margins=TRUE),file="Q33.csv")


#Q33.1
b_s_c_33.1<-group_by(datac,Q1,birth_cat,Q33.1)
write.csv(summarise(b_s_c_33.1,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_1.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.1"), measure=c("Q33.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.1,length,margins=TRUE),file="Q33_1.csv")


#Q33.2
b_s_c_33.2<-group_by(datac,Q1,birth_cat,Q33.2)
write.csv(summarise(b_s_c_33.2,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_2.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.2"), measure=c("Q33.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.2,length,margins=TRUE),file="Q33_2.csv")


#Q33.3
b_s_c_33.3<-group_by(datac,Q1,birth_cat,Q33.3)
write.csv(summarise(b_s_c_33.3,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_3.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.3"), measure=c("Q33.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.3,length,margins=TRUE),file="Q33_3.csv")



#Q33.4
b_s_c_33.4<-group_by(datac,Q1,birth_cat,Q33.4)
write.csv(summarise(b_s_c_33.4,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_4.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.4"), measure=c("Q33.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.4,length,margins=TRUE),file="Q33_4.csv")


#Q33.5
b_s_c_33.5<-group_by(datac,Q1,birth_cat,Q33.5)
write.csv(summarise(b_s_c_33.5,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_5.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.5"), measure=c("Q33.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.5,length,margins=TRUE),file="Q33_5.csv")


#Q33.6
b_s_c_33.6<-group_by(datac,Q1,birth_cat,Q33.6)
write.csv(summarise(b_s_c_33.6,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_6.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.6"), measure=c("Q33.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.6,length,margins=TRUE),file="Q33_6.csv")


#Q33.7
b_s_c_33.7<-group_by(datac,Q1,birth_cat,Q33.7)
write.csv(summarise(b_s_c_33.7,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_7.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.7"), measure=c("Q33.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.7,length,margins=TRUE),file="Q33_7.csv")


#Q33.8
b_s_c_33.8<-group_by(datac,Q1,birth_cat,Q33.8)
write.csv(summarise(b_s_c_33.8,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_8.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.8"), measure=c("Q33.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.8,length,margins=TRUE),file="Q33_8.csv")


#Q33.9
b_s_c_33.9<-group_by(datac,Q1,birth_cat,Q33.9)
write.csv(summarise(b_s_c_33.9,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_9.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.9"), measure=c("Q33.9"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.9,length,margins=TRUE),file="Q33_9.csv")


#Q33.10
b_s_c_33.10<-group_by(datac,Q1,birth_cat,Q33.4)
write.csv(summarise(b_s_c_33.10,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_33_10.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q33.10"), measure=c("Q33.10"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q33.10,length,margins=TRUE),file="Q33_10.csv")


#Q34
b_s_c_34<-group_by(datac,Q1,Q34)
write.csv(summarise(b_s_c_34,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_34.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q34"), measure=c("Q34"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q34,length,margins=TRUE),file="Q34.csv")

#Q35
b_s_c_35<-group_by(datac,Q1,birth_cat,Q35)
write.csv(summarise(b_s_c_35,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35"), measure=c("Q35"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35,length,margins=TRUE),file="Q35.csv")

#Q35.1
b_s_c_35.1<-group_by(datac,Q1,birth_cat, Q35.1)
write.csv(summarise(b_s_c_35.1,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_1.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.1"), measure=c("Q35.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.1,length,margins=TRUE),file="Q35_1.csv")


#Q35.2
b_s_c_35.2<-group_by(datac,Q1,birth_cat, Q35.2)
write.csv(summarise(b_s_c_35.2,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_2.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.2"), measure=c("Q35.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.2,length,margins=TRUE),file="Q35_2.csv")


#Q35.3
b_s_c_35.3<-group_by(datac,Q1,birth_cat, Q35.3)
write.csv(summarise(b_s_c_35.3,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_3.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.3"), measure=c("Q35.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.3,length,margins=TRUE),file="Q35_3.csv")


#Q35.4
b_s_c_35.4<-group_by(datac,Q1,birth_cat, Q35.4)
write.csv(summarise(b_s_c_35.4,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_4.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.4"), measure=c("Q35.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.4,length,margins=TRUE),file="Q35_4.csv")

#Q35.5
b_s_c_35.5<-group_by(datac,Q1,birth_cat, Q35.5)
write.csv(summarise(b_s_c_35.5,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_5.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.5"), measure=c("Q35.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.5,length,margins=TRUE),file="Q35_5.csv")


#Q35.6
b_s_c_35.6<-group_by(datac,Q1,birth_cat, Q35.6)
write.csv(summarise(b_s_c_35.6,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_6.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.6"), measure=c("Q35.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.6,length,margins=TRUE),file="Q35_6.csv")


#Q35.7
b_s_c_35.7<-group_by(datac,Q1,birth_cat, Q35.7)
write.csv(summarise(b_s_c_35.7,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_7.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.7"), measure=c("Q35.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.7,length,margins=TRUE),file="Q35_7.csv")

#Q35.8
b_s_c_35.8<-group_by(datac,Q1,birth_cat, Q35.8)
write.csv(summarise(b_s_c_35.8,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_35_8.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q35.8"), measure=c("Q35.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q35.8,length,margins=TRUE),file="Q35_8.csv")


#Q36
b_s_c_36<-group_by(datac,Q1,birth_cat, Q36)
write.csv(summarise(b_s_c_36,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_36.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q36"), measure=c("Q36"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q36,length,margins=TRUE),file="Q36.csv")


#Q36.1
b_s_c_36.1<-group_by(datac,Q1,birth_cat, Q36.1)
write.csv(summarise(b_s_c_36.1,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_36_1.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q36.1"), measure=c("Q36.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q36.1,length,margins=TRUE),file="Q36_1.csv")


#Q36.2
b_s_c_36.2<-group_by(datac,Q1,birth_cat, Q36.2)
write.csv(summarise(b_s_c_36.2,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_36_2.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q36.2"), measure=c("Q36.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q36.2,length,margins=TRUE),file="Q36_2.csv")


#Q36.3
b_s_c_36.3<-group_by(datac,Q1,birth_cat, Q36.3)
write.csv(summarise(b_s_c_36.3,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_36_3.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q36.3"), measure=c("Q36.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q36.3,length,margins=TRUE),file="Q36_3.csv")



#Q37
b_s_c_37<-group_by(datac,Q1,birth_cat,Q37)
write.csv(summarise(b_s_c_37,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_37.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q37"), measure=c("Q37"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q37,length,margins=TRUE),file="Q37.csv")

#Q38
b_s_c_38<-group_by(datac,Q1,birth_cat, Q38)
write.csv(summarise(b_s_c_38,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_38.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q38"), measure=c("Q38"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q38,length,margins=TRUE),file="Q38.csv")


#Q38.1
b_s_c_38.1<-group_by(datac,Q1,birth_cat, Q38.1)
write.csv(summarise(b_s_c_38.1,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_38_1.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q38.1"), measure=c("Q38.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q38.1,length,margins=TRUE),file="Q38_1.csv")


#Q38.2
b_s_c_38.2<-group_by(datac,Q1,birth_cat, Q38.2)
write.csv(summarise(b_s_c_38.2,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_38_2.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q38.2"), measure=c("Q38.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q38.2,length,margins=TRUE),file="Q38_2.csv")


#Q38.3
b_s_c_38.3<-group_by(datac,Q1,birth_cat, Q38.3)
write.csv(summarise(b_s_c_38.3,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_38_3.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q38.3"), measure=c("Q38.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q38.3,length,margins=TRUE),file="Q38_3.csv")


#Q38.4
b_s_c_38.4<-group_by(datac,Q1,birth_cat, Q38.4)
write.csv(summarise(b_s_c_38.4,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_38_4.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q38.4"), measure=c("Q38.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q38.4,length,margins=TRUE),file="Q38_4.csv")


#Q39
b_s_c_39<-group_by(datac,Q1,birth_cat, Q39)
write.csv(summarise(b_s_c_39,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_39.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q39"), measure=c("Q39"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q39,length,margins=TRUE),file="Q39.csv")

#Q39.1
b_s_c_39.1<-group_by(datac,Q1,birth_cat, Q39.1)
write.csv(summarise(b_s_c_39.1,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_39_1.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q39.1"), measure=c("Q39.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q39.1,length,margins=TRUE),file="Q39_1.csv")

#Q39.2
b_s_c_39.2<-group_by(datac,Q1,birth_cat, Q39.2)
write.csv(summarise(b_s_c_39.2,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_39_2.csv")


datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q39.2"), measure=c("Q39.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q39.2,length,margins=TRUE),file="Q39_2.csv")

#Q39.3
b_s_c_39.3<-group_by(datac,Q1,birth_cat, Q39.3)
write.csv(summarise(b_s_c_39.3,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_39_3.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q39.3"), measure=c("Q39.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q39.3,length,margins=TRUE),file="Q39_3.csv")


#Q39.4
b_s_c_39.4<-group_by(datac,Q1,birth_cat, Q39.4)
write.csv(summarise(b_s_c_39.4,count=n(),pct_of_total=n()/dim(datac)[1]),file="b_s_c_39_4.csv")

datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q39.4"), measure=c("Q39.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q39.4,length,margins=TRUE),file="Q39_4.csv")

#Q40
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40"), measure=c("Q40"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40,length,margins=TRUE),file="Q40.csv")

#Q40.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.1"), measure=c("Q40.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.1,length,margins=TRUE),file="Q40_1.csv")

#Q40.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.2"), measure=c("Q40.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.2,length,margins=TRUE),file="Q40_2.csv")

#Q40.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.3"), measure=c("Q40.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.3,length,margins=TRUE),file="Q40_3.csv")

#Q40.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.4"), measure=c("Q40.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.4,length,margins=TRUE),file="Q40_4.csv")

#Q40.5
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.5"), measure=c("Q40.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.5,length,margins=TRUE),file="Q40_5.csv")

#Q40.6
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.6"), measure=c("Q40.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.6,length,margins=TRUE),file="Q40_6.csv")

#Q40.7
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.7"), measure=c("Q40.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.7,length,margins=TRUE),file="Q40_7.csv")

#Q40.8
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q40.8"), measure=c("Q40.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q40.8,length,margins=TRUE),file="Q40_8.csv")

#Q41
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41"), measure=c("Q41"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41,length,margins=TRUE),file="Q41.csv")

#Q41.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.1"), measure=c("Q41.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.1,length,margins=TRUE),file="Q41_1.csv")

#Q41.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.2"), measure=c("Q41.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.2,length,margins=TRUE),file="Q41_2.csv")

#Q41.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.3"), measure=c("Q41.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.3,length,margins=TRUE),file="Q41_3.csv")

#Q41.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.4"), measure=c("Q41.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.4,length,margins=TRUE),file="Q41_4.csv")

#Q41.5
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.5"), measure=c("Q41.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.5,length,margins=TRUE),file="Q41_5.csv")

#Q41.6
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.6"), measure=c("Q41.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.6,length,margins=TRUE),file="Q41_6.csv")

#Q41.7
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.7"), measure=c("Q41.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.7,length,margins=TRUE),file="Q41_7.csv")

#Q41.8
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.8"), measure=c("Q41.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.8,length,margins=TRUE),file="Q41_8.csv")

#Q41.9
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.9"), measure=c("Q41.9"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.9,length,margins=TRUE),file="Q41_9.csv")

#Q41.10
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.10"), measure=c("Q41.10"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.10,length,margins=TRUE),file="Q41_10.csv")

#Q41.11
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.11"), measure=c("Q41.11"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.11,length,margins=TRUE),file="Q41_11.csv")

#Q41.12   
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q41.12"), measure=c("Q41.12"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q41.12,length,margins=TRUE),file="Q41_12.csv")

#Q42
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42"), measure=c("Q42"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42,length,margins=TRUE),file="Q42.csv")

#Q42.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.1"), measure=c("Q42.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.1,length,margins=TRUE),file="Q42_1.csv")

#Q42.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.2"), measure=c("Q42.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.2,length,margins=TRUE),file="Q42_2.csv")

#Q42.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.3"), measure=c("Q42.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.3,length,margins=TRUE),file="Q42_3.csv")

#Q42.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.4"), measure=c("Q42.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.4,length,margins=TRUE),file="Q42_4.csv")

#Q42.5
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.5"), measure=c("Q42.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.5,length,margins=TRUE),file="Q42_5.csv")

#Q42.6
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.6"), measure=c("Q42.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.6,length,margins=TRUE),file="Q42_6.csv")

#Q42.7
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.7"), measure=c("Q42.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.7,length,margins=TRUE),file="Q42_7.csv")

#Q42.8
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.8"), measure=c("Q42.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.8,length,margins=TRUE),file="Q42_8.csv")

#Q42.9
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q42.9"), measure=c("Q42.9"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q42.9,length,margins=TRUE),file="Q42_9.csv")

#Q43
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43"), measure=c("Q43"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43,length,margins=TRUE),file="Q43.csv")


#Q43.1
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.1"), measure=c("Q43.1"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.1,length,margins=TRUE),file="Q43_1.csv")

#Q43.2
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.2"), measure=c("Q43.2"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.2,length,margins=TRUE),file="Q43_2.csv")

#Q43.3
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.3"), measure=c("Q43.3"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.3,length,margins=TRUE),file="Q43_3.csv")

#Q43.4
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.4"), measure=c("Q43.4"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.4,length,margins=TRUE),file="Q43_4.csv")

#Q43.5
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.5"), measure=c("Q43.5"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.5,length,margins=TRUE),file="Q43_5.csv")

#Q43.6
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.6"), measure=c("Q43.6"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.6,length,margins=TRUE),file="Q43_6.csv")

#Q43.7
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.7"), measure=c("Q43.7"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.7,length,margins=TRUE),file="Q43_7.csv")

#Q43.8
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.8"), measure=c("Q43.8"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.8,length,margins=TRUE),file="Q43_8.csv")

#Q43.9
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.9"), measure=c("Q43.9"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.9,length,margins=TRUE),file="Q43_9.csv")

#Q43.10
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.10"), measure=c("Q43.10"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.10,length,margins=TRUE),file="Q43_10.csv")

#Q43.11
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.11"), measure=c("Q43.11"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.11,length,margins=TRUE),file="Q43_11.csv")

#Q43.12
datam<-melt(datac,id=c("RespondentID","Q1","birth_cat","Q43.12"), measure=c("Q43.12"),na.rm=TRUE)
write.csv(cast(datam,Q1+birth_cat~Q43.12,length,margins=TRUE),file="Q43_12.csv")

############################Create histograms for the paper##################################
#definiton bar graph

tbl_def<-rbind(sum_q15_tbl, sum_q16_tbl, sum_q17_tbl, sum_q18_tbl, sum_q19_tbl, sum_q26_tbl, sum_q27_tbl)

pdf("L:/PPH Project/Baseline Assessment/def_tbl.pdf")

c<-ggplot(tbl_def,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()


#QBL Risk Assessments

tbl_RA<-rbind(sum_q20_tbl, sum_q21_tbl, sum_q22_tbl, sum_q23_tbl, sum_q24_tbl)

pdf("L:/PPH Project/Baseline Assessment/RA_tbl.pdf")

c<-ggplot(tbl_RA,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()



#Leading QI Initiatives

tbl_QI<-rbind(sum_q25_tbl, sum_q25.1_tbl, sum_q25.2_tbl, sum_q25.3_tbl)

pdf("L:/PPH Project/Baseline Assessment/QI_tbl.pdf")

c<-ggplot(tbl_QI,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()


#QBL bar graph

tbl_QBL<-rbind(sum_q28_tbl, sum_q29_tbl, sum_q31_tbl, sum_q32_tbl, sum_q34_tbl)

pdf("L:/PPH Project/Baseline Assessment/QBL_tbl.pdf")

c<-ggplot(tbl_QBL,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()

#Procedures available for Managing PPH

tbl_procedures<-rbind(sum_q39.4_tbl, sum_q39.1_tbl, sum_q39_tbl, sum_q39.3_tbl)

pdf("L:/PPH Project/Baseline Assessment/procedures_tbl.pdf")

c<-ggplot(tbl_procedures,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()

#Equipment available for Managing PPH

tbl_equipment<-rbind(sum_q40_tbl, sum_q40.1_tbl, sum_q40.3_tbl, sum_q40.4_tbl,sum_q40.5_tbl, sum_q40.6_tbl, sum_q40.7_tbl, sum_q40.8_tbl)

pdf("L:/PPH Project/Baseline Assessment/equipment_tbl.pdf")

c<-ggplot(tbl_equipment,aes(x=Quest,y=V1,fill=state))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()

#bar graph figure for the journal article
#Q16
b_s_c_16<-group_by(by_state_bcat, Q16)
sum_q16<-summarise(b_s_c_16, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q16_tbl_2<-sum_q16[sum_q16$Q16=="1",]
sum_q16_tbl_2$Quest<-rep("Q16",dim(sum_q16_tbl_2)[1])
sum_q16_tbl_2<-sum_q16_tbl_2[,-1]

#Q18
b_s_c_18<-group_by(by_state_bcat,  Q18)
sum_q18<-summarise(b_s_c_18, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q18_tbl_2<-sum_q18[sum_q18$Q18=="1",]
sum_q18_tbl_2$Quest<-rep("Q18",dim(sum_q18_tbl_2)[1])
sum_q18_tbl_2<-sum_q18_tbl_2[,-1]

#Q20
b_s_c_20<-group_by(by_state_bcat, Q20)
sum_q20<-summarise(b_s_c_20, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q20_tbl_2<-sum_q20[sum_q20$Q20=="1",]
sum_q20_tbl_2$Quest<-rep("Q20",dim(sum_q20_tbl_2)[1])
sum_q20_tbl_2<-sum_q20_tbl_2[,-1]

#Q21
b_s_c_21<-group_by(by_state_bcat, Q21)
sum_q21<-summarise(b_s_c_21, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q21_tbl_2<-sum_q21[sum_q21$Q21=="1",]
sum_q21_tbl_2$Quest<-rep("Q21",dim(sum_q21_tbl_2)[1])
sum_q21_tbl_2<-sum_q21_tbl_2[,-1]


#Q23
b_s_c_23<-group_by(by_state_bcat, Q23)
sum_q23<-summarise(b_s_c_23, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q23_tbl_2<-sum_q23[sum_q23$Q23=="1",]
sum_q23_tbl_2$Quest<-rep("Q23",dim(sum_q23_tbl_2)[1])
sum_q23_tbl_2<-sum_q23_tbl_2[,-1]


#Q26: Does your hospital have a P and P about the active management of the third stage of labor?
b_s_c_26<-group_by(by_state_bcat, Q26)
sum_q26<-summarise(b_s_c_26, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q26_tbl_2<-sum_q26[sum_q26$Q26=="1",]
sum_q26_tbl_2$Quest<-rep("Q26",dim(sum_q26_tbl_2)[1])
sum_q26_tbl_2<-sum_q26_tbl_2[,-1]


#Q28: Which of the choices below primarly describes how maternal blood loss at your hospital is evaluated within the first 1-2 hours after vaginal birth?
b_s_c_28<-group_by(by_state_bcat, Q28)
sum_q28<-summarise(b_s_c_28, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q28_tbl_2<-sum_q28[sum_q28$Q28=="1",]
sum_q28_tbl_2$Quest<-rep("Q28",dim(sum_q28_tbl_2)[1])
sum_q28_tbl_2<-sum_q28_tbl_2[,-1]

#Q31: How often does your hospital perform a debriefing of a PPH emergency?
b_s_c_31<-group_by(by_state_bcat,Q31)
sum_q31<-summarise(b_s_c_31, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q31_tbl_2<-sum_q31[sum_q31$Q31=="1",]
sum_q31_tbl_2$Quest<-rep("Q31",dim(sum_q31_tbl_2)[1])
sum_q31_tbl_2<-sum_q31_tbl_2[,-1]


#Q34: Does your hospital run PPH simulation drills?
b_s_c_34<-group_by(by_state_bcat, Q34)
sum_q34<-summarise(b_s_c_34, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q34_tbl_2<-sum_q34[sum_q34$Q34=="1",]
sum_q34_tbl_2$Quest<-rep("Q34",dim(sum_q34_tbl_2)[1])
sum_q34_tbl_2<-sum_q34_tbl_2[,-1]


#Q39.1: 
b_s_c_39.1<-group_by(by_state_bcat,Q39.1)
sum_q39.1<-summarise(b_s_c_39.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q39.1_tbl_2<-sum_q39.1[sum_q39.1$Q39.1=="1",]
sum_q39.1_tbl_2$Quest<-rep("Q39.1",dim(sum_q39.1_tbl_2)[1])
sum_q39.1_tbl_2<-sum_q39.1_tbl_2[,-1]


#Q40.1:
b_s_c_40.1<-group_by(by_state_bcat,Q40.1)
sum_q40.1<-summarise(b_s_c_40.1, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q40.1_tbl_2<-sum_q40.1[sum_q40.1$Q40.1=="1",]
sum_q40.1_tbl_2$Quest<-rep("Q40.1",dim(sum_q40.1_tbl_2)[1])
sum_q40.1_tbl_2<-sum_q40.1_tbl_2[,-1]


#Q40.7:
b_s_c_40.7<-group_by(by_state_bcat,  Q40.7)
sum_q40.7<-summarise(b_s_c_40.7, count=n(), pct_of_total=n()/dim(by_state_bcat)[1])
sum_q40.7_tbl_2<-sum_q40.7[sum_q40.7$Q40.7=="1",]
sum_q40.7_tbl_2$Quest<-rep("Q40.7",dim(sum_q40.7_tbl_2)[1])
sum_q40.7_tbl_2<-sum_q40.7_tbl_2[,-1]


#graphic for paper #1

tbl_paper<-rbind(sum_q16_tbl_2, sum_q18_tbl_2, sum_q26_tbl_2, sum_q20_tbl_2, sum_q21_tbl_2, sum_q23_tbl_2, sum_q28_tbl_2, sum_q31_tbl_2, sum_q34_tbl_2, sum_q39.1_tbl_2, sum_q40.1_tbl_2, sum_q40.7_tbl_2)
tbl_paper$Quest <- factor(tbl_paper$Quest, levels = c("Q16", "Q18", "Q26", "Q20", "Q21", "Q23", "Q28", "Q31", "Q34", "Q39.1", "Q40.1", "Q40.7"))


pdf("L:/PPH Project/Baseline Assessment/paper_tbl.pdf")

c<-ggplot(tbl_paper,aes(x=Quest,y=pct_of_total))+geom_bar(stat="identity", width=0.8, position="dodge")+ylim(0,1)

plot(c)

dev.off()

