rm(list=ls())

# Loading packages

library(Epi)
library(survival)
library(readxl)
library(dplyr)
library(xtable)
library(knitr)
library(rmarkdown)
library(tableHTML)

# Importing observation data

  obs <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_obs_data.csv", colClasses=c(Q01_CodeObs_b="character"))

  names(obs) <- paste("o_", names(obs), sep="")
  obs <- rename(obs, ID = o_Q01_CodeObs_b )
  obs$o_Q531_allaitement <- tolower(obs$o_Q531_allaitement) # changing 'Non' and 'Oui' to lowercase

  obs$o_Q112_bc[obs$o_Q112_b == '' & obs$o_Q112_c == ''] <- ''
  obs$o_Q112_bc[obs$o_Q112_b == '' & obs$o_Q112_c == 'oui'] <- ''
  obs$o_Q112_bc[obs$o_Q112_b == 'oui' & obs$o_Q112_c == 'oui'] <- 'oui'
  obs$o_Q112_bc[obs$o_Q112_b == 'non' | obs$o_Q112_c == 'non'] <- 'non'
 
  obs$o_Q112_bc <- as.factor(obs$o_Q112_bc)
  
  obs$o_Q112_explain2[obs$o_Q205_expliquer == '' & obs$o_Q225_expliquer == ''] <- ''
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'oui' & obs$o_Q225_expliquer == ''] <- 'oui'
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'oui' & obs$o_Q225_expliquer == 'oui'] <- 'oui'
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'non' | obs$o_Q225_expliquer == 'non'] <- 'non'
  
  obs$o_Q112_explain2 <- as.factor(obs$o_Q112_explain2)
  
# Importing interview data

  ints <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_int_data.csv", colClasses=c(Q05_ID="character"))

  names(ints) <- paste("i_", names(ints), sep="") 
  ints <- rename(ints, ID = i_Q05_ID )
  ints$i_Q214_contrepression[ints$i_Q214_contrepression == 
                             "Le placenta accouche immediatement apres naissance sans assistance"] <- "non"
  
# Importing partograph data

  part <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_part_data.csv", colClasses=c(Code="character"))

  names(part) <- paste("p_", names(part), sep="")
  part <- rename(part, ID = p_Code )

# Creating merged datasets 
  
  obs_ints <- inner_join(obs, ints, by = "ID")
  part_ints <- inner_join(part, ints, by = "ID")
  part_obs <- inner_join(part, obs, by = "ID")
  

# Function to create table
  
rm_miss <- function(var1, var2) {
                dtab <- obs_ints[var1 != "" & var2 != "" & 
                           var1 != "ne sait pais" & var2 != "ne sait pais" &
                           var1 != "ne sait pas" & var2 != "ne sait pas" &
                           var1 != "ne sait pas/peut pas rappelez-vous" & 
                           var2 != "ne sait pas/peut pas rappelez-vous" &
                           var1 != "non applicable" & var2 != "non applicable" ,] 
                
                short.var.1 <- substring(deparse(substitute(var1)), 10, 50)
                short.var.2 <- substring(deparse(substitute(var2)), 10, 50)
              
                # Re-ordering factor levels
                dtab[,short.var.1] <- factor(dtab[,short.var.1], levels = c("oui", "non"))
                dtab[,short.var.2] <- factor(dtab[,short.var.2], levels = c("oui", "non"))
                 
                # Selecting columns
                dtab <<- dtab[,c(substring(deparse(substitute(var1)), 10, 50), substring(deparse(substitute(var2)), 10, 50) )]
                
                #N
                t = table(dtab[,short.var.1], dtab[,short.var.2])
                N = t[1,1] + t[1,2] + t[2,1] + t[2,2]
                col1 <- N
                
                #Reported Prevalence
                t.i =  table(dtab[,short.var.1])
                col2 <- t.i[1] / ( t.i[1] + t.i[2] ) * 100
                
                #True Prevalence
                t.o =  table(dtab[,short.var.2])
                col3 <- t.o[1] / ( t.o[1] + t.o[2] ) * 100
                
                #Sensitivity
                col4 <- sensitivity(dtab[,short.var.1], dtab[,short.var.2])
                
                #Specificity
                col5 <- specificity(dtab[,short.var.1], dtab[,short.var.2])
                
                #AUC
                col6 <- (col4+col5)/2
                
                #Inflation Factor | TAP = (a+b)/(a+c)
                TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])
                col7 <- TAP
                
                c(col1, col2, col3, col4, col5, col6, col7)

}

# Initial Client Assessment

  Stage1 <- c( NA, NA, NA, NA, NA, NA, NA) 
  Q201 <- rm_miss(obs_ints$i_Q201_examine, obs_ints$o_Q112_bc) 
  Q202 <- rm_miss(obs_ints$i_Q202_tension, obs_ints$o_Q110_tensionart)
  Q203 <- rm_miss(obs_ints$i_Q203_temp, obs_ints$o_Q108_temp)
  Q205 <- rm_miss(obs_ints$i_Q205_carte_prenatal, obs_ints$o_Q102_docs)
  Q302 <- rm_miss(obs_ints$i_Q302_expliquer, obs_ints$o_Q112_explain2) 
  Q303 <- rm_miss(obs_ints$i_Q303_questions, obs_ints$o_Q101_motif) # n/a? did workers allow questions

# Intermittent Observation of First Stage of Labor

  Stage2 <- c( NA, NA, NA, NA, NA, NA, NA)  
  Q304 <- rm_miss(obs_ints$i_Q304_consommer, obs_ints$o_Q226_consommer)
  Q305 <- rm_miss(obs_ints$i_Q305_lever, obs_ints$o_Q227_positions)

# Continuous Observation of Second and Third Stage of Labor

  Stage3 <- c( NA, NA, NA, NA, NA, NA, NA) 
  Q209 <- rm_miss(obs_ints$i_Q209_injection, obs_ints$o_Q309_utertonique)
  Q212 <- rm_miss(obs_ints$i_Q212_masse, obs_ints$o_Q316_massageuterin)
  Q214 <- rm_miss(obs_ints$i_Q214_contrepression, obs_ints$o_Q315_traction)  #le placenta recode

# Immediate Newborn and Postpartum Care

  Stage4 <- c( NA, NA, NA, NA, NA, NA, NA)
  Q215 <- rm_miss(obs_ints$i_Q215_seche, obs_ints$o_Q402_seche)
  Q216 <- rm_miss(obs_ints$i_Q216_peau, obs_ints$o_Q407_peau)
  Q218 <- rm_miss(obs_ints$i_Q218_misausein, obs_ints$o_Q412_allaitement)
  Q221 <- rm_miss(obs_ints$i_Q221_temp_tension, obs_ints$o_Q410_vitaux)
  #Q222 <- rm_miss(obs_ints$i_Q222_cordon, obs_ints$o_Q408_temp) #no oui: obs_ints$o_Q408_temp

# Outcomes 

  Stage5 <- c( NA, NA, NA, NA, NA, NA, NA) 
  Q208 <- rm_miss(obs_ints$i_Q208_pousser, obs_ints$o_Q503_pression)
  Q308 <- rm_miss(obs_ints$i_Q307_position, obs_ints$o_Q503_verbale)
  Q309 <- rm_miss(obs_ints$i_Q307_position, obs_ints$o_Q503_physical)


# Combining rows into a data frame

  df <- data.frame(rbind(Stage1, Q201, Q202, Q203, Q205, Q302, Q303, Stage2, Q304, Q305, 
                         Stage3, Q209, Q212, Q214, Stage4, Q215, Q216, Q218, Q221, 
                         Stage5, Q208, Q308, Q309))

# Importing Question text
  
 x <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/Obs Ints Match for import.csv")
  
 Interview <- x[,2]
 Observation <- x[,3]
 
# Adding observation and interview questions to the data frame   

  df <- cbind(Interview, Observation,  df)
    
# Adding column names   

  names(df) <- c("Interview Question", "Observation Question", "N", "Interview (%)", "Observation (%)",
               "Sensitivity of Self-Report", "Specificity of Self-Report", "AUC", "IF")

# Getting rid of decimals in N's

  df$N <- as.character(df$N)

# Creating HTML code

  print(xtable(df, type="html", include.rownames=F), type="html")

#-------------------------------------------------------------------------------------------------------------#

# Comparing partograph and observation data

rm_miss3 <- function(var1, var2) {
  
  short.var.1 <<- substring(deparse(substitute(var1)), 10, 50)
  short.var.2 <<- substring(deparse(substitute(var2)), 10, 50)
  
  # Keeping yes/no responses
  dtab <<- part_obs[var1 %in%  c("oui", "non", "Oui", "Non") &
                       var2 %in%  c("oui", "non", "Oui", "Non") ,] 
  
  # Changing to lower case
  dtab[,short.var.1] <<- tolower(dtab[,short.var.1]) 
  dtab[,short.var.2] <<- tolower(dtab[,short.var.2]) 
  
  
  # Selecting columns
  dtab <<- dtab[, c(short.var.1, short.var.2)]
  
  # Re-ordering factor levels
  dtab[,short.var.1] <<- factor(dtab[,short.var.1], levels = c("oui", "non"))
  dtab[,short.var.2] <<- factor(dtab[,short.var.2], levels = c("oui", "non"))
   
  #N
  t = table(dtab[,short.var.1], dtab[,short.var.2])
  N = t[1,1] + t[1,2] + t[2,1] + t[2,2]
  col1 <- N
  
  #Reported Prevalence
  t.i =  table(dtab[,short.var.1])
  col2 <- t.i[1] / ( t.i[1] + t.i[2] ) * 100
  
  #True Prevalence
  t.o =  table(dtab[,short.var.2])
  col3 <- t.o[1] / ( t.o[1] + t.o[2] ) * 100
    
  #Sensitivity
  col4 <- sensitivity(dtab[,short.var.1], dtab[,short.var.2])
  
  #Specificity
  col5 <- specificity(dtab[,short.var.1], dtab[,short.var.2])
  
  #AUC
  col6 <- (col4+col5)/2
  
  #Inflation Factor | TAP = (a+b)/(a+c)
  TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])
  col7 <- TAP
  
  c(col1, col2, col3, col4, col5, col6, col7)
}

Stage3 <- c( NA, NA, NA, NA, NA, NA, NA)
Q04 <- rm_miss3(part_obs$p_Q04_utertonique, part_obs$o_Q309_utertonique)
Q05 <- rm_miss3(part_obs$p_Q05_tractioncordon, part_obs$o_Q315_traction )
Q06 <- rm_miss3(part_obs$p_Q06_massage, part_obs$o_Q316_massageuterin )

Stage4 <- c( NA, NA, NA, NA, NA, NA, NA)
#Q07 <- rm_miss3(part_obs$p_Q07_crede, part_obs$o_Q414_vitaminek )
Q08 <- rm_miss3(part_obs$p_Q08_soinscordon, part_obs$o_Q406_coupe )
Q09 <- rm_miss3(part_obs$p_Q09_allaitement, part_obs$o_Q412_allaitement )

df3 <- data.frame(rbind(Stage3, Q04,Q05,Q06, Stage4, Q08,Q09)) #Q07,

# Importing Question text

x2 <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/part obs match for import.csv")

Partograph2 <- x2[,2]
Observation2 <- x2[,3]

# Adding observation and interview questions to the data frame   

  df3 <- cbind(Partograph2, Observation2, df3)

# Adding column names   

  names(df3) <- c("Partograph Question", "Observation Question", "N", "Partograph (%)", "Observation (%)",
               "Sensitivity of Self-Report", "Specificity of Self-Report", "AUC", "IF")

  # Getting rid of decimals in N's
  
  df3$N <- as.character(df3$N)
  
# Creating HTML code
  
  print(xtable(df3, type="html", include.rownames=F), type="html")



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Partograph <- c(
    "Was a uterotonic administered?",
    "Was controlled cord traction performed?",
    "Was uterine massage after delivery performed?",
    #"CREDE done?",
    "Cord care done?",
    "Q09: Breastfeeding initiated within one hour of delivery?")
  
  Observation <- c(
    "Administers Uterotonic?",
    "Applies traction to the cord while applying suprapubic counter traction",
    "Performs uterine massage immediately following delivery of the placenta, or asks the woman to do the massage",
    #"Administer Vitamin K to newborn",
    "Cuts cord with sterile blade or sterile scissors",
    "Observe breastfeeding initiated within first hour after birth")
  
  
  
  
  # Scrap

  
  tableHTML(df, widths = c(100, 200, rep(5, 8)), theme = 'scientific', 
            second_head=list(11, "Table1"))
  
  
  save(x, file="//cdc.gov/private/L330/ykf1/New folder/DRC/tableHTML.html")
  
  
# First Comparison


# Getting rid of blank responses
Q209    <- obs_ints[obs_ints$i_Q209_injection != "" & obs_ints$o_Q309_utertonique != "" &
                      obs_ints$i_Q209_injection != "ne sait pais" & obs_ints$o_Q309_utertonique != "ne sait pais" ,] %>% 
  select(i_Q209_injection, o_Q309_utertonique) 

# Reordering factor levels
#N
N = t[1,1] + t[1,2] + t[2,1] + t[2,2]

#Reported Prevalence
t.i =  table(Q202$i_Q209_injection)
t.i[1] / ( t.i[1] + t.i[2] )

#True Prevalence
t.o =  table(Q202$o_Q309_utertonique)
t.o[1] / ( t.o[1] + t.o[2] )

#Sensitivity
stats202$res[2,1]

#Specificity
stats202$res[2,2]

#AUC
stats202 = ROC( form =  Q202$o_Q309_utertonique ~ i_Q209_injection , plot="ROC" )
stats202$AUC

#Inflation Factor | TAP = (a+b)/(a+c)
t = table(Q202$i_Q209_injection, Q202$o_Q309_utertonique)
TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])



















































# scrap

# Labeling interview questions

Interview_Question_Numbers <- c(
  "", "Q201", "Q202", "Q203", "Q205", "Q302", "Q303", "" , "Q304", "Q305", "",
  "Q209", "Q212", "Q214" , "",  "Q215", "Q216", "Q218", "Q221", "", "Q208", "Q308", "Q309")

Interview <- c(
  "Initial Client Assessment",
  "Were you examined by the health worker when you first arrived? (This would include feeling your stomach and listening for the baby's heart beat)",
  "After you arrived did someone take your blood pressure?", 
  "After you arrived, did someone take your temperature?",
  "Did the health care worker ask for your prenatal card or referral sheet if you were referred?",
  "Did the health worker(s) explain to you what was happening throughout your delivery?",
  "Did the health care worker(s) allow you to ask questions?",
  
  "Intermittent Observation of First Stage of Labor",
  "Were you encouraged to consume liquid/foods throughout labor?",
  "Did anyone ask you if you wanted to get up and walk around while you were in labor?",
  
  "Continuous Observation of Second and Third Stage of Labor",
  "Were you given an injection immediately after your baby was born?",
  "After the delivery of your baby, did the birth attendant firmly massage your lower abdomen?",
  "Did the birth attendant help you deliver the placenta, that is, did he/she place his/her hand firmly on your lower abdomen with one hand and hold the umbilical cord in the other hand?",
  
  "Immediate Newborn and Postpartum Care",
  "Was your baby wiped/dried off with a towel immediately after his/her birth, within a few minutes of delivery?"  ,
  "Did someone place the baby on your chest, against your skin, immediately after delivery of the baby?" ,
  
  "Was your baby breastfed within the first hour after birth?",
  "Did anyone check your temperature and blood pressure after the baby's delivery?",
  #"Q222: Did anyone check the temperature of the baby and its umbilical cord after the baby's delivery?",
  
  "Outcomes",
  
  "Did anyone help you push by pressing on your stomach?",
  "Did any health center staff shout, insult or threaten you while you were in labor or after delivery?",
  "Did any health center staff slap, hit or pinch you while you were in labor or after delivery?"
  
)

# Labeling observation questions

Observation_Question_Numbers <- c("", "Q112", "Q110", "Q108", "Q102", "Q112", "Q101", "", "Q226", "Q227", 
                                  "", "Q309", "Q316", "Q315", "", "Q402", "Q407", "Q412", "Q410","", 
                                  "Q503", "Q503", "Q503") 
Observation <- c(
  "",
  "Checks fetal presentation by palpation of abdomen AND Checks fetal heart rate with fetoscope",
  "Takes blood pressure",
  "Takes temperature",
  "Provider asks woman if she has documents from a referring facility and/or if she attended prenatal consultations",
  
  "Apply fundal pressure to hasten delivery of baby or placenta",
  "Adminsters uterotonic?",
  "Performs uterine massage immediately following delivery of the placenta, or asks the woman to do the massage",
  "Applies traction to the cord while applying suprapubic counter traction",
  "Immeidately dries baby with towel",
  "Places newborn on the mother's abdomen 'skin-to-skin'",
  "Observe breastfeeding initiated within first hour after birth",
  "Takes mother's vital signs 15 minutes after birth",
  #   "Q408: Checks baby's temperature 15 minutes after birth",
  "Explains procedures to woman (and support person) before proceeding AND Q225: At least once, explains what will happen in labor to woman (support person if present)",
  "Provider asks woman (and support person) if she has any questions",
  "At least once, encourages woman to consume fluids/food during labor",
  "At least once, encourages/assists woman to ambulate and assume different positions during labor",
  "Did You see any of the following harmful or inappropriate practices by health workers?  Answer choice: Shout, insult, or threaten the woman during labor or after",
  "Did You see any of the following harmful or inappropriate practices by health workers?  Answer choice: Did any health center staff slap, hit or pinch you while you were in labor or after delivery?")



#load("//cdc.gov/private/L330/ykf1/New folder/DRC/obs_ints_output.Rdata")
#save(df, file="//cdc.gov/private/L330/ykf1/New folder/DRC/obs_ints_output.Rdata")


# Comparing partograph and interview data

rm_miss2 <- function(var1, var2) {
  
  short.var.1 <<- substring(deparse(substitute(var1)), 11, 50)
  short.var.2 <<- substring(deparse(substitute(var2)), 11, 50)
  
  # Keeping yes/no responses
  dtab <<- part_ints[var1 %in%  c("oui", "non", "Oui", "Non") &
                       var2 %in%  c("oui", "non", "Oui", "Non") ,] 
  
  # Changing to lower case
  dtab[,short.var.1] <<- tolower(dtab[,short.var.1]) 
  dtab[,short.var.2] <<- tolower(dtab[,short.var.2]) 
  
  
  # Selecting columns
  dtab <<- dtab[, c(short.var.1, short.var.2)]
  
  # Re-ordering factor levels
  dtab[,short.var.1] <<- factor(dtab[,short.var.1], levels = c("oui", "non"))
  dtab[,short.var.2] <<- factor(dtab[,short.var.2], levels = c("oui", "non"))
  
  #N
  t = table(dtab[,short.var.1], dtab[,short.var.2])
  N = t[1,1] + t[1,2] + t[2,1] + t[2,2]
  col1 <- N
  
  #Reported Prevalence
  t.i =  table(dtab[,short.var.1])
  col2 <- t.i[1] / ( t.i[1] + t.i[2] ) * 100
  
  #True Prevalence
  t.o =  table(dtab[,short.var.2])
  col3 <- t.o[1] / ( t.o[1] + t.o[2] ) * 100
  
  #Sensitivity
  stats202 = ROC( form =  dtab[,2] ~ dtab[,1] , plot="ROC" )
  col4 <- stats202$res[2,1]
  
  #Specificity
  col5 <- stats202$res[2,2]
  
  #AUC
  col6 <- stats202$AUC
  
  #Inflation Factor | TAP = (a+b)/(a+c)
  TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])
  col7 <- TAP
  
  c(col1, col2, col3, col4, col5, col6, col7)
  
} 

Q06 <- rm_miss2(part_ints$p_Q06_massage, part_ints$i_Q212_masse)
Q09 <- rm_miss2(part_ints$p_Q09_allaitement, part_ints$i_Q219_allaiter)


df2 <- data.frame(rbind(Q06,Q09)) 

names(df2) <- c("N", "Reported Prev (%)", "True Prev (%)",
                "Sensitivity of Self-Report", "Specificity of Self-Report", "AUC", "IF")

df2$N <- as.character(df2$N)

# Creating HTML code


print(xtable(df2, type="html", include.rownames=F), type="html")