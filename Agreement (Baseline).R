rm(list=ls())

# Loading packages

library(dplyr)
library(xtable)
library(caret)
library(psych)

# Importing observation data

  obs <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_obs_data_baseline.csv", colClasses=c(Q01_CodeObs_b="character"))

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

  ints <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_int_data_baseline.csv", colClasses=c(Q05_ID="character"))

  names(ints) <- paste("i_", names(ints), sep="") 
  ints <- rename(ints, ID = i_Q05_ID )
  ints$i_Q214_contrepression[ints$i_Q214_contrepression == 
                             "Le placenta accouche immediatement apres naissance sans assistance"] <- "non"
  
# Importing partograph data

  part <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_part_data_baseline.csv", colClasses=c(Code="character"))

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
                
                #Percent agreement
                col4 <- (t[1,1] + t[2, 2]) / ( t[1,1] + t[1,2] + t[2,1] + t[2,2] ) * 100
                
                #Cohen's kappa
                col5 <- cohen.kappa(t)$kappa
                  
                #Sensitivity
                col6 <- sensitivity(dtab[,short.var.1], dtab[,short.var.2])
                
                #Specificity
                col7 <- specificity(dtab[,short.var.1], dtab[,short.var.2])
                
                #PPV
                col8 <- (t[1,1]) / ( t[1,1] + t[1,2] )
                #posPredValue(dtab[,short.var.1], dtab[,short.var.2])
                
                #NPV
                col9 <- (t[2,2]) / ( t[2,1] + t[2,2] )
                #negPredValue(dtab[,short.var.1], dtab[,short.var.2])
                
                #Inflation Factor | TAP = (a+b)/(a+c)
                TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])
                col10 <- TAP
                
                c(col1, col2, col3, col4, col5, col6, col7, col8, col9, col10)

}

# Initial Client Assessment

  Stage1 <- rep(NA, 10)
  Q201 <- rm_miss(obs_ints$i_Q201_examine, obs_ints$o_Q112_bc) 
  Q202 <- rm_miss(obs_ints$i_Q202_tension, obs_ints$o_Q110_tensionart)
  Q203 <- rm_miss(obs_ints$i_Q203_temp, obs_ints$o_Q108_temp)
  Q205 <- rm_miss(obs_ints$i_Q205_carte_prenatal, obs_ints$o_Q102_docs)
  Q302 <- rm_miss(obs_ints$i_Q302_expliquer, obs_ints$o_Q112_explain2) 
  Q303 <- rm_miss(obs_ints$i_Q303_questions, obs_ints$o_Q101_motif) # n/a? did workers allow questions

# Intermittent Observation of First Stage of Labor

  Stage2 <- rep(NA, 10) 
  Q304 <- rm_miss(obs_ints$i_Q304_consommer, obs_ints$o_Q226_consommer)
  Q305 <- rm_miss(obs_ints$i_Q305_lever, obs_ints$o_Q227_positions)

# Continuous Observation of Second and Third Stage of Labor

  Stage3 <- rep(NA, 10) 
  Q209 <- rm_miss(obs_ints$i_Q209_injection, obs_ints$o_Q309_utertonique)
  Q212 <- rm_miss(obs_ints$i_Q212_masse, obs_ints$o_Q316_massageuterin)
  Q214 <- rm_miss(obs_ints$i_Q214_contrepression, obs_ints$o_Q315_traction)  #le placenta recode

# Immediate Newborn and Postpartum Care

  Stage4 <- rep(NA, 10)
  Q215 <- rm_miss(obs_ints$i_Q215_seche, obs_ints$o_Q402_seche)
  Q216 <- rm_miss(obs_ints$i_Q216_peau, obs_ints$o_Q407_peau)
  Q218 <- rm_miss(obs_ints$i_Q218_misausein, obs_ints$o_Q412_allaitement)
  Q221 <- rm_miss(obs_ints$i_Q221_temp_tension, obs_ints$o_Q410_vitaux)
  #Q222 <- rm_miss(obs_ints$i_Q222_cordon, obs_ints$o_Q408_temp) #no oui: obs_ints$o_Q408_temp

# Outcomes 

  Stage5 <- rep(NA, 10)
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
                 "Percent agreement", "Cohen's Kappa", "Sensitivity of Self-Report", "Specificity of Self-Report", "Positive Predictive Value", "Negative Predictive Value", "Inflation Factor")

# Getting rid of decimals in N's

  df$N <- as.character(df$N)

# Creating HTML code

  p <-  print(xtable(df, align=replicate(13, "center")), type="html")

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
    
  #Percent agreement
  col4 <- (t[1,1] + t[2, 2]) / ( t[1,1] + t[1,2] + t[2,1] + t[2,2] ) * 100
  
  #Cohen's kappa
  col5 <- cohen.kappa(t)$kappa
  
  #Sensitivity
  col6 <- sensitivity(dtab[,short.var.1], dtab[,short.var.2])
  
  #Specificity
  col7 <- specificity(dtab[,short.var.1], dtab[,short.var.2])
  
  #AUC
  col8 <- (col4+col5)/2
  
  #Inflation Factor | TAP = (a+b)/(a+c)
  TAP = (t[1,1] + t[1, 2]) / (t[1,1] + t[2, 1])
  col9 <- TAP
  
  c(col1, col2, col3, col4, col5, col6, col7, col8, col9)
}

Stage3 <- c( NA, NA, NA, NA, NA, NA, NA, NA, NA)
Q04 <- rm_miss3(part_obs$p_Q04_utertonique, part_obs$o_Q309_utertonique)
Q05 <- rm_miss3(part_obs$p_Q05_tractioncordon, part_obs$o_Q315_traction )
Q06 <- rm_miss3(part_obs$p_Q06_massage, part_obs$o_Q316_massageuterin )

Stage4 <- c( NA, NA, NA, NA, NA, NA, NA, NA, NA)
#Q07 <- rm_miss3(part_obs$p_Q07_crede, part_obs$o_Q414_vitaminek )
Q08 <- rm_miss3(part_obs$p_Q08_soinscordon, part_obs$o_Q406_coupe )
Q09 <- rm_miss3(part_obs$p_Q09_allaitement, part_obs$o_Q412_allaitement )

df3 <- data.frame(rbind(Stage3, Q04,Q05,Q06, Stage4, Q08,Q09)) #Q07,

# Importing Question text

x2 <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/part obs match for import.csv")

Partograph2 <- x2[,1]
Observation2 <- x2[,2]

# Adding observation and interview questions to the data frame   

  df3 <- cbind(Partograph2, Observation2, df3)

# Adding column names   

  names(df3) <- c("Partograph Question", "Observation Question", "N", "Partograph (%)", "Observation (%)", "Percent Agreement", "Cohen's Kappa", "Sensitivity of Self-Report", "Specificity of Self-Report", "AUC", "IF")

  # Getting rid of decimals in N's
  
  df3$N <- as.character(df3$N)
  
# Creating HTML code
  
  print(xtable(df3, type="html", include.rownames=F), type="html")

