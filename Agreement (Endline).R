rm(list=ls())

# Loading packages

library(dplyr)
library(xtable)
library(caret)
library(psych)

# Importing observation data

  obs <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_obs_data_endline.csv",
                  colClasses=c(Q01_CodeObs="character"))

# Changing variables to match baseline syntax
  
  names(obs) <- ifelse(substr(names(obs), 1, 1) == "X", substr(names(obs), 2, 50), substr(names(obs), 1, 50))
  names(obs) <- paste("o_", names(obs), sep="") 
  
  obs <- rename(obs, ID = o_Q01_CodeObs)
  obs <- obs[1:53,]
  
# Creating a new variable to combine Q112B and Q112C
  
  obs$o_Q112_bc[obs$o_Q112_b == '' & obs$o_Q112_c == ''] <- ''
  obs$o_Q112_bc[obs$o_Q112_b == '' & obs$o_Q112_c == 'oui'] <- ''
  obs$o_Q112_bc[obs$o_Q112_b == 'oui' & obs$o_Q112_c == 'oui'] <- 'oui'
  obs$o_Q112_bc[obs$o_Q112_b == 'non' | obs$o_Q112_c == 'non'] <- 'non'
  
  obs$o_Q112_bc <- as.factor(obs$o_Q112_bc)

# Creating a new variable to combine Q205 and Q225
  
  obs$o_Q112_explain2[obs$o_Q205_expliquer == '' & obs$o_Q225_expliquer == ''] <- ''
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'oui' & obs$o_Q225_expliquer == ''] <- 'oui'
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'oui' & obs$o_Q225_expliquer == 'oui'] <- 'oui'
  obs$o_Q112_explain2[obs$o_Q205_expliquer == 'non' | obs$o_Q225_expliquer == 'non'] <- 'non'
  
  obs$o_Q112_explain2 <- as.factor(obs$o_Q112_explain2)
  
# Importing interview data

  ints <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_int_data_endline.csv", 
                   colClasses=c(Q05_CodeAccouchee="character"))

# Changing variables to match baseline syntax
  
  names(ints) <- ifelse(substr(names(ints), 1, 1) == "X", substr(names(ints), 2, 50), substr(names(ints), 1, 50))
  names(ints) <- paste("i_", names(ints), sep="") 
  
  ints <- rename(ints, ID = i_Q05_CodeAccouchee )
  ints <- ints[1:226, ]

# Importing partograph data

  part <- read.csv("//cdc.gov/private/L330/ykf1/New folder/DRC/R2HC_part_data_endline.csv",
                   colClasses=c(Code="character"))

# Changing variables to match baseline syntax
  
  names(part) <- ifelse(substr(names(part), 1, 1) == "X", substr(names(part), 2, 50), substr(names(part), 1, 50))
  names(part) <- paste("p_", names(part), sep="")
  part <- rename(part, ID = p_Code )
  
# Adding 0 to beginning of ID when necessary
  
  part$ID <- ifelse(nchar(part$ID) == 5, paste0("0", part$ID), part$ID) 

# Creating merged datasets 
  
  obs_ints <- inner_join(obs, ints, by = "ID")
  part_ints <- inner_join(part, ints, by = "ID")
  part_obs <- inner_join(part, obs, by = "ID")

# Saving datasets
  
  save(ints, file='C:/DRC/ints_endline.Rdata')
  save(obs, file='C:/DRC/obs_endline.Rdata')
  save(obs_ints, file='C:/DRC/obs_ints_endline.Rdata')
  
# Function to clean data and calculate indicators for AUC table
#
# Args: var1: interview question
#       var2: observation question  

AUC1 <- function(var1, var2) {

                short.var.1 <<- substring(deparse(substitute(var1)), 10, 50)
                short.var.2 <<- substring(deparse(substitute(var2)), 10, 50)
                
                dtab <<- obs_ints[var1 != "" & var2 != "" & var1 != "Ha Juwi/Hi Jibu" & var2 != "ne sait pais" &
                                  var1 != "Sikukuwa na ulizo" & var1 != "Si juwe/si wezi kumbuka" &
                                  var1 != "Kizazi ilitoka Palepale kisha kuzala bila musaada" , ]
                
                # Selecting columns
                dtab <<- dtab[, c(short.var.1, short.var.2)]
                
                # Recoding to account for language
                dtab[short.var.1] <- recode(unlist(dtab[short.var.1]), Ndiyo = "oui", Apana = "non")
                
                # Re-ordering factor levels
                dtab[,short.var.1] <- factor(dtab[,short.var.1], levels = c("oui", "non"))
                dtab[,short.var.2] <- factor(dtab[,short.var.2], levels = c("oui", "non"))

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
  Q201 <- AUC1(obs_ints$i_Q201_examine, obs_ints$o_Q112_bc) 
  Q202 <- AUC1(obs_ints$i_Q202_tension, obs_ints$o_Q110_tensionart)
  Q203 <- AUC1(obs_ints$i_Q203_temp, obs_ints$o_Q108_temp)
  Q205 <- AUC1(obs_ints$i_Q205_carte_prenatal, obs_ints$o_Q102_docs)
  Q302 <- AUC1(obs_ints$i_Q302_expliquer, obs_ints$o_Q112_explain2) 
  Q303 <- AUC1(obs_ints$i_Q303_questions, obs_ints$o_Q101_motif) 

# Intermittent Observation of First Stage of Labor

  Stage2 <- rep(NA, 10)  
  Q304 <- AUC1(obs_ints$i_Q304_consommer, obs_ints$o_Q226_consommer)
  Q305 <- AUC1(obs_ints$i_Q305_lever, obs_ints$o_Q227_positions)

# Continuous Observation of Second and Third Stage of Labor

  Stage3 <- rep(NA, 10)
  Q209 <- AUC1(obs_ints$i_Q209_injection, obs_ints$o_Q309_utertonique)
  Q212 <- AUC1(obs_ints$i_Q212_masse, obs_ints$o_Q316_massageuterin)
  Q214 <- AUC1(obs_ints$i_Q214_contrepression, obs_ints$o_Q315_traction)  #le placenta recode

# Immediate Newborn and Postpartum Care

  Stage4 <- rep(NA, 10)
  Q215 <- AUC1(obs_ints$i_Q215_seche, obs_ints$o_Q402_seche)
  Q216 <- AUC1(obs_ints$i_Q216_peau, obs_ints$o_Q407_peau)
  Q218 <- AUC1(obs_ints$i_Q218_misausein, obs_ints$o_Q412_allaitement)
  Q221 <- AUC1(obs_ints$i_Q221_temp_tension, obs_ints$o_Q410_vitaux)
  Q222 <- AUC1(obs_ints$i_Q222_cordon, obs_ints$o_Q408_temp) 

# Outcomes 

  Stage5 <- rep(NA, 10) 
  Q208 <- AUC1(obs_ints$i_Q208_pousser, obs_ints$o_Q503_pression)
  Q308 <- AUC1(obs_ints$i_Q307_position, obs_ints$o_Q503_verbale)
  Q309 <- AUC1(obs_ints$i_Q307_position, obs_ints$o_Q503_physical)


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

AUC2 <- function(var1, var2) {
  
  short.var.1 <<- substring(deparse(substitute(var1)), 10, 50)
  short.var.2 <<- substring(deparse(substitute(var2)), 10, 50)
  
  # Keeping yes/no responses
  dtab <<- part_obs[var1 %in%  c("oui", "non", "Oui", "Non", "OUI", "NON") &
                       var2 %in%  c("oui", "non", "Oui", "Non", "OUI", "NON") ,] 
  
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
Q04 <- AUC2(part_obs$p_Q04_utertonique, part_obs$o_Q309_utertonique)
Q05 <- AUC2(part_obs$p_Q05_tractioncordon, part_obs$o_Q315_traction )
Q06 <- AUC2(part_obs$p_Q06_massage, part_obs$o_Q316_massageuterin )

Stage4 <- c( NA, NA, NA, NA, NA, NA, NA)
#Q07 <- AUC2(part_obs$p_Q07_crede, part_obs$o_Q414_vitaminek )
Q08 <- AUC2(part_obs$p_Q08_soinscordon, part_obs$o_Q406_coupe )
Q09 <- AUC2(part_obs$p_Q09_allaitement, part_obs$o_Q412_allaitement )

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
  
  print(xtable(df3, align=replicate(8, "center")), type="html")



  
  
  