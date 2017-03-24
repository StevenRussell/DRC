

library(dplyr)
library(xtable)
library(caret)

# Baseline data

load('C:/DRC/ints_baseline.Rdata')
load('C:/DRC/obs_baseline.Rdata')
load('C:/DRC/obs_ints_baseline.Rdata')

# Creating data frame with unmatched interviews

ints_only_baseline <- filter(ints_baseline, !(ID %in% obs_ints_baseline$ID))

# 2 sample test for equality of proportions

round(prop.table(table(ints_only_baseline$i_Q201_examine))*100, 1)
round(prop.table(table(obs_ints_baseline$i_Q201_examine))*100, 1)

tbl <- rbind(table(ints_only_baseline$i_Q201_examine)[c("non", "oui")],
             table(obs_ints_baseline$i_Q201_examine)[c("non", "oui")])


prop.test(tbl)

# Function version

fun <- function(question) {
 
  tbl <<- rbind(table(ints_only_baseline$question)[c("non", "oui")],
               table(obs_ints_baseline$question)[c("non", "oui")])
  
  prop.test(tbl)
  
}

fun(i_Q201_examine)
fun(i_Q202_tension)
fun(i_Q203_temp)
fun(i_Q205_carte_prenatal)
fun(i_Q302_expliquer)
fun(i_Q303_questions)
fun(i_Q304_consommer)
fun(i_Q305_lever)
fun()
fun()
fun()








  
# Endline data

load('C:/DRC/ints_endline.Rdata')
load('C:/DRC/obs_ints_endline.Rdata')

round(prop.table(table(obs$o_Q112_bc))*100, 1)
round(prop.table(table(obs_ints$o_Q112_bc))*100, 1)

prop.test(
        
        
obs
obs_ints