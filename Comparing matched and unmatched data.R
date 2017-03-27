
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



fun <- function(question) {
  
  tbls <- rbind(table(ints_only_baseline[,question])[c("oui", "non")],
                table(obs_ints_baseline[,question])[c("oui", "non")])
  
  pt <- prop.test(tbls)

  d <- data.frame(rbind(
    
  rep(NA, 4),
  
  c(table(ints_only_baseline[,question])[c("oui")],
    table(ints_only_baseline[,question])[c("non")],
    pt$estimate[1],
    NA),
  
  c(table(obs_ints_baseline[,question])[c("oui")],
    table(obs_ints_baseline[,question])[c("non")],
    pt$estimate[2],
    pt$p.value), 
  
  rep(NA, 4)
  
  ))
  
  names(d) <- c("Yes", "No", "Proportion", "P-value")
  row.names(d) <- c(question, "Unmatched interviews", "Matched interviews", " ")
  d

}

fun("i_Q201_examine")
fun("i_Q202_tension")
fun("i_Q203_temp")
fun("i_Q205_carte_prenatal")
fun("i_Q302_expliquer")
fun("i_Q303_questions")
fun("i_Q304_consommer")
fun("i_Q305_lever")
fun("i_Q209_injection")
fun("i_Q212_masse")
fun("i_Q214_contrepression")
fun("i_Q215_seche")
fun("i_Q216_peau")
fun("i_Q218_misausein")
fun("i_Q221_temp_tension")
fun("i_Q222_cordon")
fun("i_Q208_pousser")
fun("i_Q307_position")

list <- lapply(questions, fun)
df <- do.call("rbind", list)

print(xtable(df, align=rep("center", 5)), type="html")


questions <- c("i_Q201_examine", "i_Q202_tension", "i_Q203_temp", "i_Q205_carte_prenatal",
               "i_Q302_expliquer", "i_Q303_questions", "i_Q304_consommer", "i_Q305_lever",
               "i_Q209_injection", "i_Q212_masse", "i_Q214_contrepression", "i_Q215_seche", 
               "i_Q216_peau", "i_Q218_misausein", "i_Q221_temp_tension", "i_Q222_cordon", 
               "i_Q208_pousser", "i_Q307_position")





  
# Endline data

load('C:/DRC/ints_endline.Rdata')
load('C:/DRC/obs_ints_endline.Rdata')

round(prop.table(table(obs$o_Q112_bc))*100, 1)
round(prop.table(table(obs_ints$o_Q112_bc))*100, 1)

prop.test(

