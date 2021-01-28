##############################################
#' most of the code below is adjusted from 
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda 
##############################################

library(data.table)

source("config.R")
source("utility_fun.R")

demographics_set = load_instrument("abcd_lpds01",demographics_files_path)


########### rearrange data ########### 
###1. convert variables names to be more readable  
###2. change outliers (777,999) to be NA
demographics_set = data.table(demographics_set)

########### sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]

########### age
#interview age will be used instead of age
demographics_set[, age := as.numeric(as.character(interview_age))]

########### gender
demographics_set[,gender := as.numeric(as.character(demo_gender_id_v2_l))]
demographics_set[(gender %in%  c(777,999)) ,gender := NA] 
demographics_set[, gender:= gender-1]
demographics_set[, demo_gender_id_v2_l:= NULL]

########### parents education
demographics_set[(demo_prnt_ed_v2_l %in%  c(777,999)), demo_prnt_ed_v2_l:= NA]
#fix bug when reading demo_prtnr_ed_v2_l
demographics_set[,demo_prtnr_ed_v2_l := as.numeric(as.character(demo_prtnr_ed_v2_l))]
demographics_set[(demo_prtnr_ed_v2_l %in%  c(777,999)), demo_prtnr_ed_v2_l:= NA]

demographics_set[, parents_avg_edu:= (as.numeric(as.character(demo_prnt_ed_v2_l)) + demo_prtnr_ed_v2_l)/2]
#in case of edu is missing in one of the parents, it will be the edu the of other parent 
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= as.numeric(as.character(demo_prnt_ed_v2_l))]
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= as.numeric(as.character(demo_prtnr_ed_v2_l))]

########### family income 
demographics_set[,household_income:= demo_comb_income_v2_l]
demographics_set[( household_income %in%  c(777,999)) ,household_income:= NA]
demographics_set[,demo_comb_income_v2_l := NULL]

########### parents married status 
demographics_set[demo_prnt_marital_v2_l == 777 , demo_prnt_marital_v2_l:= NA]

demographics_set[,separated_or_divorced := 0]
demographics_set[(demo_prnt_marital_v2_l %in%  c(3,4)), separated_or_divorced := 1]

demographics_set[,parents_married := 0]
demographics_set[(demo_prnt_marital_v2_l == 1), parents_married := 1]

########### economic hardship
economic_hardship_names = grep("demo_fam_exp", colnames(demographics_set),value = T)
for(name in economic_hardship_names){
  set(demographics_set,i = which(demographics_set[[name]] == 777), j= name, value = NA)
}

demographics_set[ , (economic_hardship_names) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = economic_hardship_names]

library("psych")
xcor <- polychoric(as.data.frame(demographics_set)[ ,economic_hardship_names ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

demographics_set[, demo_fam_poverty := rowSums(.SD, na.rm = T), .SDcols = economic_hardship_names]
# demographics_set[ , View(.SD), .SDcols = c(economic_hardship_names, "demo_fam_poverty") ]
demographics_set[rowSums(is.na(demographics_set[,.SD,.SDcols = economic_hardship_names])) == 7 , "demo_fam_poverty" := NA]


##################only in exposome
demographics_set = demographics_set[eventname == "1_year_follow_up_y_arm_1",]

#remove any empty cols in 1 year follow up
demographics_set[, colnames(demographics_set)[colSums(is.na(demographics_set)) == dim(demographics_set)[1]] := NULL]

#remove irrelevant columns
#languages *** fix if need leng
demographics_set[, grep("_lang", colnames(demographics_set), value = T) := NULL]
#parent data
demographics_set [, c("demo_prnt_age_v2_l", "demo_prnt_age_v2_refuse_l", "demo_prnt_gender_id_v2_l") := NULL]

demographics_set[, grep("_(prnt|prtnr)_(race|ethn.*|indust_refuse|empl_time|2yr)_", colnames(demographics_set), value = T) := NULL]
demographics_set[, grep("_child_time(2|3)_", colnames(demographics_set), value = T) := NULL]
demographics_set[, grep("_roster_(.*c|.*refuse)_", colnames(demographics_set), value = T) := NULL]
demographics_set[, grep("_yrs_2([a-b]|.*display)_", colnames(demographics_set), value = T) := NULL]
demographics_set[, grep("_med_", colnames(demographics_set), value = T) := NULL]

demographics_set = droplevels(demographics_set)



write.csv(file = "outputs/discrimination_demographics.csv",x = demographics_set[,c("src_subject_id", "interview_date", "interview_age", "demo_prim" , "eventname", "sex",
                                                                                                "demo_ed_v2_l", 
                                                                                                "household_income","demo_prnt_ed_v2_l", "demo_prtnr_ed_v2_l", "parents_avg_edu", 
                                                                                                "demo_prnt_marital_v2_l", "separated_or_divorced", "parents_married",
                                                                                                "age", "sex_br", "gender",
                                                                                                economic_hardship_names, "demo_fam_poverty")], row.names=F, na = "")




