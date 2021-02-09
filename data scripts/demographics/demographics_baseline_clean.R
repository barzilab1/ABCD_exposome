##############################################
#' most of the code below is adjusted from 
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda 
##############################################

library(data.table)

source("config.R")
source("utility_fun.R")

demographics_set = load_instrument("pdem02",demographics_files_path)

#remove empty columns 
names(demographics_set)[sapply(demographics_set, function(x) all((x=="NA")|(is.na(x))))]
demographics_set = demographics_set[!sapply(demographics_set, function(x) all((x=="NA")|(is.na(x))))]

########### rearrange data ########### 
###1. convert variables names to be more readable  
###2. change outliers (777,999) to be NA
demographics_set = data.table(demographics_set)

########### sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]
demographics_set[,demo_sex_v2 := NULL]

########### gender
demographics_set[,gender := as.numeric(as.character(demo_gender_id_v2))]
demographics_set[(gender %in%  c(777,999)) ,gender := NA] 
demographics_set[, gender:= gender-1]
demographics_set[, demo_gender_id_v2:= NULL]

########### ethnicity
demographics_set[demo_ethn_v2 == "1", ethnicity_hisp := 1]
demographics_set[demo_ethn_v2 == "2", ethnicity_hisp := 0]
demographics_set[(demo_ethn_v2 %in% c(777,999)), ethnicity_hisp:= NA]
demographics_set = demographics_set[, demo_ethn_v2 := NULL]


########### child race
#"refuse to answer" and "dont know" will be 0

# White
demographics_set[, race_white:= (demo_race_a_p___10 == "1")*1 ]

# Black
demographics_set[, race_black:= (demo_race_a_p___11 == "1")*1 ]

# Asian
demographics_set[, race_asian:= 0]
demographics_set[ (demo_race_a_p___18 == "1" | demo_race_a_p___19 == "1" | demo_race_a_p___20 == "1" |
        demo_race_a_p___21 == "1" | demo_race_a_p___22 == "1" | demo_race_a_p___23 == "1" |
        demo_race_a_p___24 =="1"), race_asian:= 1 ]

# AIAN: American Indian and Alaska Native
demographics_set[, race_aian:= 0]
demographics_set[ (demo_race_a_p___12 == "1" | demo_race_a_p___13 == "1"), race_aian:=1 ]


#NHPI: Native Hawaiian and Other Pacific
demographics_set[, race_nhpi:= 0]
demographics_set[ demo_race_a_p___14 == "1" | demo_race_a_p___15 == "1" | demo_race_a_p___16 == "1" |
       demo_race_a_p___17 == "1", race_nhpi:= 1 ]

# Other
demographics_set[, race_other:= 0 ]
demographics_set[ demo_race_a_p___25 == "1", race_other:= 1 ]

# Mixed
demographics_set[, race_mixed:= (race_white + race_black + race_asian + race_aian + race_nhpi + race_other)]
demographics_set[, table(race_mixed, useNA = "if")]
demographics_set[ race_mixed <= 1, race_mixed:= 0]
demographics_set[ race_mixed > 1, race_mixed:= 1]
demographics_set[, table(race_mixed, useNA = "if")]

demographics_set[, grep("^demo_race_a_p___",colnames(demographics_set), value = T) := NULL]


########### child's country of birth 
demographics_set[, born_in_usa := 0]
demographics_set[demo_origin_v2 %in% c(777,999), born_in_usa := NA]
demographics_set[demo_origin_v2 == 189, born_in_usa := 1]



demographics_set = droplevels(demographics_set)


write.csv(file = "outputs/demographics_baseline.csv",x = demographics_set[,c("src_subject_id", "interview_date", "interview_age" , "eventname", "sex",
                                                                        "race_white", "race_black", "race_aian", "race_nhpi", "race_asian", "race_other","race_mixed" ,"ethnicity_hisp", 
                                                                        "born_in_usa")], row.names=F, na = "")








