
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",exposome_files_path)

rhds01 = rhds01[, grepl("^(src|interview|event|sex)|addr1_(valid|status|years|elevation|adi_(income|pov|wsum|perc))", colnames(rhds01))]

summary(rhds01[rhds01$eventname == "1_year_follow_up_y_arm_1",])
summary(rhds01[rhds01$eventname == "baseline_year_1_arm_1",grepl("percentile",colnames(rhds01))])


