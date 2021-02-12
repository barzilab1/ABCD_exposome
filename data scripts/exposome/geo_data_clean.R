
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",exposome_files_path)

# rhds01 = rhds01[, grepl("^(src|interview|event|sex)|addr1_(valid|status|years|elevation|adi_(income|pov|wsum|perc))", colnames(rhds01))]


#TODO check for each time point
#remove columns with more than 20% NA
rhds01 = droplevels(rhds01[rhds01$eventname == "baseline_year_1_arm_1",])
rhds01 = rhds01[, colSums(is.na(rhds01)) <= 0.2*dim(rhds01)[1]]

summary(rhds01)

write.csv(file = "outputs/geo_data.csv",x = rhds01, row.names = F, na = "")


