source("config.R")
source("utility_fun.R")


########## child report time point 3 (august) ########## 
covidy = load_instrument("yabcdcovid19questionnaire01",covid19_files_path)

y_colnames = c("attentive_y_cv","calm_y_cv","concentrate_y_cv","confident_y_cv",
               "delighted_y_cv","ease_y_cv","energetic_y_cv","enthusiastic_y_cv",
               "interested_y_cv", "felt_alone_cv","felt_always_sad","felt_angry_cv",
               "felt_cv","felt_life_went_wrong_cv","felt_lonely_cv","felt_nervous_cv",
               "felt_no_fun_cv","felt_sad_cv","felt_scared_cv","felt_unhappy_cv",
               "mental_health_cv","pstr_confidence_p_cv","pstr_overcome_p_cv",
               "pstr_unable_control_cv","pstr_way_p_cv","scared_easily_cv","something_awful_cv",
               "worried_about_me_cv","worried_at_home_cv","worried_at_night_cv",
               "worried_cv","money_cv")

covidy3 = covidy[covidy$eventname == "covid19_cv3_arm_2",c("src_subject_id","interview_date", "interview_age","eventname", "sex",
                                                           y_colnames)]

covidy3[covidy3 == 777] = NA
covidy3 = droplevels(covidy3)

#remove empty cols
covidy3 = covidy3[,-which(colSums(is.na(covidy3)) == dim(covidy3)[1])]

summary(covidy3)


########## parent report time point 3 (august) ########## 
covidp = load_instrument("pabcdcovid19questionnaire01",covid19_files_path)

covidp3 = covidp[covidp$eventname == "covid19_cv3_arm_2",grep("^(src|interview|eventname|sex|fam_(exp[1-7]|w))", colnames(covidp))]

covidp3[covidp3 == 777] = NA
covidp3 = droplevels(covidp3)

#change scale to 0 - 1
covidp3[,grep("fam_", colnames(covidp3))] = lapply(covidp3[,grep("fam_", colnames(covidp3))], function(x) as.numeric(as.character(x)))
covidp3[,grep("fam_", colnames(covidp3))] = covidp3[,grep("fam_", colnames(covidp3))] - 1

summary(covidp3)

########## merge ########## 
covid = merge(covidy3,covidp3)

write.csv(file = "outputs/covid_august.csv",x = covid,row.names=F, na = "")



library("psych")
xcor_fin <- polychoric(covid[,grep("fam_exp[1-6]|_wag", colnames(covid))])$rho
VSS.scree(xcor_fin)
eigen(xcor_fin)$values[1]/eigen(xcor_fin)$values[2]

xcor_psyc <- polychoric(covid[,grep("felt", colnames(covid))])$rho
VSS.scree(xcor_psyc)
eigen(xcor_psyc)$values[1]/eigen(xcor_psyc)$values[2]




