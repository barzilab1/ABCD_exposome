library(readr)
library(lme4)
library(lmerTest)
library(MuMIn)


ABCD_exposome_1_year <- read_csv("abcd exposome/Dataset_ABCD_exposome_1_year_for_Elina_10_28_2021.csv")
site <- read_csv("abcd exposome/site.csv")

site = site[site$eventname == "1_year_follow_up_y_arm_1",]

ABCD_exposome_1_year$src_subject_id = paste0("NDAR_",ABCD_exposome_1_year$src_subject_id)
ABCD_exposome_1_year = merge(ABCD_exposome_1_year,site[,c("src_subject_id","sex","interview_age","eventname","site_id_l_br")])



summary(ABCD_exposome_1_year[,c("General_p", "Zinterview_age",  "sex_br" , "race_white" , "race_black" ,"ethnicity_hisp" ,"race_asian","Zparents_avg_edu" , "Zhousehold_income" , "site_id_l_br", "rel_family_id")])

summary(ABCD_exposome_1_year[,c("Adversity_General_Factor", "Household_Adversity_BF", "Neighborhood_Poverty_BF",
                                "DayToDay_Adversity_BF_High_is_Bad", "State_Conservatism_Ruralness_BF", "Family_Values_BF",
                                "Birth_Pregnancy_Complications_BF"
)])




get_results = function (model){
  ci = confint(model)
  print(summary(model)$call$formula)
  
  cat("\n")
  print(data.frame(
    Estimate    = round(summary(model)$coef[ , "Estimate"] , digits = 3),
    CI_2_5      = round(ci[-c(1:3) ,1 ]  , digits = 3),
    CI_97_5     = round(ci[-c(1:3) ,2 ]  , digits = 3),
    Pvalue      = round(summary(model)$coef[ , "Pr(>|t|)"] , digits = 5) 
  ))
  
  cat("\n")
  r.squaredGLMM(model)
}




sink(file = "abcd exposome/output.txt", append = F, type = c("output", "message"), split = T)

# Model 1: Demographics predicting P-factor (continuous variable)
cat("\n##########################\nModel 1\n##########################\n")
mod1 <- lmer(General_p ~ Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
               Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod1)


# Model 2: Demographics + Exposome factors predicting P-factor (continuous variable)
cat("\n##########################\nModel 2\n##########################\n")
mod2 <- lmer(General_p ~ Adversity_General_Factor+ Household_Adversity_BF + Neighborhood_Poverty_BF+
               DayToDay_Adversity_BF_High_is_Bad + State_Conservatism_Ruralness_BF + Family_Values_BF +
               Birth_Pregnancy_Complications_BF +
               Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
               Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod2)


# Model 3: Demographics predicting CBCL total (continuous variable)
cat("\n##########################\nModel 3\n##########################\n")
ABCD_exposome_1_year$zcbcl_scr_syn_totprob_t = scale(ABCD_exposome_1_year$cbcl_scr_syn_totprob_t)
mod3 <- lmer(zcbcl_scr_syn_totprob_t ~ Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
               Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod3)


# Model 4: Demographics + Exposome factors predicting CBCL total (continuous variable)
cat("\n##########################\nModel 4\n##########################\n")
mod4 <- lmer(zcbcl_scr_syn_totprob_t ~ Adversity_General_Factor+ Household_Adversity_BF + Neighborhood_Poverty_BF+
               DayToDay_Adversity_BF_High_is_Bad + State_Conservatism_Ruralness_BF + Family_Values_BF +
               Birth_Pregnancy_Complications_BF +
               Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
               Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod4)



# Model 5: Demographics predicting BMI (continuous variable)
cat("\n##########################\nModel 9\n##########################\n")
ABCD_exposome_1_year$zBMI = scale(ABCD_exposome_1_year$BMI)
mod5 <- lmer(zBMI ~ Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
               Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod5)


# Model 10: Demographics + Exposome factors predicting BMI (continuous variable)
cat("\n##########################\nModel 10\n##########################\n")
mod6 <- lmer(zBMI ~ Adversity_General_Factor+ Household_Adversity_BF + Neighborhood_Poverty_BF+
                DayToDay_Adversity_BF_High_is_Bad + State_Conservatism_Ruralness_BF + Family_Values_BF +
                Birth_Pregnancy_Complications_BF +
                Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
                Zparents_avg_edu + Zhousehold_income + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
ss <- getME(mod6,c("theta","fixef"))
m2 <- update(mod6,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
get_results(m2)


# Model 11: Demographics predicting pubertal development stage (continuous variable)
cat("\n##########################\nModel 11\n##########################\n")
mod7 <- lmer(Both_sexes_puberty_scale ~ Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
                Zparents_avg_edu + Zhousehold_income + BMI + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod7)


# Model 12: Demographics + Exposome factors predicting pubertal development stage (continuous variable)
cat("\n##########################\nModel 12\n##########################\n")
mod8 <- lmer(Both_sexes_puberty_scale ~ Adversity_General_Factor+ Household_Adversity_BF + Neighborhood_Poverty_BF+
                DayToDay_Adversity_BF_High_is_Bad + State_Conservatism_Ruralness_BF + Family_Values_BF +
                Birth_Pregnancy_Complications_BF +
                Zinterview_age + sex_br + race_white + race_black + ethnicity_hisp + race_asian +
                Zparents_avg_edu + Zhousehold_income + BMI + (1|site_id_l_br) + (1|rel_family_id), data=ABCD_exposome_1_year)
get_results(mod8)




sink(file=NULL)








