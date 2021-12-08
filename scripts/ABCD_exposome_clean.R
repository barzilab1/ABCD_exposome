
library(psych)
library(qgraph)
library(Matrix)


###################################
# EFAs to identify clustering among 
# variables and  further reduction 
###################################

x <- read.csv("data/exposome_main_final_2.csv")

temp <- x$su_risk_p_6
temp[temp == 2] <- NA
x$su_risk_p_6 <- temp

temp <- x[,60:64]
temp[temp==4] <- NA
x[,60:64] <- temp

temp <- x[,c(86,89,92)]
temp[temp==6] <- 0
temp[temp==5] <- 0
temp[temp==4] <- 0
temp[temp==3] <- 0
temp[temp==2] <- 0
x[,c(86,89,92)] <- temp

#temp <- x$devhx_3_p
#temp[temp > 55] <- NA
#x$devhx_3_p <- temp

temp <- x$devhx_4_p
temp[temp > 80] <- NA
x$devhx_4_p <- temp

temp <- x$devhx_caffeine_11
temp[temp==0] <- 4
x$devhx_caffeine_11 <- temp

temp <- x$devhx_11_p
temp[temp>50] <- NA
x$devhx_11_p <- temp

#temp1 <- x$devhx_15
temp2 <- x$devhx_16_p
#temp3 <- x$devhx_17_p
#temp4 <- x$devhx_18_p
temp5 <- x$devhx_19a_p
temp6 <- x$devhx_19b_p
temp7 <- x$devhx_19c_p
temp8 <- x$devhx_19d_p
#temp1[temp1>30] <- NA
temp2[temp2>60] <- NA
#temp3[temp3>50] <- NA
#temp4[temp4>40] <- NA
temp5[temp5>24] <- NA
temp6[temp6>60 | temp6<2] <- NA
temp7[temp7>60 | temp7<6] <- NA
temp8[temp8>72 | temp8<3] <- NA
#x$devhx_15 <- temp1
x$devhx_16_p <- temp2
#x$devhx_17_p <- temp3
#x$devhx_18_p <- temp4
x$devhx_19a_p <- temp5
x$devhx_19b_p <- temp6
x$devhx_19c_p <- temp7
x$devhx_19d_p <- temp8

temp <- x$reshist_addr1_d1a
temp[temp>75] <- NA
x$reshist_addr1_d1a <- temp

temp <- x$reshist_addr1_adi_home_v
temp[temp < 15000] <- 15000
x$reshist_addr1_adi_home_v <- temp

temp <- x[,c(227:234,258,262:268,277:284,287:288,290,303,305:306,309:312)]
temp <- log(temp+1)
x[,c(227:234,258,262:268,277:284,287:288,290,303,305:306,309:312)] <- temp

temp <- x[,c(270,291)]
temp <- winsor(temp,trim=0.005)
x[,c(270,291)] <- temp

temp <- x$reshist_state_mj_laws
temp[temp==3] <- 2
temp[temp==4] <- 3
x$reshist_state_mj_laws <- temp

mature_entertainment <- rowMeans(scale(x[,79:80]),na.rm=TRUE)
screentime <- rowMeans(scale(x[,67:78]),na.rm=TRUE)


parent_monitor <- x$pmq_y_ss_mean

Rx_before_during <- rowSums(x[,c(185,193)],na.rm=TRUE)
alc_before_during <- rowSums(x[,c(187,195)],na.rm=TRUE)
TobMar_before_during <- rowSums(x[,c(186,188,194,196)],na.rm=TRUE)
HardDrugs_before_during <- rowSums(x[,c(189:192,197:200)],na.rm=TRUE)

preg_comp_anemia <- rowSums(x[,c(203,206,209,210)],na.rm=TRUE)
preg_comp_bp <- rowSums(x[,c(205,211,212)],na.rm=TRUE)
preg_comp_placenta <- rowSums(x[,c(213,204,208,215,207,214)],na.rm=TRUE)

birth_comp_blue <- rowSums(x[,219:221],na.rm=TRUE)
birth_comp_reqOxygen <- rowSums(x[,223:225],na.rm=TRUE)
birth_comp_convulsions <- rowSums(x[,c(222,226)],na.rm=TRUE)

trauma_death_family <- x$ksads_ptsd_raw_770_p
trauma_adults_fighting <- x$ksads_ptsd_raw_766_p
trauma_sexual_abuse <- rowSums(x[,251:253],na.rm=TRUE)
trauma_all_other <- rowSums(x[,238:249],na.rm=TRUE)

father_mental_health <- rowSums(x[,c(126,168,138,156,162,174,144)],na.rm=TRUE)
mother_mental_health <- rowSums(x[,c(129,171,141,159,165,177,147)],na.rm=TRUE)
parents_bad_lifestyle <- rowSums(x[,c(120,132,150,123,135,153)],na.rm=TRUE)

extracurriculars <- rowMeans(scale(x[,2:30]),na.rm=TRUE)


neighborhood_density <- rowMeans(scale(x[,c(258,287)]),na.rm=TRUE)
neighborhood_air_pollution <- rowMeans(scale(x[,c(288,289,292)]),na.rm=TRUE)
lead_exposure_risk <- rowMeans(scale(x[,294:295]),na.rm=TRUE)

events_good_bad_ratio <- (x$ple_y_ss_total_good + x$ple_p_ss_total_good + 1)/(x$ple_y_ss_total_bad + x$ple_p_ss_total_bad + 1)

family_religiosity <- rowMeans(x[,322:323],na.rm=TRUE)
family_poverty_events <- rowSums(x[,315:319],na.rm=TRUE)
family_foregone_medical <- rowSums(x[,320:321],na.rm=TRUE)

life_events_legal <- x$ple_jail_y+x$ple_arrest_y+x$ple_law_y+x$ple_sud_y
life_events_discord <- x$ple_financial_y+x$ple_argue_y+x$ple_job_y+x$ple_away_y+x$ple_mh_y+x$ple_victim_y+x$ple_sib_y
life_events_casualties <- x$ple_friend_died_y+x$ple_friend_y+x$ple_friend_injur_y+x$ple_ill_y+x$ple_injured_y+x$ple_injur_y+x$ple_crime_y+x$ple_died_y
life_events_divorce <- x$ple_step_y+x$ple_separ_y
life_events_relocation <- x$ple_move_y+x$ple_school_y+x$ple_new_job_y+x$ple_new_sib_y

discrimination_ethnic <- rowMeans(x[,97:100],na.rm=TRUE)
discrimination_foreigner <- rowMeans(x[,101:103],na.rm=TRUE)

school_Enjoy <- rowMeans(data.frame(x$school_12_y,x$school_15_y*(-1),x$school_3_y),na.rm=TRUE)
school_FeelInvolved <- rowMeans(data.frame(x$school_10_y,x$school_5_y,x$school_2_y,x$school_6_y),na.rm=TRUE)
school_GoodGrades <- rowMeans(data.frame(x$school_8_y,x$school_17_y*(-1),x$school_9_y),na.rm=TRUE) 
school_PositiveFeedback <- rowMeans(data.frame(x$school_7_y,x$school_4_y),na.rm=TRUE) 

parental_age <- rowMeans(scale(x[,180:181]),na.rm=TRUE)
developmental_delay <- rowMeans(scale(x[,231:236]),na.rm=TRUE)

access_to_alc_tobac <- rowMeans(x[,60:62],na.rm=TRUE)

temp <- x[,c(262,264,266:268,270:279,281:284,293)]
sc <- fa(temp,4)$scores
colnames(sc) <- c("neighborhood_DisPoverty","neighborhood_crime","neighborhood_wealth_property","neighborhood_immigration_crowding")

caregiver_psychopathology <- rowMeans(scale(x[,c(47:50,52:54)]),na.rm=TRUE)

x <- x[, !names(x) %in% c("fes_p_ss_fc_pr","fes_y_ss_fc_pr","meim_p_ss_total","tbi_ss_nmrpi","parent_rules_q1a","parent_rules_q6","accult_q1_y","accult_q2_y","reshist_addr1_valid","reshist_addr1_status","reshist_addr1_grndtot","reshist_addr1_p1tot","reshist_addr1_years","reshist_addr1_percentile","reshist_addr1_adi_edu_h","reshist_addr1_adi_wsum","reshist_addr1_adi_perc","ple_y_ss_total_number","ple_y_ss_affect_sum","ple_y_ss_affected_bad_mean","ple_p_ss_total_number","ple_p_ss_affected_mean","ple_p_ss_affect_sum","screen1_wkdy_y","screen2_wkdy_y","screen3_wkdy_y","screen4_wkdy_y","screen5_wkdy_y","screen_wkdy_y","screen7_wknd_y","screen8_wknd_y","screen9_wknd_y","screen10_wknd_y","screen11_wknd_y","screen12_wknd_y","screen13_y","screen14_y","devhx_10a3_p","devhx_10b3_p","devhx_10c3_p","devhx_10d3_p","devhx_10e3_p","devhx_10f3_p","devhx_10g3_p","devhx_10h3_p","devhx_10i3_p","devhx_10j3_p","devhx_10k3_p","devhx_10l3_p","devhx_10m3_p","devhx_14a3_p","devhx_14b3_p","devhx_14c3_p","devhx_14d3_p","devhx_14e3_p","devhx_14f3_p","devhx_14g3_p","devhx_14h3_p","reshist_addr1_p1vlnt","reshist_addr1_drugtot","reshist_addr1_drgsale","reshist_addr1_mjsale","reshist_addr1_drgposs","reshist_addr1_dui","reshist_addr1_leadrisk_poverty","reshist_addr1_d1a","reshist_addr1_popdensity","reshist_addr1_no2","reshist_addr1_pm25","reshist_addr1_pm252016aa","reshist_addr1_leadrisk_housing","reshist_addr1_leadrisk","pmq_y_ss_mean","school_12_y","school_15_y","school_3_y","school_10_y","school_5_y","school_2_y","school_6_y","school_8_y","school_17_y","school_9_y","school_7_y","school_4_y","devhx_19a_p","devhx_19b_p","devhx_19c_p","devhx_19d_p","devhx_20_p","devhx_21_p","devhx_3_p","devhx_4_p","su_risk_p_1","su_risk_p_2","su_risk_p_3")]

x <- x[,!grepl("parent_monitor",colnames(x))]
x <- x[,!grepl("devhx_8",colnames(x))]
x <- x[,!grepl("devhx_9",colnames(x))]
x <- x[,!grepl("ksads_ptsd_raw",colnames(x))]
x <- x[,!grepl("reshist_addr1_adi",colnames(x))]
x <- x[,!grepl("famhx_ss_",colnames(x))]
x <- x[,!grepl("sai_ss_",colnames(x))]
x <- x[,!grepl("asr_scr_",colnames(x))]
#x <- x[,!grepl("ple_y_ss",colnames(x))]
#x <- x[,!grepl("ple_p_ss",colnames(x))]
x <- x[,!grepl("demo_",colnames(x))]
x <- x[,!grepl("ple_",colnames(x))]
x <- x[,!grepl("dim_matrix_",colnames(x))]

x <- data.frame(x,
                mature_entertainment,
                screentime,
                parent_monitor,
                Rx_before_during,
                alc_before_during,
                TobMar_before_during,
                HardDrugs_before_during,
                preg_comp_anemia,
                preg_comp_bp,
                preg_comp_placenta,
                birth_comp_blue,
                birth_comp_reqOxygen,
                birth_comp_convulsions,
                trauma_death_family,
                trauma_adults_fighting,
                trauma_sexual_abuse,
                trauma_all_other,
                sc,
                caregiver_psychopathology,
                father_mental_health,
                mother_mental_health,
                parents_bad_lifestyle,
                extracurriculars,
                neighborhood_density,
                neighborhood_air_pollution,
                lead_exposure_risk,
                events_good_bad_ratio,
                family_religiosity,
                family_poverty_events,
                family_foregone_medical,
                life_events_legal,
                life_events_discord,
                life_events_casualties,
                life_events_divorce,
                life_events_relocation,
                discrimination_ethnic,
                discrimination_foreigner,
                school_Enjoy,
                school_FeelInvolved,
                school_GoodGrades,
                school_PositiveFeedback,
                parental_age,
                developmental_delay,
                access_to_alc_tobac)





ids <- x[,1]

xcor <- cor_auto(x[,-1],ordinalLevelMax=7)

fa.sort(fa(xcor,6))

factors <- 6      # How many factors do you want to extract?
Threshold <- 0.2      # Threshold to use when creating target from pattern matrix
n.obs <- 10000

TT <- 0.5 - Threshold

items <- dim(xcor)[1]

r1 <- fa(xcor,factors)

t0 <- round(abs(r1$loadings) + TT,0)

targ0 <- list(as.numeric(matrix(gsub(1,NA,t0),items,factors)))

first <- matrix(targ0,1,items)

tr1 <- fa(xcor,factors,rotate="TargetQ",Target=targ0,maxit=50000)

targ <- round(abs(tr1$loadings) + TT,0)

targ <- list(as.numeric(matrix(gsub(1,NA,targ),items,factors)))

TargetList <- rbind(matrix(unlist(first),1,items*factors),matrix(unlist(targ),1,items*factors))

repeat {
  
  sol <- fa(xcor,factors,rotate="TargetQ",Target=targ,maxit=50000)
  
  targ <- round(abs(sol$loadings) + TT,0)
  
  targt <- matrix(as.numeric(gsub(1,9,targ)),items,factors)
  targw <- matrix(0,items,factors)
  targw[targt == 0] <- 1
  targphi <- matrix(9,items,items)
  targwphi <- matrix(0,items,items)
  
  targ <- list(as.numeric(matrix(gsub(1,NA,targ),items,factors)))
  
  TargetList <- rbind(TargetList,matrix(unlist(targ),1,items*factors))
  
  if (dim(TargetList)[1] != dim(unique(TargetList))[1]) break }

fa.sort(sol)

write.csv(fa.sort(sol$loadings),"final_exposome_EFA_results.csv")  #write final results to examine

##############################################################################
# Here, Mplus takes over using the model determined above, including 
# cross-loadings.  The CFA is used to estimate the scores, and those 
# are read in below.
##############################################################################

# pull in Mplus scores

ids <- read.csv("exposome_MPLUS_famid.csv",header=FALSE)[,1]
x <- read.table("ABCD_exposome_bifactor_fam.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(66,68,70,72,74,76,78)]))

colnames(x) <- c("ID", "Household_Adversity", "Neighborhood_Poverty", "DayToDay_Adversity", "State_Conservatism_Ruralness", "Family_Values", "Birth_Pregnancy_Complications","Adversity_General_Factor")

# write.csv(x,"ABCD_Exposome_bifactor_scores_16March2021.csv",na="")
temp <- x

ids <- read.csv("exposome_MPLUS_famid.csv",header=FALSE)[,1]
x <- read.table("ABCD_exposome_corrTraits_fam.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(66,68,70,72,74,76)]))

colnames(x) <- c("ID", "Household_Adversity", "Neighborhood_Poverty", "DayToDay_Adversity", "State_Conservatism_Ruralness", "Family_Values", "Birth_Pregnancy_Complications")

# write.csv(x,"ABCD_Exposome_correlated-traits_scores_16March2021.csv",na="")
