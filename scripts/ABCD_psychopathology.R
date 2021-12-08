
library(psych)
library(nFactors)
library(qgraph)
library(mirt)

psychopathology <- read.csv("psychopathology_final.csv")
x <- psychopathology[, !grepl("sex|ksads_15_91_p|ksads_15_92_p", colnames(psychopathology))]
x <- x[-which(rowSums(is.na(x[,-1])) == 88),] 

#ksads_14_76_p <- x$ksads_14_76_p + x$ksads_14_77_p
#ksads_14_78_p <- x$ksads_14_78_p + x$ksads_14_79_p
#ksads_14_84_p <- x$ksads_14_84_p + x$ksads_14_85_p
#ksads_14_80_p <- x$ksads_14_80_p + x$ksads_14_81_p
#ksads_14_82_p <- x$ksads_14_82_p + x$ksads_14_83_p
ksads_14_7781_p <- x$ksads_14_77_p + x$ksads_14_81_p
ksads_14_7983_p <- x$ksads_14_79_p + x$ksads_14_83_p

x <- x[, !names(x) %in% c("ksads_14_77_p","ksads_14_81_p","ksads_14_76_p","ksads_14_78_p","ksads_14_84_p","ksads_14_80_p","ksads_14_82_p","ksads_14_89_p","ksads_14_86_p","ksads_14_79_p","ksads_14_83_p","ksads_16_103_p","ksads_16_100_p","ksads_16_101_p","ksads_16_102_p")]

x <- data.frame(x,ksads_14_7983_p,ksads_14_7781_p)

#xcor <- cor_auto(x[,-1],ordinalLevelMax=7)
#VSS.scree(xcor)
#fa.sort(fa(xcor,6))

mod <- mirt(x[,-1],5,method="QMCEM")
fa.sort(summary(mod,rotate="oblimin")$rotF)
sc <- scale(fscores(mod,QMC=TRUE,rotate="oblimin"))
sc[,1] <- sc[,1]*(-1)
sc[,4] <- sc[,4]*(-1)
colnames(sc) <- c("Externalizing","Psychosis Spectrum","Positive Affect","Mania","Youth Report Problems")
x <- data.frame(x[,1],sc)
#write.csv(x,"ABCD_Clinical_CorrTraits.csv",na="")

mod <- bfactor(x[,-1],model=c(rep(1,21),rep(2,10),rep(3,19),rep(4,9),rep(5,16)))
summary(mod)
sc <- scale(fscores(mod,QMC=TRUE))
colnames(sc) <- c("General P","Psychosis Spectrum","Mania","Youth Report Problems","Positive Affect","Externalizing")
x <- data.frame(x[,1],sc)
M2(mod, QMC=TRUE, na.rm=TRUE)
#write.csv(x,"ABCD_Clinical_Bifactor.csv",na="")

# add suicide (different from below a/o 29March2021

x <- read.csv("ABCD_psy_sui.csv")
x <- x[,-c(1,2,78:85,93,94,95,96,104,107:118)]
xcor <- cor_auto(x,ordinalLevelMax=7)
VSS.scree(xcor)

fa.sort(fa(xcor,6))
mod <- mirt(x[,-1],6,method="QMCEM")
fa.sort(summary(mod,rotate="oblimin")$rotF) 

# pull in Mplus scores

ids <- read.csv("ABCD_psy_MPLUS.csv",header=FALSE)[,1]
x <- read.table("ABCD_psychopathology_bifactor.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(76,78,80,82,84,86)]))

colnames(x) <- c("ID", "Psychosis_bifactor", "Mania_bifactor", "YouthReport_bifactor", "PosAffect_bifactor", "Externalizing_bifactor", "General_p")

# write.csv(x,"ABCD_psychopathology_bifactor_scores_23March2021.csv",na="")
temp <- x

ids <- read.csv("ABCD_psy_MPLUS.csv",header=FALSE)[,1]
x <- read.table("ABCD_psychopathology_CorrTraits.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(76,78,80,82,84)]))

colnames(x) <- c("ID", "Psychosis", "Mania", "YouthReport", "PosAffect", "Externalizing")

# write.csv(x,"ABCD_psychopathology_CorrTraits_scores_23March2021.csv",na="")



# pull in Mplus scores - WITH SUICIDE

ids <- read.csv("ABCD_psy_sui_MPLUS.csv",header=FALSE)[,1]
x <- read.table("ABCD_psychopathology_bifactor_sui.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(92,94,96,98,100,102,104)]))

colnames(x) <- c("ID", "Psychosis_bifactor", "Mania_bifactor", "YouthReport_bifactor", "PosAffect_bifactor", "Externalizing_bifactor", "Suicidality_bifactor", "General_p")

# write.csv(x,"ABCD_psychopathology_bifactor_scores_23March2021_WITH_SUICIDALITY.csv",na="")
temp <- x

ids <- read.csv("ABCD_psy_sui_MPLUS.csv",header=FALSE)[,1]
x <- read.table("ABCD_psychopathology_CorrTraits_sui.dat",header=FALSE)

x <- data.frame(ids,scale(x[,c(92,94,96,98,100,102)]))

colnames(x) <- c("ID", "Psychosis", "Mania", "YouthReport", "PosAffect", "Externalizing", "Suicidality")

# write.csv(x,"ABCD_psychopathology_CorrTraits_scores_23March2021_WITH_SUICIDALITY.csv",na="")



























############ add suicide ############

suicide <- read_csv("suicide.csv")

data_sui = merge(data,suicide[,grepl("(src|ksads_(.*)_(145|146|9[4-6][0-9]))", colnames(suicide))])
data_sui[,c("ksads_23_955_t", "ksads_23_966_t")] = NULL

#number of factors in the data
xcor <- cor_auto(data_sui[,-1],ordinalLevelMax=6)
VSS.scree(xcor)

fa.sort(fa(xcor,7))

bf_mod_sui = bfactor(data_sui[,-1],model= c(rep(3,21),rep(5,10),rep(6,17),1,6,rep(4,9),rep(2,15),rep(7,14),rep(1,22)))
bfactor_scores_sui = fscores(bf_mod_sui, QMC=TRUE)
bfactor_scores_sui = scale(bfactor_scores_sui)
colnames(bfactor_scores_sui) = paste0("bif_",colnames(bfactor_scores_sui))
bfactor_scores_sui = cbind(data_sui[,1],bfactor_scores_sui)

#fit statistics
M2(bf_mod_sui, QMC=TRUE, na.rm=TRUE)
  
#loadings for the bifactor diagram
loadings_sui = summary(bf_mod_sui)$rotF
write.csv(file = "loadings_with_sui.csv",x = loadings_sui, na = "")



#correlated-traits scores
corr_mod_sui <- mirt(data_sui[,-1],6,method="QMCEM")
scores_sui <- scale(fscores(corr_mod_sui,QMC=TRUE,rotate="oblimin"))
colnames(scores_sui) = paste0("corr_",colnames(scores_sui))

p_scores_sui = cbind(bfactor_scores_sui,scores_sui)
write.csv(file = "p_score_with_sui.csv",x = p_scores_sui, na = "", row.names = F)

