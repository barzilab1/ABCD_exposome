library(readr)
library(psych)
library(nFactors)
library(qgraph)
library(mirt)

psychopathology <- read_csv("outputs/psychopathology_final.csv")
data = psychopathology[, !grepl("sex|ksads_15_91_p|ksads_15_92_p", colnames(psychopathology))]
#remove kids with no data
data = data[-which(rowSums(is.na(data[,-1])) == 88),] 

#number of factors in the data
xcor <- cor_auto(data[,-1],ordinalLevelMax=6)
VSS.scree(xcor)

fa.sort(fa(xcor,6))


bf_mod = bfactor(data[,-1],model=c(rep(2,21),rep(4,10),rep(5,19),rep(3,9),rep(1,15),rep(6,14)))
bfactor_scores = fscores(bf_mod, QMC=TRUE)
bfactor_scores = scale(bfactor_scores)
colnames(bfactor_scores) = paste0("bif_",colnames(bfactor_scores))
bfactor_scores = cbind(data[,1],bfactor_scores)

#fit statistics
M2(bf_mod, QMC=TRUE, na.rm=TRUE)

#loadings for the bfactor diagram
loadings = summary(bf_mod)$rotF
write.csv(file = "outputs/loadings_no_sui.csv",x = loadings, na = "")



#correlated-traits scores
corr_mod <- mirt(data[,-1],6,method="QMCEM")
scores <- scale(fscores(corr_mod,QMC=TRUE,rotate="oblimin"))
colnames(scores) = paste0("corr_",colnames(scores))

p_scores = cbind(bfactor_scores,scores)
write.csv(file = "outputs/p_scores.csv",x = p_scores, na = "", row.names = F)


############ add suicide ############

suicide <- read_csv("outputs/suicide.csv")

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
write.csv(file = "outputs/loadings_with_sui.csv",x = loadings_sui, na = "")



#correlated-traits scores
corr_mod_sui <- mirt(data_sui[,-1],6,method="QMCEM")
scores_sui <- scale(fscores(corr_mod_sui,QMC=TRUE,rotate="oblimin"))
colnames(scores_sui) = paste0("corr_",colnames(scores_sui))

p_scores_sui = cbind(bfactor_scores_sui,scores_sui)
write.csv(file = "outputs/p_score_with_sui.csv",x = p_scores_sui, na = "", row.names = F)

