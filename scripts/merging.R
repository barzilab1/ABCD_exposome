#' ############################################################
#' Here should be all the merging of the different datasets. 
#' as well as creation of additional variables that are specific to this project
#' #############################################################

#' example of merging:
#' 1. first read the datasets from the outputs folder 
#' 2. merge the datasets (make sure you merge datasets from the same time points)

  # demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
  # family_set_baseline <- read_csv("outputs/family_set_baseline.csv")
  # baseline_set = merge(demographics_baseline,family_set_baseline)





psychopathology_sum_scores <- read_csv("outputs/psychopathology_sum_scores.csv")


psychopathology <- read_csv("outputs/psychopathology.csv")
ksad_y_symptoms <- read_csv("outputs/ksad_y_symptoms.csv")
ksad_y_diagnosis <- read_csv("outputs/ksad_y_diagnosis.csv")
externalize_ksad_diagnosis_p <- read_csv("outputs/externalize_ksad_diagnosis_p.csv")
externalize_ksad_symptoms_p <- read_csv("outputs/externalize_ksad_symptoms_p.csv")

#remove diagnosis sums from ksads


data = merge()

#how many factors there are in the data
library(nFactors)
xcor <- cor_auto(data,ordinalLevelMax=5)
nfactors(xcor,n.obs=XXXXXX)
VSS.scree(xcor)

#factor analysis 
library(qgraph)
fa.parallel(xcor,n.obs=XXXXXXX)
fa.sort(fa(xcor,3))
