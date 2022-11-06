#Generating the college cutoff data

#We assume in our model that the scores in your class 12 exams (board exams) is determined by how hard you work and your caste category
#Hardwork is assumed to be uniformly distributed from 0 to 10
#Caste categories are assigned randomly into General, Scheduled Castes, Scheduled Tribes and Other Bachward Classes as classified in the Constitution of India
#The relative proportions are taken to be the same as that actually found in India, according to NSSO 2007 survey
#It is assumed that ceteris paribus, historically underpriveleged castes would perform worse due to lack of resources

hardwork <- runif(1355, 0, 10)
caste <- c()
for (i in 1:1355) {
  casteLottery <- runif(1, 0, 1)
  if (casteLottery > 0.7) {
    caste <- c(caste, 3)
  }
  if (casteLottery <= 0.7 & casteLottery > 0.29) {
    caste <- c(caste, 2)
  }
  if (casteLottery <= 0.29 & casteLottery > 0.09) {
    caste <- c(caste, 1)
  }
  if (casteLottery <= 0.09) {
    caste <- c(caste, 0)
  }
}
twScores <- (4 * hardwork) + (20 * caste)

#We separate the scored for the caste categories and sort them in descending order

gens <- c()
OBCs <- c()
SCs <- c()
STs <- c()

for (i in 1:1355) {
  if (caste[i] == 3) {
    gens <- c(gens, twScores[i])
  }
  if (caste[i] == 2) {
    OBCs <- c(OBCs, twScores[i])
  }
  if (caste[i] == 1) {
    SCs <- c(SCs, twScores[i])
  }
  if (caste[i] == 0) {
    STs <- c(STs, twScores[i])
  }
}

gens <- sort(gens, decreasing = TRUE)
OBCs <- sort(OBCs, decreasing = TRUE)
SCs <- sort(SCs, decreasing = TRUE)
STs <- sort(STs, decreasing = TRUE)

#We combine them, and also sort the caste categories accordingly

twScores <- c(gens, OBCs, SCs, STs)
castes <- c(rep('gen', 406), rep('OBC', 549), rep('SC', 280), rep('ST', 120))

plot(x = caste, y = twScores) #Plotting board exam scores by caste to ensure there are no obvious mistakes

#Assume the college of concern has 400 seats
#200 unreserved
#100 reserved for OBCs
#50 reserved for SCs
#50 reserved for STs

#The admissions are based solely on the twelfth scores (as is actually done in the University of Delhi)
#The cutoff twelfth scores are calculated below

genCutoff <- gens[200]
OBCCutoff <- OBCs[100]
SCCutoff <- SCs[50]
STCutoff <- STs[50]

#For each student, we determine whether the student is admitted to the college based on their caste and twelfth score

colleges <- rep(0, 1355)
for (i in 1:1355) {
  if (castes[i] == 'gen' & twScores[i] >= genCutoff) {colleges[i] = 1}
  if (castes[i] == 'OBC' & twScores[i] >= OBCCutoff) {colleges[i] = 1}
  if (castes[i] == 'SC' & twScores[i] >= SCCutoff) {colleges[i] = 1}
  if (castes[i] == 'ST' & twScores[i] >= STCutoff) {colleges[i] = 1}
}

sum(colleges) #We look at the sum to make sure that the number of students accepted is actually the number that was decided, and that there are no mistakes

#We assume that the UG CGPA is determined by the twelfth scores and whether you got admitted into the college of concern, with some error

CGPAs <- (0.05 * twScores) + (3 * colleges) + rnorm(1355, 0, 0.1)
max(CGPAs) #Makinf sure that the CGPAs do not exceed 10, as DU uses a 0 to 10 scale

#We plot the data for an initial impression

plot(twScores, CGPAs)

#We finally prepare the data frame on which the analysis will be done

admitData <- data.frame(
  boards = twScores,
  category = castes,
  CGPA = CGPAs,
  college = colleges,
  stringsAsFactors = TRUE
)

#This is where we finally begin the analysis

#We want to find the causal effect of getting admitted to the college of concern on UG CGPA
#From our theoretical model, we know that the ATE is 3, but of course we plan to replace the self-generated data with actual DSE Placement Cell Data
#The plan below shall act as a pre-registering device

#We first note that the cutoffs are different for different caste categories
#We assume that the ATE is independent of caste
#Therefore, to solve the problem of differing cutoffs, we use the following procedure:
#We define the variable "excess" as the individual's board score minus the cutoff for their caste category
#In other words, it tells us by how many marks the student got or missed the admission

excesses <- c()

for (i in 1:1355) {
  boardScorei <- admitData[i, 1]
  CGPAi <- admitData[i, 3]
  castei <- admitData[i, 2]
  
  if (castei == "gen") {
    excessi = boardScorei - genCutoff
  }
  if (castei == "OBC") {
    excessi = boardScorei - OBCCutoff
  }
  if (castei == "SC") {
    excessi = boardScorei - SCCutoff
  }
  if (castei == "ST") {
    excessi = boardScorei - STCutoff
  }
  
  excesses <- c(excesses, excessi)
}

admitData$excess <- excesses

#We now import relevant libraries for regression discontinuity

library(tidyverse)
library(broom)
library(rdrobust)
library(rdpower)
library(rddensity)
library(modelsummary)
library(ggplot2)

#Ascertaining that there IS an arbitrary rule assigning college admissions

#This has been ascertained, in the case of the genetared data by assumption

#For the real world case, interviews with college administration has made us certain that the admissions are strictly based on cutoffs and no exceptions are made on any grounds
#Also, the colleges in DU do not have the precise data regarding twelfth scores of applicants before deciding the cutoffs, thereby making them partly random
#It is only after the cutoffs are released (partly based on guess work) that students approach individual colleges in DU for admission
#The college cannot deny admission to a student who has scored above the cutoff even if there is over admission
#Similarly, no student scoring below the final cutoff can be admitted

#Determining whether the design is fuzzy or sharp

#In our generated data, it is sharp by assumption



#We begin with a simple histogram

ggplot(data = admitData,
       mapping = aes(x = excess, y = college, color = college)) + geom_point() +geom_vline(xintercept = 0)

#From the plot, it appears as if the design is strict

#We now tabulate the data to be sure
#We group the data by college and cutoff satisfaction

admitData[admitData$college == 1 & admitData$excess < 0, ] #0 rows reported
admitData[admitData$college == 0 & admitData$excess >= 0, ] #0 rows reported

#We are therefore sure that the design is strict

#We now test whether there is manipulation

#In the generated data this is of course ruled out by assumption

#In actual DU admissions, we have been told by college authorities that cutoffs are set after board exams, and also publicly displayed and strictly adhered to, therefore, there is no possibility of manipulation whatsoever
#But, we still test them graphically and through formal testing

#We begin with a histogram
hist(admitData$excess, breaks = 40, xlim = c(-20, 20), main  = "Histogram depicting the running variable", xlab = "Excess scores")
#There appears to be no manipulation

#We now do a formal test

rdplotdensity(rdd = rddensity(admitData$excess, c = 0),
              X = admitData$excess,
              type = "both")

#The gap is well within the 95% confidence intervals, therefore we do not find evidence for manipulation

#We take a graphical look first at the relationship between the running variable excess and UG CGPA

ggplot(data = admitData,
       aes(x = excess, y = CGPA, color = college)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_smooth(method = 'lm', data = admitData[admitData$excess < 0, ], color = 'black', width = 2) +
  geom_smooth(method = 'lm', data = admitData[admitData$excess >= 0, ], color = 'black', width = 2)

#From the graph, it appears that there indeed is an LATE that is strictly positive

#Now we actually estimate the gap using 3 simple parametric models

#We begin by considering the entire data (without bandwidths)
#For the generated data, this should give an accurate estimate because the ATE is uniform across values of the regressors
#But for the real data to be used later on, we do not expect it to be correct

model1 <- lm(CGPA ~ excess + college, data = admitData)

summary(model1)

#We get roughly accurate estimates: the excess variable affects CGPA by 0.05 and college by 2.97

#We now introduce bandwidths

#Let us use a bandwidth of 10

model2 <- lm(CGPA ~ excess + college, data = admitData[admitData$excess <10 & admitData$excess > -10, ])

summary(model2)

#The estimates are roughly correct again

#Now we use a bandwidth of 5

model3 <- lm(CGPA ~ excess + college, data = admitData[admitData$excess <5 & admitData$excess > -5, ])

summary(model3)

#Quite accurate estimates again, but excess' effect becomes insignificant

#We finally use non-parametric methods to estimate the LATE

model4 <- rdrobust(y = admitData$CGPA, x = admitData$excess, c = 0)

summary(model4)

#The optimal badwidths (left and right respectively) were estimated to be 5.95 and 5.95
#The estimate found is 3.018 which is quite accurate
