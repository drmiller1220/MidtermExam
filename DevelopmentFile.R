library(devtools)
library(roxygen2)
setwd("C://Users//drmiller1220//Documents//GitHub//MidtermExam") #This will need to be changed to match your directory

current.code <- as.package("raschPack")
load_all(current.code)
document(current.code)
check(current.code)
install(pkg="raschPack", local=TRUE)

# testing the functionality of the package

library(raschPack) # loading in the package

# simulating data for toy example
diff_params <- rnorm(20,0,3)
answers <- rbinom(20,1, prob=0.7)

bob <- new("Rasch", name="Bob", difficulty=diff_params, correct_answers=answers)
bob
# creating a new student

chris <- new("Rasch", name="Chris", difficulty=c(1:5), correct_answers=c(1:6))
# testing validity; throws error that difficulty and correct answers must be of the same length

bob_prob <- raschProb(bob, 1)
bob_prob
# obtaining the probabilities for each question and for the observed data

bob_lik <- raschLik(bob, 1)
bob_lik
# calculating the likelihood for the observed data given a proposed theta

bob_prior <- raschPrior(2)
bob_prior
# obtaining a prior probability

bob_EAP <- raschEAP(bob)
bob_EAP
# calculating the EAP for the student

raschPlot(raschObj=bob, lower_bound=-10, upper_bound=10, 
          questions=c(1:4), theta_EAP=TRUE)
# plotting using the default settings

raschPlot(raschObj=bob, lower_bound=-10, upper_bound=10, 
          questions=c(6,10,15), theta_EAP=TRUE)
# plotting a non-default question

raschPlot(raschObj=bob, lower_bound=-3, upper_bound=3, 
          questions=c(6,10,15), theta_EAP=TRUE)
# changing the plotting bounds

raschPlot(raschObj=bob, lower_bound=-10, upper_bound=10, 
          questions=c(6,10,15), theta_EAP=FALSE)
# turning off the EAP

raschInfo(bob,1)
# obtaining the Fisher's information for each question given the student and a proposed theta
