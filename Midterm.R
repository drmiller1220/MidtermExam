# function to create a student, with necessary inputs; returns a list with the necessary
# inputs
make_student <- function(name, difficulty, correct_answers){
  return(list(name=name, difficulty=difficulty, correct_answers=correct_answers))
}

# simulating data for toy example
diff_params <- rnorm(20,0,1)
answers <- rbinom(20,1, prob=0.5)

# verifying function yields student appropriately
Bob <- make_student(name="Bob", difficulty=diff_params, correct_answers = answers)
Bob

# creating function to calculate probabilities for student
prob_answers <- function(object, proposal){ 
#inputs are the student object and a proposed theta
  prob_corr_question <- exp(proposal - object$difficulty)/ # probability that the student
    (1+ exp(proposal - object$difficult))                  # answers each question correctly
                                                           # given difficulty and theta
  prob_obs_answer <- ifelse(object$correct_answers==1, 
                            prob_corr_question, # given our theta and difficulty, we want the
                            1-prob_corr_question) # probability that we observe the student's
                                                # observed right or wrong answer; ifelse
                                                # returns P_ij if the student answered 
                                                # correctly, and Q_ij=1-P_ij if the student
                                                # answered incorrectly
  return(list(prob_corr=prob_corr_question, prob_obs=prob_obs_answer)) # return a list
                                                                    # containing the two
                                                                    # vectors requested;
                                                                    # P_ij for each question
                                                                    # and probability of
                                                                    # observed student answer
}

proposal <- 1 # generic proposal

Bob_prob <- prob_answers(Bob, proposal) # verifying that the probability function yields
                                        # what is desired
Bob_prob

lik_answers <- function(object, proposal){ # likelihood function takes a student object and
                                           # a theta proposal
  student_prob <- prob_answers(object, proposal) #we need the information yielded from the
                                                 # probability function, so we feed the inputs
                                                 # into that function
  likelihood <- prod(student_prob$prob_obs) #the likelihood is the product of the observed
                                            # probabilities; i.e. if the student answers
                                            # correctly, y_ij=1 and thus we use P_ij; if
                                            # not, y_ij=0, and thus we use Q_ij
  return(likelihood) # we return the calculated likelihood
}

Bob_lik <- lik_answers(Bob, 0.01) #verifying that the function returns the likelihood

prior <- function(proposal){ # creating function to yield a prior
  height <- dnorm(proposal, mean=0, sd=3) # the prior is the height of the normal 
                                          # distribution specified at theta
  return(height)
}

Bob_prior <- prior(0.5) # verifying that the function returns the prior
