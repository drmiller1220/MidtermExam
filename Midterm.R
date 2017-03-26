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
    (1+ exp(proposal - object$difficulty))                  # answers each question correctly
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

EAP <- function(student, lower=-6, upper=6){ # creating EAP function; takes in student object
                                             # and integration limits, defaulted to -6 and 6
  integrand <- function(x) lik_answers(student, x) * prior(x) # specifying function to
                                                              # integrate, we want to
                                                              # integrate the likelihood
                                                              # times the prior, given
                                                              # our data
  posterior <- integrate(integrand, lower=lower, upper=upper) # we use the integrand in the
                                                              # integrate function to yield
                                                              # our expected a posteriori
                                                              # value
  return(posterior)
}

EAP(Bob)

plotting <- function(student){
  lower_bound <- -10
  upper_bound <- 10
  points <- seq(from=lower_bound, to=upper_bound, by=0.1)
  probs <- NULL
  for(i in points){
    probs <- append(probs, prob_answers(student, i)[[1]])
  }
  question1 <- probs[seq(from=1, to=length(probs), by=20)]
  question1_correct <- student$correct_answers[1]==1
  question1_color <- ifelse(question1_correct==TRUE, "forestgreen", "firebrick1")
  plot(points, question1, type="l", col=question1_color, xlab=expression(paste(theta, " values")), 
       ylab=expression(paste("Pr(P"[ij],"=1)")), main="Response Function for Question 1",
       xaxt="n", yaxt="n")
  axis(1, at=seq(from=lower_bound, to=upper_bound, by=1))
  axis(2, at=seq(from=0, to=1, by=0.1), tick=TRUE)
  theta_hat <- EAP(student)
  abline(v=theta_hat$value, col="dodgerblue")
}


