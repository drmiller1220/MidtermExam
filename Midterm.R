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
  posterior <- integrate(Vectorize(integrand), lower=lower, upper=upper) # we use 
                                                              # the integrand in the
                                                              # integrate function to yield
                                                              # our expected a posteriori
                                                              # value
                                                              # initially, the integrate
                                                              # function threw a warning that
                                                              # the object lengths of the
                                                              # inputs did not match; the
                                                              # help file suggested using
                                                              # Vectorize() to get the
                                                              # function in the correct form
  return(posterior)
}

EAP(Bob)

plotting <- function(student, lower_bound=-10, upper_bound=-10, questions=c(1:4)){
  points <- seq(from=lower_bound, to=upper_bound, by=0.1)
  probs <- sapply(points, function(x) prob_answers(student, x)[[1]])
  probs_for_questions <- probs[questions,]
  questions_correct <- sapply(questions, function(x) student$correct_answers[x]==1)
  questions_color <- ifelse(questions_correct==TRUE, "forestgreen", "firebrick1")
  layout(matrix(c(1,2), nrow=2), heights = c(0.7,0.3))
  plot(points, probs_for_questions[1,], type="l", col=question1_color, 
       xlab=expression(paste(theta, " values")), 
       ylab=expression(paste("Pr(P"[ij],"=1)")), main="Response Function for Question 1",
       xaxt="n", yaxt="n")
  axis(1, at=seq(from=lower_bound, to=upper_bound, by=1))
  axis(2, at=seq(from=0, to=1, by=0.1), tick=TRUE)
  theta_hat <- EAP(student)
  abline(v=theta_hat$value, col="dodgerblue")
  # adjusting plotting margins to create legend
  par(mar=c(0,0,0,0))
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="") # creating null plot
  # creating a legend for the null plot, which is effectively the legend for the above
  # plot
  legend("center",legend=c(paste0("Response Function (",student$name," answered correctly)"), 
                           paste0("Response Function (",student$name," answered incorrectly)"),
                           paste0(student$name,"'s EAP")), # providing items in legend
         col=c("forestgreen","firebrick1","dodgerblue"), 
         # providing color for each item in legend
         lty = c(1,1,1)) # providing line type for each item in legend
}


