# function to create a student, with necessary inputs; returns a list with the necessary
# inputs
make_student <- function(name, difficulty, correct_answers){
  return(list(name=name, difficulty=difficulty, correct_answers=correct_answers))
}

# simulating data for toy example
diff_params <- rnorm(20,0,2)
answers <- rbinom(20,1, prob=0.9)

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

plotting <- function(student, lower_bound=-10, 
                     upper_bound=10, questions=c(1:4), theta_EAP=TRUE){
  # plotting function takes in the student, lower and upper bounds for the thetas to be
  # plotted on the x axis, the questions the user wants plotted (each on its own plot),
  # and whether the user wants theta_hat to be plotted
  points <- seq(from=lower_bound, to=upper_bound, by=0.1)
  # generating a vector of points for thetas
  probs <- sapply(points, function(x) prob_answers(student, x)[[1]])
  # calculating the probabilities for each question at each theta
  probs_for_questions <- unlist(probs[questions,])
  # subsetting the probabilities for the questions to be plotted
  questions_correct <- sapply(questions, function(x) student$correct_answers[x]==1)
  #evaluating if the student answers the questions to be plotted correctly
  questions_color <- ifelse(questions_correct==TRUE, "forestgreen", "firebrick1")
  # determining how to color each response function depending on correctness of answer
  if(theta_EAP==TRUE){ # if the user wants the EAP calculated, we do so
    theta_hat <- EAP(student)
  }
  plotting_function <- function(x){ # creating an internal plotting function
    opar <- par(mar=c(5.1, 4.1, 4.1, 2.1))
    # saving defaultmargin parameters so we can restore them after adding the legend
    # for each plot
    layout(matrix(c(1,2), nrow=2), heights = c(0.7,0.3)) # using layout to make room for
                                                         # a legend
    # we plot the probabilities for each theta for the given question, with the appropriate
    # color to indicate whether the answer is correct
    plot(points, probs_for_questions[x,], type="l", col=questions_color[x], 
         xlab=expression(paste(theta, " values")), 
         ylab=expression(paste("Pr(P"[ij],"=1)")), 
         main=paste0("Response Function for Question ",questions[x]),
         xaxt="n", yaxt="n")
    # making margins more granular than default
    axis(1, at=seq(from=lower_bound, to=upper_bound, by=1))
    axis(2, at=seq(from=0, to=1, by=0.1), tick=TRUE)
    # if the user wants the EAP plotted, we do so
    if(exists("theta_hat")==TRUE){
      abline(v=theta_hat$value, col="dodgerblue")
    }
    # adjusting plotting margins to create legend
    par(mar=c(0,0,0,0))
    plot(0,0, type="n", axes=FALSE, xlab="", ylab="") # creating null plot
    # creating a legend for the null plot, which is effectively the legend for the above
    # plot
    # we need to create two different legends--one to be used if EAP is selected, the other
    # if it is not
    if(exists("theta_hat")==TRUE){
      legend("center",
             legend=c(paste0("Response Function (",student$name," answered correctly)"), 
                      paste0("Response Function (",student$name," answered incorrectly)"),
                      paste0(student$name,"'s EAP")), # providing items in legend
             col=c("forestgreen","firebrick1","dodgerblue"), 
             # providing color for each item in legend
             lty = c(1,1,1)) # providing line type for each item in legend
    } else {
      legend("center",
             legend=c(paste0("Response Function (",student$name," answered correctly)"), 
                      paste0("Response Function (",student$name," answered incorrectly)")), 
                      # providing items in legend
             col=c("forestgreen","firebrick1"), 
             # providing color for each item in legend
             lty = c(1,1)) # providing line type for each item in legend
    }
    par(opar) # resetting margins
  }
  # we create plots for each of the questions selected, by the index of the question in
  # the questions specified
  plots <- sapply(1:length(questions), function(x) plotting_function(x))
}

plotting(Bob, questions=c(5:7), theta_EAP = TRUE) # checking to see if function works
