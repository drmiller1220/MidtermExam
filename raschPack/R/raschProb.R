#' Evaluating probability of correct responses
#'
#' Determines the probability of a correct response given an ability parameter, and the probability of the observed responses
#'
#' @param raschObj An object of class Rasch
#' @param theta A proposed ability parameter
#'
#' @return A list with the elements
#'  \item{prob_corr}{The probability of a correct answer given the proposed ability parameter}
#'  \item{prob_obs}{The probability of the observed answers given the proposed ability parameter} 
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschProb
#' @export
setGeneric(name="raschProb",
           def=function(raschObj,theta,...)
           {standardGeneric("raschProb")}
           )

#' @export
setMethod(f="raschProb",
          definition= function(raschObj,theta,...){
            prob_corr_question <- exp(theta - raschObj@difficulty)/ 
              (1+ exp(theta - raschObj@difficulty))                  
            # probability that the student answers each question correctly
            # given difficulty and theta
            prob_obs_answer <- ifelse(raschObj@correct_answers==1, 
                                      prob_corr_question, 
                                      1-prob_corr_question) 
                                          # given our theta and difficulty, we want the
                                          # probability that we observe the student's
                                          # observed right or wrong answer; ifelse
                                          # returns P_ij if the student answered 
                                          # correctly, and Q_ij=1-P_ij if the student
                                          # answered incorrectly
            return(list(prob_corr=prob_corr_question, prob_obs=prob_obs_answer)) 
            # return a list containing the two vectors requested; P_ij for each question 
            # and probability of observed student answer
          }
          )