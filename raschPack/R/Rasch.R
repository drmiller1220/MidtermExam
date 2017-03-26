#' A Rasch object 
#' 
#' Object of class \code{Rasch} needed to use functions in \code{raschPack}
#'
#' 
#' An object of the class `Rasch' has the following slots:
#' \itemize{
#' \item \code{name} Character vector containing name of student
#' \item \code{difficulty} Numeric vector containing known difficulty parameters of each question
#' \item \code{correct_answers} Numeric vector indicating whether student answered correctly (1) or not (0)
#' }
#'
#' @author David R. Miller: \email{drmiller@@wustl.edu}
#' @rdname Rasch
#' @export
setClass(Class="Rasch",
         slots = c(name='character', difficulty='numeric', correct_answers='numeric'))

#' @export
setValidity("Rasch", function(object){
  length_difficulty <- length(object@difficulty)
  length_correct_answers <- length(object@correct_answers)
  if(length_difficulty!=length_correct_answers){return("@difficulty and @correct_answers must be of the same length")}
})