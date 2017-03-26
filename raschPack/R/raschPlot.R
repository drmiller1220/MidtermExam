#' Plots the item characteristic curves for Rasch objects
#'
#' Produces plots of item characteristic curves for some questions answered by students of
#' class Rasch.  Enables the user to specify whether the EAP should also be plotted.
#' 
#' @param raschObj An object of class Rasch
#' @param lower_bound The smallest value of theta to be included in the plot (default is -10)
#' @param upper_bound The largest value of theta to be included in the plot (default is 10)
#' @param questions The question items to be plotted.  By default, the function plots the
#' first four item characteristic curves on separate plots
#' @param theta_EAP Whether the plots should include the students EAP (default is true)
#'
#' @return Separate plots for each question requested by the user
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschPlot
#' @export
setGeneric(name="raschPlot",
           def=function(raschObj,ower_bound=-10, upper_bound=10, 
                        questions=c(1:4), theta_EAP=TRUE,...)
           {standardGeneric("raschPlot")}
)

# I spent a lot of time trying to figure out why my method for the default "plot" would not
# work; I kept getting an error that `raschObj' was missing from this line:
#probs <- sapply(points, function(x){raschProb(raschObj, x)[[1]]})
# and could not work out the issue in enough time to finish.  Therefore, I made a new generic,
# raschPlot, which I acknowledge is suboptimal to just creating a method for a Rasch class
# object

#' @export
setMethod(f="raschPlot",
          definition= function(raschObj, lower_bound=-10, upper_bound=10, 
                               questions=c(1:4), theta_EAP=TRUE,...){
            # plotting function takes in the student, lower and upper bounds 
            # for the thetas to be
            # plotted on the x axis, the questions the user wants plotted 
            # (each on its own plot),
            # and whether the user wants theta_hat to be plotted
            points <- seq(from=lower_bound, to=upper_bound, by=0.1)
            # generating a vector of points for thetas
            probs <- sapply(points, function(x){raschProb(raschObj, x)[[1]]})
            # calculating the probabilities for each question at each theta
            probs_for_questions <- unlist(probs[questions,])
            # subsetting the probabilities for the questions to be plotted
            questions_correct <- sapply(questions, function(x){raschObj@correct_answers[x]==1})
            #evaluating if the student answers the questions to be plotted correctly
            questions_color <- ifelse(questions_correct==TRUE, "forestgreen", "firebrick1")
            # determining how to color each response function depending on 
            # correctness of answer
            if(theta_EAP==TRUE){ # if the user wants the EAP calculated, we do so
              theta_hat <- raschEAP(raschObj)
            }
            plotting_function <- function(x){ # creating an internal plotting function
              opar <- par(mar=c(5.1, 4.1, 4.1, 2.1))
              # saving defaultmargin parameters so we can restore them after adding the legend
              # for each plot
              layout(matrix(c(1,2), nrow=2), heights = c(0.7,0.3)) # using layout to make
              # room for a legend
              # we plot the probabilities for each theta for the given question,
              # with the appropriate color to indicate whether the answer is correct
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
              # creating a legend for the null plot, which is effectively the legend for
              # the above plot
              # we need to create two different legends--one to be used if EAP is selected,
              # the other if it is not
              if(exists("theta_hat")==TRUE){
                legend("center",
                       legend=c(paste0("Response Function (",raschObj@name," answered correctly)"),
                                paste0("Response Function (",raschObj@name," answered incorrectly)"),
                                paste0(raschObj@name,"'s EAP")), # providing items in legend
                       col=c("forestgreen","firebrick1","dodgerblue"),
                       # providing color for each item in legend
                       lty = c(1,1,1)) # providing line type for each item in legend
              } else {
                legend("center",
                       legend=c(paste0("Response Function (",raschObj@name," answered correctly)"),
                                paste0("Response Function (",raschObj@name," answered incorrectly)")),
                       # providing items in legend
                       col=c("forestgreen","firebrick1"),
                       # providing color for each item in legend
                       lty = c(1,1)) # providing line type for each item in legend
              }
              par(opar) # resetting margins
            }
            # we create plots for each of the questions selected,
            # by the index of the question in the questions specified
            plots <- sapply(1:length(questions), function(x) plotting_function(x))
          }
          )