#' @title The simple Weighted Sum method
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' Provides for interactive input of subjective user judgements and suppsequently applies
#' the simple Weighted Sum method in order to obtain ranking scores for the alternatives.
#'
#' @details
#' The function requires the package \code{tcltk} to pop up interactive messages and the
#' package \code{MCDA} which provides implementation of the \code{weightedSum} function.
#'
#'
#' @param{criteria} { a vector of character elements naming the criteria under consideration}
#' @param{alternatives} { a vector of character elements naming the alternatives to be evaluated under the specified criteria}
#'
#'
#' @return A list containing:
#' \itemize{
#' \item\code{Weights} {a vector which contains subjective preference weights of the criteria as specified by the user}
#' \item\code{Table} {a matrix which corresponds to the subjective performance estimation of the alternatives with respect to the criteria as provided by the user and, in addition, contains the criteria weights as provided by the user, and the scores computed by the weighted sum heuristic for all alternatives}
#' \item\code{Scores} {a vector which for each alternative provides the weighted sum scores for the alternatives as computed by the process}
#' \item\code{Order} {a vector which contains the numbering (index) of the alternatives in decreasing order}
#' }
#'
### if(!require("tcltk")){ install.packages("tcltk") }
### if(!require("MCDA")){ install.packages("MCDA") }
#'
#'
#' @examples
#' \dontrun{
#'
#' ## -- Example -- ##
#'
#' crit <- c("housing", "salary", "career", "recreation")
#' alt <-c("Stockholm", "Lulea", "Boden", "Malmoe", "Gothenburg")
#' WS_interactive(criteria=crit, alternatives=alt)
#'
#' }
### if(!require("tcltk")){ install.packages("tcltk") }
### if(!require("MCDA")){ install.packages("MCDA") }
#'
#'
#'
#' @export
WS_interactive <- function(criteria, alternatives){

  lc <- length(criteria)
  if( lc <= 1 ){ return(NULL) }

  la <- length(alternatives)
  if( la <= 1 ){ return(NULL) }

  criteria.weights <- rep(NaN,lc)
  names(criteria.weights) <- criteria

  msg <- "You are requested to provide your preference weights of the criteria.\n\nClick OK to proceed!"
  tcltk::tkmessageBox(title="WS Message", message=msg)

  weights <- matrix(nrow=lc,ncol=1)
  weights[,1] <- NaN
  rownames(weights) <- criteria
  colnames(weights) <- "Weights"

  weights <- fix(weights)
  criteria.weights <-  as.vector(weights)

  alternatives.performance <- matrix(nrow=la, ncol=lc)
  alternatives.performance[,] <- NaN
  rownames(alternatives.performance) <- alternatives
  colnames(alternatives.performance) <- criteria

  msg <- "You are requested to provide your judgement of the performance of each alternative with respect to each criterion.\n\nClick OK to proceed!"
  tcltk::tkmessageBox(title="WS Message", message=msg)

  alternatives.performance <- fix(alternatives.performance)

  res <- MCDA::weightedSum(performanceTable=alternatives.performance,
                           criteriaWeights=criteria.weights)

  ind <- 1:length(res)
  ord <- ind[order(res,decreasing =TRUE)]

  alternatives.performance <- rbind(Weights=criteria.weights,alternatives.performance)
  alternatives.performance <- cbind(alternatives.performance,Scores=c(NaN,res))

  return(list(Weights=criteria.weights, Table=alternatives.performance, Scores=res, Order=ord))

}
