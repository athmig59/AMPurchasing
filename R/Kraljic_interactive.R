#' @title Kraljic Matrix
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#
#'
#' @description
#' In purchasing and supply chain management, the Kraljic matrix (or Kraljic model or portfolio) is a method used to
#' segment the purchases or suppliers of a company by dividing them into four categories; Non-critical,
#' Bottleneck, Leverage and Bottlemneck based on attributes, typically risk and profitability.
#' The function provides for interactive input of subjective user estimations of these attributes and subsequently
#' categorizes purchases or suppliers based on an estimation of a user utility function.
#'
#' @details
#' The function requires the package \code{tcltk} to pop up interactive messages and the
#' package \code{KraljicMatrix} which provides implementation of necessary functions for the
#' classification of the items and the graphical presentation of the Kraljic portfolio. The latter
#' requires the package \code{ggplot2} too.
#'
#' @param{Items} {an item catalogue in the form of a vector of character elements (strings)}
#' @param{attribute1} {a string holding the name of the first attribute}
#' @param{attribute2} {a string holding the name of the second attribute}
#'
#' @return A list containing;
#' \itemize{
#' \item\code{Scales} {a data frame containing the limits of the scales for the user estimation of the attributes, as specified by the user}
#' \item\code{Table} {a data frame containing the user input for each item, the utility scores and the categorization of each of them}
#' \item\code{Matrix} {a \code{ggplot2} figure of the Kraljic matrix which can be displayed with the \code{print()} function}
#' }
#'
#' @references
#' - Kraljic, P. (1983) Purchasing Must Become Supply Management, Harvard Business Review, September 1983
#' - van Weele, A. J. (2014) Purchasing & Supply Chain Management, Cengage.
#'
#' @examples
#' \dontrun{
#'
#' ## Product names
#'
#' Items <- c("aaa", "bbb", "ccc", "ddd")
#'
#' ## Attributes
#'
#' attribute1 <- "Risk"
#' attribute2 <- "Profitability"
#'
#' ## Run
#'
#' res <- Kraljic_interactive(Items,attribute1,attribute2)
#'
#' ## Check the scales used
#'
#' res$Scales
#'
#'      Risk Profitability
#' Min    1      1
#' Max    5     10
#'
#' ## check Table (depending on user input, it can be similar to)
#'
#' res$Table
#'
#'    Items Risk Profitability Attr1Score Attr2Score     Category
#' 1   aaa    1     10         0.00000    1.0000000    Strategic
#' 2   bbb    5      1         1.00000    0.0000000 Non-critical
#' 3   ccc    3      5         0.50005    0.6208212     Leverage
#' 4   ddd    3      7         0.50005    0.8096883     Leverage
#'
#' ## Plot the Kraljic matrix
#'
#' print(res$Matrix)
#'
#' }
#'
#'
## if(!require("ggplot2")){ install.packages("ggplot2") }
## if(!require("tcltk")){ install.packages("tcltk") }
## if(!require("KraljicMatrix")){ install.packages("KraljicMatrix") }
##??## if(!require("DataEditR")){ install.packages("DataEditR") }
#'
#'
#'@export
Kraljic_interactive <- function(Items, attribute1, attribute2){

  numItems <- length(Items)

  # Check number of items (should be at least two)

  if(numItems <= 1){return(NULL)}

  x <- rep(NaN,numItems)
  y <- rep(NaN,numItems)

  df <- data.frame(Items, x, y)
  names(df) <- c("Items", attribute1, attribute2)

  msg <- "You are requested to provide the minimum and maximum values for the scales in which the attributes are measured.\n\nThe scales of the attributes can differ.\n\nClick OK to proceed!"
  tcltk::tkmessageBox(title="Kraljic Matrix Message", message=msg)

  scales <- data.frame(c(NaN, NaN), c(NaN, NaN))
  colnames(scales) <- c(attribute1,attribute2)
  rownames(scales) <- c("Min", "Max")

  scales <- edit(scales)

  # Check scales

  if( any( is.na(scales) ) ) {return}

  if( (scales[1,1] >= scales[2,1]) || (scales[1,2] >= scales[2,2]) ) {return(NULL)}

  msg <- "You are requested to provide your estimates of the attributes for each item in the prespicified scales.\n\nClick OK to proceed!"
  tcltk::tkmessageBox(title="Kraljic Matrix Message", message=msg)

  df <- edit(df)

  # Check scales & judgements

  if( any(is.na(df[,2])) || any(is.na(df[,3])) ) {return(NULL)}

  if( prod(df[,2] < scales[1,1]) || prod(df[,2] > scales[2,1]) ) {return(NULL)}
  if( prod(df[,3] < scales[1,2]) || prod(df[,3] > scales[2,2]) ) {return(NULL)}

  mx <- min(5, scales[2,1])
  my <- min(5, scales[2,2])

  xpoints <- unique( sort( round( runif(mx, min=min( df[,2] ), max=max( df[,2] ) ) ) ) )
  ypoints <- unique( sort( round( runif(my, min=min( df[,3] ), max=max( df[,3] ) ) ) ) )

  msg <- "You are requested to provide the desirability of a few random values of each attribute in a scale from 0 to 1.\n\nThe purpose is to estimate a utility function for your preferences.\n\nClick OK to continue!"
  tcltk::tkmessageBox(title="Kraljic Matrix Message", message=msg)

  xutils <- rep(NaN,length(xpoints))
  yutils <- rep(NaN, length(ypoints))

  dfx <- data.frame(xpoints, xutils)
  dfy <- data.frame(ypoints, yutils)

  colnames(dfx) <- c(attribute1, "Utility")
  colnames(dfy) <- c(attribute2, "Utility")


  msg <- paste("Determination of utility function with respect to", attribute1, "\n\nClick OK to continue!")
  tcltk::tkmessageBox(title="Kraljic Matrix Message", message=msg)

  dfx <- edit(dfx)
  if( any(dfx[,2] < 0) || any(dfx[,2] > 1) || any(is.na(dfx[,2])) ){ return(NULL) }


  msg <- paste("Determination of utility function with respect to", attribute2, "\n\nClick OK to continue!")
  tcltk::tkmessageBox(title="Kraljic Matrix Message", message=msg)

  dfy <- edit(dfy)
  if( any(dfy[,2] < 0) || any(dfy[,2] > 1) || any(is.na(dfy[,2])) ){ return(NULL) }

  rhox <- KraljicMatrix::SAVF_preferred_rho(desired_x = dfx[,1], desired_v = dfx[,2], x_low=scales[1,1], x_high=scales[2,1], rho_low=0, rho_high=1)
  rhoy <- KraljicMatrix::SAVF_preferred_rho(desired_x = dfy[,1], desired_v = dfy[,2], x_low=scales[1,2], x_high=scales[2,2], rho_low=0, rho_high=1)

  xscore <- KraljicMatrix::SAVF_score(x=df[,2], x_low=scales[1,1], x_high=scales[2,1], rho=rhox)
  yscore <- KraljicMatrix::SAVF_score(x=df[,3], x_low=scales[1,2], x_high=scales[2,2], rho=rhoy)

  #dfscore <- data.frame(xscore,yscore)
  #names(dfscore)<- c( paste("Score(",attribute1,")"), paste("Score(",attribute2,")") )

  #df <- data.frame(df, dfscore)

  df <- data.frame(df, Attr1Score=xscore, Attr2Score=yscore)

  gm <- KraljicMatrix::kraljic_matrix(df, Attr1Score, Attr2Score )
  gm <- gm + ggplot2::ggtitle("Kraljic Matrix") + ggplot2::xlab(attribute1)+ggplot2::ylab(attribute2)

  category <- KraljicMatrix::kraljic_quadrant(df$Attr1Score,df$Attr2Score)
  df <- data.frame(df, Category=category)

  return(list(Scales=scales, Table=df, Matrix=gm))
}
