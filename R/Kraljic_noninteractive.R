#' @title Kraljic Matrix
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#
#'
#' @description
#' In purchasing and supply chain management, the Kraljic matrix (or Kraljic model or portfolio) is a method used to
#' segment the purchases or suppliers of a company by dividing them into four categories; Non-critical,
#' Bottleneck, Leverage and Bottlemneck based on attributes, typically risk and profitability.
#' The function provides for input of subjective user estimations of these attributes and subsequently
#' categorizes purchases or suppliers based on an estimation of a user utility function.
#'
#' @details
#' The function the
#' package \code{KraljicMatrix} which provides implementation of necessary functions for the
#' classification of the items and the graphical presentation of the Kraljic portfolio. The latter
#' requires the package \code{ggplot2} too.
#'
#' @param{df} {a data frame with three columns
#' \itemize{
#' \item{Items} {containing strings naming the items to be classified}
#' \item{Attribute1} {containing numerical values (in some scale to be specified) measuring the performance of each item w.r.t. this attribute}
#' \item{Attribute2} {measures w.r.t- this attribute (not necessarily in the same scale as the previous one)}
#'
#' The columns can be named freely but it would be appropriate if the names reflect the case under consideration, for example,
#' \code{Services}, \code{Risk}, and \code{Profitability} respectively.
#' }}
#' @param{scales} {a data frame with two columns corresponding to the scales of Attribute1 and Attribute2, and advisably named after them,
#' and with two rows, mandatorily named \code{Min} and \code{Max} respectively, giving the minimum and maximum value of the respective scale.}
#' @param{utilx} {a data frame corresponding to Attribute1 and having two columns:
#' \itemize{
#' \item{the first column contains some random values within the specified scale of Attribute1}
#' \item{the second column contains the user's estimation of the desirability of the values in the first column expressed in utils in the interval \[0,1\]}
#' }}
#' @param{utily} {a data frame similar to the previous one but corresponding to Attribute2 (can be of different length though)}
#'
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
#' ## Items to be classified
#'
#' Items <- c("aaa", "bbb", "ccc", "ddd")
#' Risk <- c(1, 4, 2, 3)
#' Profitability <- c(10, 5, 5, 7)
#' df <- data.frame(Items, Risk, Profitability)
#'
#' ## Scales used in the attributes
#'
#' scales <- data.frame(Risk = c(1,5), Profitability=c(1,10))
#' rownames(scales) <- c("Min", "Max")
#'
#' ## User utils expressing desirability
#'
#' utilx <- data.frame( Risk = c(1, 3, 5), Utils = c(1, 0.6, 0.08))
#' utily <- data.frame( Profitability = c(1, 3, 5, 8, 10), Utils = c(0.05,0.15,0.35,0.95,1))
#'
#' ## Create Kraljic portfolio
#'
#' res <- Kraljic_noninteractive(df,scales,utilx,utily)
#'
#' ## View the categorization
#'
#' res$Table
#'
#' ## Plot it
#'
#' print(res$Matrix)
#'
#' }
#'
#' @export
Kraljic_noninteractive <- function(df, scales, utilx, utily ){

  attribute1 <- names(df)[2]
  attribute2 <- names(df)[3]

  # Check scales & judgements

  if( any( is.na(scales) ) ) {return}

  if( (scales[1,1] >= scales[2,1]) || (scales[1,2] >= scales[2,2]) ) {return(NULL)}

  if( any(is.na(df[,2])) || any(is.na(df[,3])) ) {return(NULL)}

  if( prod(df[,2] < scales[1,1]) || prod(df[,2] > scales[2,1]) ) {return(NULL)}
  if( prod(df[,3] < scales[1,2]) || prod(df[,3] > scales[2,2]) ) {return(NULL)}

  # Check utilities

  if( any(utilx[,2] < 0) || any(utilx[,2] > 1) || any(is.na(utilx[,2])) ){ return(NULL) }
  if( any(utily[,2] < 0) || any(utily[,2] > 1) || any(is.na(utily[,2])) ){ return(NULL) }


  rhox <- KraljicMatrix::SAVF_preferred_rho(desired_x = utilx[,1], desired_v = utilx[,2], x_low=scales[1,1], x_high=scales[2,1], rho_low=0, rho_high=1)
  rhoy <- KraljicMatrix::SAVF_preferred_rho(desired_x = utily[,1], desired_v = utily[,2], x_low=scales[1,2], x_high=scales[2,2], rho_low=0, rho_high=1)

  xscore <- KraljicMatrix::SAVF_score(x=df[,2], x_low=scales[1,1], x_high=scales[2,1], rho=rhox)
  yscore <- KraljicMatrix::SAVF_score(x=df[,3], x_low=scales[1,2], x_high=scales[2,2], rho=rhoy)

  df <- data.frame(df, Attr1Score=xscore, Attr2Score=yscore)

  gm <- KraljicMatrix::kraljic_matrix(df, Attr1Score, Attr2Score )
  gm <- gm + ggplot2::ggtitle("Kraljic Matrix") + ggplot2::xlab(attribute1)+ggplot2::ylab(attribute2)

  category <- KraljicMatrix::kraljic_quadrant(df$Attr1Score,df$Attr2Score)
  df <- data.frame(df, Category=category)

  return(list(Scales=scales, Table=df, Matrix=gm))
}
