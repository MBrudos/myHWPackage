#' My Linear Regression function.
#'
#' This function takes in a list of outcomes, a set of covariates, and a list of row numbers. It is important to not that the list of outcomes has the same number of rows as the set of covariates.
#' The function checks the number of columns and if there are 5 or less in X then a matrix of scatterplots that contain the covariates in X is printed.
#' The function will then run a linear regression using Y as the outcome and X as the explanatory variables before outputting a table that contains the coefficients and the p-values for those coefficients
#' Both the linear regression and the matrix of scatterplots will be created with the subset of data indicated in the "sub" list of row numbers.
#'
#' @param x a matrix of  covariates.
#' @param y a vector  of outcomes.
#' @param sub a list of  subjects (a set of integers corresponding to row numbers in X).
#' @return The coeffcients and p values of \code{x} when \code{y} is the outcome and subset with the rows included in sub.
#' @export
#' @examples
#' myLinearRegression(myData[2:6], myData$Y, c(1:15, 25,27,33,36:45, 51, 55, 57, 59, 63, 66:71, 77, 81, 88, 90:95, 99))
#' myLinearRegression(Iris[2:4], Iris$Sepal.Length, c(1,3,6,7,8:24, 27, 38, 45:50))

myLinearRegression<-  function(x,y, sub){

  if(ncol(x)>5){
    print("Too many variables to plot")
    inter<-cbind(y,x)
    inter_2<-inter[rownames(inter) %in% sub, ]
    reg_model<-lm(y~., data=inter_2)
    coeff<-reg_model$coeff
    pvals<-summary(reg_model)$coefficients[,4]

  }else{
    #library(GGally)
    inter<-cbind(y,x)
    inter_2<-inter[rownames(inter) %in% sub, ]
    print(GGally::ggpairs(inter_2))
    reg_model<-lm(y~., data=inter_2)
    coeff<-reg_model$coeff
    pvals<-summary(reg_model)$coefficients[,4]
  }
  output<-rbind(coeff,pvals)

  return(list(output))
}
