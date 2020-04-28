# Buck's Method Package
#.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

.onLoad <- function(lib, pkg) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Alexandria Rhoads", "Siyi Li",
    devtools.desc.author = '"Alexandria Rhoads <acrhoads7@gmail.com> [aut, cre]"', '"Siyi Li <@gmail.com> [aut, cre]"',
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

#' Calculates missing data groups and appends new column to original dataframe
#'
#' This function calculates missing data groups and appends new column
#' to original dataframe and orders it
#' @param x a data frame
#' @param n number of rows
#' @param p number of columns, variables
#' @param r a binary matrix, 1 is mising value, 0 is complete
#' @param group missing pattern groups
#' @param new matrix with original dataframe and missing pattern group appened
#' and ordered as new column
#' @export
miss.group <- function(x) {
  n <- nrow(x)
  p <- ncol(x)
  r <- 1 * is.na(x)
  group <- (r %*% (2^(seq_len(p) - 1))) + 1

  table(group)
  new<- cbind(x, group)

return(new)
}

#' Bucks complete case model & prediction
#'
#' This function defines model based on complete cases and predicts
#' @param m1 model
#' @param formula a model to be fitted
#' @param data from miss.group function
#' @export
model <- function(formula, data){
  m1 <- lm(formula, data)
  p1 <- predict(m1, type="response")

return(p1)
}

#' Bucks impute function
#'
#' This function imputes missing, NA vaules based on defined linear model
#' @param model.pred a vector, model prediction output from function model
#' @param data.output a matrix, datset from function miss.group
#' @param m an integer, variable to impute - enter as column number
#' @param n an integer, group number of interest
#' @export

buck.im <- function(model.pred=as.vector(model.pred), data.output=as.matrix(data), m=as.integer(m), n=as.integer(n)){
  data.output[,m] <-ifelse(is.na(data.output[,m]) & (data[,ncol(data.output)]==n), model.pred, data.output[,m])
  return(data[,m])
}

