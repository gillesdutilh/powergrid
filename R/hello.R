## Hello, world!

## This is an example function named 'hello'
## which prints 'Hello, world!'.

## You can learn more about package authoring with RStudio at:

##   http://r-pkgs.had.co.nz/

## Some useful keyboard shortcuts for package authoring:

##   Install Package:           'Ctrl + Shift + B'
##   Check Package:             'Ctrl + Shift + E'
##   Test Package:              'Ctrl + Shift + T'
##' This is the decription, apparently
##'
##' and here are some details.
##' @title the name
##' @param x describes gilles
##' @param bla does nothing
##' @return nothing
##' @author me myself
test <- function(x, bla){
    x <- x + 1 + bla
    y <- mean(x)
    x <- ifelse(x > 5, x, x - 100)
    list(x, y)
}
