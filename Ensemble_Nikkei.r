############################################
# Predicting the Nikkei
# ---------------------------------
# Created by  : Adam Nguyen
# Updated by  : Adam Nguyen
# Created at  : 10/01/2014
# Updated at  : xx/xx/xxxx
# Description : Ensemble Methods
#############################################
#http://indexes.nikkei.co.jp/en/nkave/index

#Set Directory
setInternet2(TRUE)
Sys.setenv(language = "en", tz = 'UTC')
options(max.print = 500)
setwd("C:/Users/adam.nguyen/Desktop/R")
source(paste(c("C:/Users/adam.nguyen/Desktop/R/GH/", "library.R"), collapse = "")) #Get library
source(paste(c("C:/Users/adam.nguyen/Desktop/R/GH/", "break_munging.txt"), collapse = "")) #Get tools

#Install appropriate libraries
library(shiny)
library(quantmod)

##Required functions
eigenvar <- function(x){
    #Check creates a table for eigenvariances
    x <- as.matrix(x)
    x.scale <- scale(x)
    x.cov <- cov(x.scale)
    x.eigen <- eigen(x.cov)
    x.eigen$eigenvar <- x.eigen$values / sum(x.eigen$values)
    x.eigen$cumsum <- cumsum(x.eigen$eigenvar)
    return(x.eigen)
}

binarize <- function(x){
    #Converts vector into binary where > 0 == 1
    returns.binary <- NULL
    for (i in 1:length(x)){
        if (x[i] > 0){
            returns.binary[i] <- 1
        }else{
            returns.binary[i] <- 0
        }
    }
    return(returns.binary)
}

#Build returns dataset
#N225 <- getSymbols(symbols, src='yahoo', from = "1997-01-01", to = '2014-09-25')
symbols <- c('^N225')
N225 <- read.csv('C:/Users/adam.nguyen/Desktop/nikkei.csv')
N225$returns <- c(NaN, (N225[2:nrow(N225), 2] / N225[1:nrow(N225) - 1, 2] - 1))

#Establish Teradata Connection
#RShowDoc(“teradataR”, package = “teradataR”)
Teradata.Connect <- tdConnect("Teradata2750")

#Query Teradata via R for sales data
data.raw <- tdQuery(
"query"
)

#Convert Dataset into Dataframe
data.df <- as.data.frame(data.raw)
data.cast <- cast(data.df, datetime ~ g1, sum)
#write.csv(data.cast, "returns.csv")

#Read CSV filed
data.cast <- read.csv("returns.csv")

#Synchronize time series
data.recast <- data.cast[-nrow(data.cast)]
data.time <- data.cast[, 1] #Depends on input source
data.recast <- data.cast[, -c(1)] #Removes index

#Insert Binary Target Variable and dates
data.recast$TGT <- N225$returns[4:(nrow(N225) - 1)]

#Remove Zero Variance Columns
data.var <- data.recast[, apply(data.recast, 2, var) != 0]

##Reduce Dimensionality of the dataset via SVD
data.svd <- svd(scale(data.var)) #Scale before SVD
plot(data.svd$d)
sum((data.svd$d^2)[1:22] / sum(data.svd$d^2)) #Check share of variance

#Create reduce row approximation based on 99% information
data.red <- data.svd$u[, 1:22] %*% diag(data.svd$d[1:22]) %*% t(data.svd$v[, 1:22])
dim(data.var)
dim(data.red) # Truncated n-components
data.red <- as.data.frame(data.red)
names(data.red) <- names(data.var)
str(data.red)

#Create binary set
data.red$TGT <- binarize(N225$returns[4:(nrow(N225) - 1)])
data.var$TGT <- binarize(N225$returns[4:(nrow(N225) - 1)])

#Compare pre and post SVD
data.red[1:25 ,(ncol(data.red) - 1):ncol(data.red)]
data.var[1:25 ,(ncol(data.red) - 1):ncol(data.red)]

#Split dataframe
data.split <- splitdf(data.red, train = .7, test = .3, seed = 123)
data.train <- data.split$train
data.test <- data.split$test

###Filter by time series period rather than random sample then re-run and calculate ROC
#Ensemble files
