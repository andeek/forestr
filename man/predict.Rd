% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict}
\alias{predict}
\alias{predict.forestr}
\title{Predict Method for forestr objects}
\usage{
\method{predict}{forestr}(object, newdata, ...)
}
\arguments{
\item{object}{an object of class forest as created by \code{forest}}

\item{newdata}{dataframe containing new data}

\item{...}{not currently used}
}
\value{
A list with the following components
   \item{response}{the predicted classes or values for each observation}
   \item{votes}{if \code{object$type} is classification, the vote matrix used to create the prediction}
}
\description{
Predict methods to predict from forestr tree and random forest
within forestr using the \code{predict} function
}
\examples{

mtcars.forestr <- forestr(factor(cyl) ~ ., data = mtcars[1:25, ])
predict(mtcars.forestr, mtcars[26:32, ])

}

