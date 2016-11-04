\name{RegSL}
\alias{RegSL}
\title{Adjust a regression model using lasso and a simple regression using lasso variables, using part of the time series as a holdout sample}
\usage{
RegSL(y,x,lagy=0,lagx=0,ini=0,out.of.sample=0)
}


\arguments{
  \item{y}{time series}

  \item{x}{covariables}
  
  \item{lagy}{how many lags of y should be tested if wanted.}
  
  \item{lagx}{how many lags of x should be tested if wanted.}
  
  \item{ini}{how many times of the beggining of series should be leave out of adjustment to keeping the model with lagy>0 or lagx>0 comparable with models with other lag options.}
  
  \item{out.of.sample}{how many times of the end of series should be leave out of testing sample for testing forecast.}
  
}

\value{
Returns the samples used to adjust and test model, predictions, prediction errors, simple and adjusted coefficients of determination, the estimated coefficients from regression with lasso and simple regression and p-values.
  }

\examples{
x <- cbind(rnorm(30),rnorm(30))
colnames(x)=c("x1","x2")
y <- x[,1] + x[,2] + rnorm(30)
tst=RegSL(y=y,x=x,lagy=0,lagx=0,nS=4,out.of.sample=5)
}

\author{
  Coded by Luciene Gomes de Souza, last updated on 04-11-2016.
  }
