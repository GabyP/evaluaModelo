% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ks_var_equals_mean.R
\name{ks_var_equals_mean}
\alias{ks_var_equals_mean}
\title{ks_var_equals_mean}
\usage{
ks_var_equals_mean(tbla, modelo_object, formula_model)
}
\arguments{
\item{tbla}{table with data. It has to have the target_name}

\item{modelo_object}{the model trained to predict the target variable.}

\item{formula_model}{the final formula of the model.}
}
\description{
get Komolgorov-Smirnov value test for an already trained model imputing each variable by its mean one by one,
.
}
\examples{
set.seed(1)
formula_model=as.formula(vs~ mpg+cyl+disp)
tbla=mtcars
modelo_object = glm(formula_model, family=binomial, data = tbla)
tbla$pred_prob<-predict(modelo_object, tbla,type=c("response"))
ks_var_equals_mean(tbla, modelo_object, formula_model)
}
