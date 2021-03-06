% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ks_obtener.R
\name{ks_obtener}
\alias{ks_obtener}
\title{ks_obtener}
\usage{
ks_obtener(tbla, pred_prob_name, target_name)
}
\arguments{
\item{tbla}{table with data. It has to have the target_name}

\item{pred_prob_name}{the name fo the column with predicted probabilities.}

\item{target_name}{the target variable used to train the model.}
}
\description{
get Komolgorov-Smirnov value test for an already trained model, given data, predicted probabilities and target name.
.
}
\examples{
set.seed(1)
formula_model=as.formula(vs~ mpg+cyl+disp)
tbla=mtcars
modelo_object = glm(formula_model, family=binomial, data = tbla)
tbla$pred_prob<-predict(modelo_object, tbla,type=c("response"))
ks_obtener(tbla,modelo_object, target_name='vs')
}
