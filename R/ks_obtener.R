
#' ks_obtener
#'
#' get Komolgorov-Smirnov value test for an already trained model, given data, predicted probabilities and target name.
#' .
#' @param tbla table with data. It has to have the target_name
#' @param pred_prob_name the name fo the column with predicted probabilities.
#' @param target_name the target variable used to train the model.
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'formula_model=as.formula(vs~ mpg+cyl+disp)
#'tbla=mtcars
#'modelo_object = glm(formula_model, family=binomial, data = tbla)
#'tbla$pred_prob<-predict(modelo_object, tbla,type=c("response"))

#'ks_obtener(tbla,modelo_object, target_name='vs')

ks_obtener<-function (tbla, pred_prob_name, target_name) {
  probs_pos<-tbla[tbla[,target_name]==1, pred_prob_name]
  probs_neg<-tbla[tbla[,target_name]==0, pred_prob_name]
  return (as.numeric(ks.test(probs_pos,probs_neg)[1]))
}

