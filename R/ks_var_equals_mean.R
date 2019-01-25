
#' ks_var_equals_mean
#'
#' get Komolgorov-Smirnov value test for an already trained model imputing each variable by its mean one by one,
#' .
#' @param tbla table with data. It has to have the target_name
#' @param modelo_object the model trained to predict the target variable.
#' @param formula_model the final formula of the model.
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'formula_model=as.formula(vs~ mpg+cyl+disp)
#'tbla=mtcars
#'modelo_object = glm(formula_model, family=binomial, data = tbla)
#'tbla$pred_prob<-predict(modelo_object, tbla,type=c("response"))

#'ks_var_equals_mean(tbla, modelo_object, formula_model)

ks_var_equals_mean<-function (tbla, modelo_object, formula_model){
  target_name<-colnames((model.frame(formula_model, data=tbla)))[1]
  tbla$target<-tbla[,target_name]

  #ks using all variables
  set.seed(55555)
  mod = glm(formula_model, family=binomial, data = tbla)
  tbla$pred_prob<-predict(mod, tbla, type=c("response"))
  vars=length(attr(terms(mod), "term.labels"))
  ks_todas=ks_obtener(tbla, pred_prob_name='pred_prob', target_name)
  #ks impute one variable each time by its mean
  lista<-attr(terms(mod), "term.labels")
  result_df=data.frame(variable_mean=c('none'), ks=ks_todas)
  for (i in lista) {
    tbla0<-tbla
    tbla0[,i]<-0
    tbla0$pred_prob<-predict(mod, tbla0, type=c("response"))
    ks_i=ks_obtener(tbla0, pred_prob_name='pred_prob', target_name)
    result_df<-rbind(result_df,
                     data.frame(variable_mean=as.character(i), ks=ks_i))
    }

  result_df0<-result_df[result_df$variable_mean=='none',]
  result_df_resto<-result_df[result_df$variable_mean!='none',]
  result_df_resto<-result_df_resto[order(result_df_resto$ks),]
  return(rbind(result_df0, result_df_resto))
}
