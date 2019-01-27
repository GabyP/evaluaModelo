
#' ks_var_less_one
#'
#' get Komolgorov-Smirnov value test for a model in train and test, deleting one variable at a time
#' .
#' @param tbla table with data. It has to have the target_name. It will be used to train the new model when deleting one variable
#' @param modelo_object the model trained to predict the target variable.
#' @param formula_model the final formula of the model.
#' @param tbla_test table with test data.

#' @keywords
#' @export
#' @examples
#'
#'

ks_var_less_one<-function (tbla, modelo_object, formula_model, tbla_test){
  target_name<-colnames((model.frame(formula_model, data=tbla)))[1]
  tbla=data.frame(tbla)
  tbla_test=data.frame(tbla_test)

  tbla$target<-tbla[,target_name]
  tbla_test$target<-tbla_test[,target_name]

  #ks using all variableslista
  set.seed(55555)
  mod = glm(formula_model, family=binomial, data = tbla)
  tbla$pred_prob<-predict(mod, tbla, type=c("response"))
  tbla_test$pred_prob<-predict(mod, tbla_test, type=c("response"))

  vars=length(attr(terms(mod), "term.labels"))

  ks_todas=ks_obtener(tbla, pred_prob_name='pred_prob', target_name)
  ks_test=ks_obtener(tbla_test, pred_prob_name='pred_prob', target_name)
  print(paste0('ks_train_todas: ', ks_todas, '. Ks_test_todas: ', ks_test))

  #ks impute one variable each time by its mean
  lista<-attr(terms(mod), "term.labels")
  result_df=data.frame(less_one=c('none'), ks=ks_todas, ks_test=ks_test)

  for (i in lista) {#i=lista[1]
    print(i)
    tbla0<-tbla
    #tbla0[,i]<-0#mean(tbla0[,i])

    lista0=gsub(i, '',lista)
    lista0=sort(lista0)
    lista0=lista0[2:length(lista0)]
    formula_model0=as.formula(paste0(target_name,' ~ ' , paste(lista0 , collapse='+')))

    mod0 = glm(formula_model0, family=binomial, data = tbla0)

    tbla0$pred_prob<-predict(mod0, tbla0, type=c("response"))
    tbla_test$pred_prob<-predict(mod0, tbla_test, type=c("response"))


    ks_i=ks_obtener(tbla=tbla0, pred_prob_name='pred_prob', target_name)
    ks_i_test=ks_obtener(tbla=tbla_test, pred_prob_name='pred_prob', target_name)


    result_df<-rbind(result_df,
                     data.frame(less_one=as.character(i), ks=ks_i, ks_test=ks_i_test))
  }

  result_df0<-result_df[result_df$less_one=='none',]
  result_df_resto<-result_df[result_df$less_one!='none',]
  result_df_resto<-result_df_resto[order(-result_df_resto$ks),]
  return(rbind(result_df0, result_df_resto))
}
