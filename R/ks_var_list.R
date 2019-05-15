
#' ks_var_list
#'
#' get Komolgorov-Smirnov value test for a variable in train and test(receives a list or variables)
#' .
#' @param tbla table with data. It has to have the target_name.
#' @param formula_model the final formula of the model.
#' @param tbla_test table with test data.

#' @keywords
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' z = 1 + 3*x1
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x4=x4)
#' formula_model=formula('y~x1+x2')
#' ks_var_list(tbla, formula_model, tbla)




ks_var_list<-function (tbla, formula_model, tbla_test){
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


  #ks impute one variable each time by its mean
  lista<-attr(terms(mod), "term.labels")
  result_df=data.frame(only_one_var=c('all'), ks=ks_todas, ks_test=ks_test)

  print('performance del modelo de cada variable contra la target')
  for (i in lista) {
    print(i)
    tbla0<-tbla
    #tbla0[,i]<-0#mean(tbla0[,i])

    formula_model0=as.formula(paste0(target_name,' ~ ' , i))

    mod0 = glm(formula_model0, family=binomial, data = tbla0)

    tbla0$pred_prob<-predict(mod0, tbla0, type=c("response"))
    tbla_test$pred_prob<-predict(mod0, tbla_test, type=c("response"))


    ks_i=ks_obtener(tbla=tbla0, pred_prob_name='pred_prob', target_name)
    ks_i_test=ks_obtener(tbla=tbla_test, pred_prob_name='pred_prob', target_name)


    result_df<-rbind(result_df,
                     data.frame(only_one_var=as.character(i), ks=ks_i, ks_test=ks_i_test))
  }

  result_df0<-result_df[result_df$only_one_var=='all',]
  result_df_resto<-result_df[result_df$only_one_var!='all',]
  result_df_resto<-result_df_resto[order(-result_df_resto$ks),]
  return(rbind(result_df0, result_df_resto))
}
