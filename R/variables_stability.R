
#' vars_stability
#'
#' get Komolgorov-Smirnov value test for a variable for a list of periods/or other variable.
#' .
#' @param tbla table with data. It has to have the target_name.
#' @param model the model with the list of variables.
#' @param target_name the name of the target.
#' @param splitter_name the name of the variable that is going to be used to split the data.
#' @import reshape2
#' @keywords
#' @export
#' @examples
#'
#'
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x4='A'
#' x4=ifelse(x1>0.1,'B', x4)
#' x4=ifelse(x1>0.4,'C', x4 )
#' x4=ifelse(x1>0.6,'D', x4 )
#' x4=ifelse(x1>0.8,'E', x4 )
#' per=c(rep(201810,200),rep(201812,400),rep(201901,400))
#' z = 1 + 3*x1 + x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x4=x4, per=per)
#' formula_model=formula(y~x2+x1+x4)
#' aa=vars_stability(tbla, formula_model,'y', 'per')
#' aa[1]
#' aa[2]
#' aa[3]
#' aa[4]


vars_stability<-function(tbla, formula_model, target_name, splitter_name){
  res_final=data.frame()
  mean_dist_final=data.frame()
  tbla<-data.frame(tbla)
  splitter_levels=sort(unique(tbla[,splitter_name]))
  for (splitter_level in splitter_levels){
    #splitter_level=splitter_levels[1]
    tbla_nivel<-tbla[tbla[, splitter_name]==splitter_level,]
    mean_dist=mean_dist_var_list(tbla_nivel, formula_model)
    mean_dist$nivel=splitter_level
    res_nivel=ks_var_list(tbla_nivel, formula_model, tbla_nivel)##ks de hacer un modelo con esa sola
    res_nivel$nivel=splitter_level
    res_nivel<-res_nivel[res_nivel$only_one_var!='all',]
    res_nivel$ks_test<-NULL
    res_nivel$ks=round(res_nivel$ks,2)
    res_final=rbind(res_final, res_nivel)
    mean_dist_final=rbind(mean_dist_final,mean_dist)
  }

  colnames(res_final)<-c('variable', 'ks', 'nivel')
  res_final$variable<-as.character(res_final$variable)
  res_final<-res_final[order(res_final$variable),]
  res_final<-res_final[,c('variable', 'nivel','ks')]
  variables_names<-sort(unique(as.character(res_final$variable)))
  res_final2<-dcast(res_final,nivel~variable, value.var='ks')
  res_final2<-res_final2[,c('nivel', variables_names)]

  mean_dist_final2<-dcast(mean_dist_final,nivel~variable+nivel_factor+tipo, value.var='val_o_partic')

  return(list(res_final, res_final2, mean_dist_final,mean_dist_final2))
}

