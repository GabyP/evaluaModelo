
#' mean_dist_var_list
#'
#' get mean value for a lis of variables (numeric or factors)
#' .
#' @param tbla table with data. It has to have the target_name.
#' @param formula_model the model with the list of variables.
#' @keywords
#' @import dplyr
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
#' mean_dist_var_list(tbla, formula_model)
#' formula_model=formula(y~x2+x1)




mean_dist_var_list<-function (tbla, formula_model){
  distris=NULL
  res_num=NULL
  #q_variables=length(formula_model[[3]])
  mod = glm(formula_model, family=binomial, data = tbla)
  lista<-attr(terms(mod), "term.labels")
  lista=sort(lista)
  for (i in lista){#i=lista[1]
    print(i)
    if(class(tbla[,i])%in% c('numeric', 'integer', 'bit64') ){
      media=round(mean(tbla[,i], na.rm=T),2)
      res_num=data.frame(variable=i, tipo='numeric', nivel_factor='', operacion='mean',valor=media, participacion=1)
      if(is.null(res_num)==F ){ res_num$val_o_partic=res_num$valor}
      }
    if(class(tbla[,i])%in% c('character', 'factor') ){#i=lista[3]
      distris=data.frame(tbla%>%group_by_(i)%>%summarise(cant=n()))
      if(is.null(distris)==F ){
        colnames(distris)=c('nivel_factor', 'valor')
        distris$variable=i
        distris$tipo='factor'
        distris$operacion='conteo'
        distris=distris[,c('variable','tipo', 'nivel_factor', 'operacion', 'valor')]
        tot=sum(distris$valor)
        distris$participacion=round(distris$valor/tot,2)
        distris$val_o_partic=distris$participacion
        }
      }
  }
  if(is.null(res_num)==F ){res=res_num}
  if(is.null(distris)==F ){res=distris}
  if(is.null(res_num)==F & is.null(distris)==F){print('ambas'); res=rbind(res_num, distris)}
return(res)
}






