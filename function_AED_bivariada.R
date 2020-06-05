

## Definindo Vars Explicativas, ajustanto classe e criando tabela com def de nome e Classe das mesmas ## 

AED_biv <- function(Base, Response_var, Analyse = "Pre" ) {
  
#Placing the y factor to the end of the table so it will not be valuated.  
  only.y <- Base[ ,glue("Response"), drop = FALSE]
  only.x <- Base[ , !(names(Base) %in% names(only.y))] %>% as.data.frame()  
  
  Base <- cbind(only.x, only.y)
    
x = (names(Base))

z= NULL
for (i in 1:length(x)){
  
  z[i] = class(Base[,i])
  
}

x = (cbind(x,z))
colnames(x) = c("nome", "classe")
vars = colnames(Base)

############### Gerar dist pre e pos Binning #################
if(Analyse == "Pre") {
  for(i in 1:(length(Base))) {
    
      if( x[i,2] %in% c("character","factor")){ #Caso Variaveis categoricas
      prop1 <- Base[!is.na(Base[i]),] %>% # PROPENS?O POR CATEGORICAS INICIALMENTE
        group_by(get(vars[i])) %>%
        count(as.factor(get(glue(Response_var)))) %>%            # group_by() & summarise(n = n()) are implicit
        mutate(prop = prop.table(n)) 
      
      colnames(prop1) =c(vars[i],"Response_var", "n", "prop") 
      
      p1 <- ggplot(data= filter(prop1, Response_var == 1), aes(x= get(vars[i]), y = prop))+
        geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)  +
        geom_hline(yintercept = mean(select(Base, Response_var)[1:nrow(select(Base, Response_var )),]), col = "red", linetype = 'dashed')+
        scale_y_continuous(labels = scales::percent)+ xlab(glue(vars[i]))+
        theme(axis.text.x = element_text(angle = 90)) 
        
      p2 <- ggplot(data= Base, aes(x= get(vars[i])))+
        geom_bar(alpha = 0.6)+
        facet_grid(get(glue(Response_var)) ~., scales = "free")
      
  
      grid.arrange(p1,p2)#, p3, p3.3)
        } 
      
          else{  #Caso Var numerica  
      p1 <- ggplot(data= Base, aes(x= get(vars[i]), fill = as.factor(get(glue(Response_var))) ))+
            geom_density(alpha = 0.6)+ xlab(glue(vars[i])) 


grid.arrange(p1)
  }
    } }  
####### pre e pos binning  
  else {
    for(i in 1:(length(Base)/2-.5)) {
    if( x[i,2] %in% c("character","factor")){ #Caso Variaveis categoricas
    prop1 <- Base[!is.na(Base[i]),] %>% # PROPENS?O POR CATEGORICAS INICIALMENTE
      group_by(get(vars[i])) %>%
      count(as.factor(get(glue(Response_var)))) %>%
      mutate(prop = prop.table(n)) 
    
    colnames(prop1) =c(vars[i],"Response_var", "n", "prop") 
    
    p1 <- ggplot(data= filter(prop1, Response_var == 1), aes(x= get(vars[i]), y = prop))+
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)  +
      geom_hline(yintercept = mean(select(Base, Response_var)[1:nrow(select(Base, Response_var )),]), col = "red", linetype = 'dashed')+
      scale_y_continuous(labels = scales::percent)+ xlab(glue(vars[i]))+
      theme(axis.text.x = element_text(angle = 90)) 
    
    p2 <- ggplot(data= Base, aes(x= get(vars[i])))+
      geom_bar(alpha = 0.6)+
      facet_grid(get(glue(Response_var)) ~., scales = "free") + xlab(glue(vars[i]))
    
    prop2 <- Base %>% #PROPENSAO POR CATEGORICAS POS BINNING
      group_by(get(paste(glue("{vars[i]}.binned")))) %>%
      count(as.factor(get(Response_var))) %>%            # group_by() & summarise(n = n()) are implicit
      mutate(prop = prop.table(n))

    colnames(prop2) =c(glue("{vars[i]}.binned"),"Response_var", "n", "prop")
    
    p3 <- ggplot(data= prop2[prop2$Response_var==1,], aes(x= get(paste(glue("{vars[i]}.binned"))), y = prop))+
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)  +
      geom_hline(yintercept = mean(select(Base, Response_var)[1:nrow(select(Base, Response_var )),]), col = "red", linetype = 'dashed')+
      scale_y_continuous(labels = scales::percent)+ xlab(glue("{vars[i]}.binned"))+
      theme(axis.text.x = element_text(angle = 90))
   
    p3.3 <- ggplot(data= Base, aes(x= get(paste(glue("{vars[i]}.binned"))) ))+
      geom_bar(alpha = 0.6) + xlab(glue("{vars[i]}.binned"))
    
    grid.arrange(p1,p2, p3, p3.3)
  } 
    
    else{  #Caso Var numerica  
      p1 <- ggplot(data= Base, aes(x= get(vars[i]), fill = as.factor(get(glue(Response_var))) ))+
        geom_density(alpha = 0.6)+ xlab(glue(vars[i])) 
      
      prop3 <- Base %>% #PROPENSAO POR CATEGORICAS POS BINNING
        group_by(get(paste(glue("{vars[i]}.binned")))) %>%
        count(as.factor(get(Response_var))) %>%            # group_by() & summarise(n = n()) are implicit
        mutate(prop = prop.table(n))

      colnames(prop3) =c(glue("{vars[i]}.binned"),"Response_var", "n", "prop")

      p2 <- ggplot(data= prop3[prop3$Response_var==1,], aes(x= get(glue("{vars[i]}.binned")), y = prop))+
        geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)  +
        geom_hline(yintercept = mean(select(Base, Response_var)[1:nrow(select(Base, Response_var )),]), col = "red", linetype = 'dashed')+
        scale_y_continuous(labels = scales::percent)+ xlab(glue("{vars[i]}.binned"))+
       theme(axis.text.x = element_text(angle = 360))

      p2.2 <- ggplot(data= Base, aes(x= get(paste(glue("{vars[i]}.binned"))) ))+
        geom_bar(alpha = 0.6)+ xlab(glue("{vars[i]}.binned"))
      
      grid.arrange(p1,p2,p2.2)
    }
    
  }  
    
  }
}
