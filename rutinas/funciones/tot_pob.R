tot_pob <- function(bp, dominio, si.area=F, si.sexo=F, gedad, anio){
    
    nc <- length(gedad)/2
    
    # if(nc>1){
    #     for(i in 1: nc){
    #         if(i==1){
    #             v1 <- c(0, gedad[i]-1)
    #         }else if(i>1 & i<nc){
    #             v1 <- c(gedad[i-1], gedad[i]-1)
    #         }else if(i==nc){
    #             v1 <- c(gedad[i-1], gedad[i]-1, gedad[i], 100)
    #         }
    #         
    #         if(i==1){
    #             vr <- v1
    #         }else{
    #             vr <- c(vr, v1)
    #         }
    #     }
    # }else{
    #     vr <- c(0, gedad[i]-1, gedad[i], 100)
    # }
    # rm(i, v1)
    
    mge <- as.character(rep(1:nc, gedad[1:nc*2] - gedad[1:nc*2-1] + 1))
    
    print(glue("Los grupos de edad a considerar son {length(gedad)/2}, con conteo {table(mge)}"))
    
    b0 <- bp %>% 
        filter(edad>=min(gedad) & edad<=max(gedad))
    
    b1 <- cbind(b0, gedad=mge)
    
    rm(nc, mge)
    
    dominio <- as.name(dominio)
    dominio <- enquo(dominio)
    
    anio <- as.name(anio)
    anio <- enquo(anio)
    
    gedad <- "gedad"
    gedad <- as.name(gedad)
    gedad <- enquo(gedad)
    
    area <- "area"
    area <- as.name(area)
    area <- enquo(area)
    
    sexo <- "sexo"
    sexo <- as.name(sexo)
    sexo <- enquo(sexo)
    
    if(si.area==T & si.sexo==T){
        
        b1 <- b1
        
    }else if(si.area==T & si.sexo==F){
        
        b1 <- mutate(b1, sexo="0")
        
    }else if(si.area==F & si.sexo==T){
        
        b1 <- mutate(b1, area="0")
        
    }else{
        
        b1 <- mutate(b1, area="0", sexo="0")
        
    }
    
    b2 <- do.call(select, list(b1, dominio, area, sexo, gedad, anio))
    b2 <- do.call(group_by, list(b2, dominio, area, sexo, gedad))
    b3 <- do.call(summarise_all, list(b2, sum))
    
    return(ungroup(b3))
    
}


