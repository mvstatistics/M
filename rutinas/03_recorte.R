#
rm(list = ls())

#
library(stringr)
library(tidyverse)
#
# trimm factores de expansion
#
source("./rutinas_fexp/funciones/trimming.R")

index <- list.files("bases/ribd/");j=1

  # Base de personas
  base <- readRDS(paste0("bases/tratadas/personas_fexp/personas_fexp",index[j],".rds"))

  base = base %>%
    mutate(dom7 = ifelse(dominio == "06" | dominio == "07" | dominio == "08" | (dominio == "12" & area == 1), "06",
                         ifelse(dominio == "09" | dominio == "10" | dominio == "11" | (dominio == "12" & area == 2), "07",
                                dominio)))
 
  # Recorte
  indice <- sort(unique(base$estrato))
  
  for(i in 1:length(indice)){
    
    l1 <- base %>% 
      filter(estrato==indice[i]) %>% 
      mutate(fexp_rec35 = trimming(fexp_aju, 3.5),
             fexp_rec45 = trimming(fexp_aju, 4.5),
             fexp_rec50 = trimming(fexp_aju, 5))
    
    if(i==1){
      base_fexp <- l1
    }else{
      base_fexp <- rbind(l1, base_fexp)
    }
  }
  
  
  print(index[j])
  print("Descriptivos de factores de expansión:")
  
  print(summary(base_fexp$fexp_aju))
  print(summary(base_fexp$fexp_rec35))
  print(summary(base_fexp$fexp_rec45))
  print(summary(base_fexp$fexp_rec50))
  
  # Guardar las tablas del porcentaje de factores de expansion recortados
   
  saveRDS(base_fexp, paste0("bases/tratadas/personas_fexp/personas_recortado",index[j],".rds"))
  
# Tablas de porcentaje de recorte 4.5
x_rec_45 <- as.data.frame.matrix(table(base_fexp$estrato,
                                       base_fexp$fexp_aju>base_fexp$fexp_rec45))
x_rec_45 = x_rec_45 %>%
  mutate(prop = `TRUE`/(`FALSE` + `TRUE`)*100)


x_rec_3.5 <- as.data.frame.matrix(table(base_fexp$estrato,
                                       base_fexp$fexp_aju>base_fexp$fexp_rec35))
x_rec_3.5 = x_rec_3.5 %>%
  mutate(prop = `TRUE`/(`FALSE` + `TRUE`)*100)



