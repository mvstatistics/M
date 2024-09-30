
rm(list = ls())
#
library(stringr)
library(srvyr)
library(tidyverse)
library(rio)

index <- list.files("bases/ribd/"); i=1

  # Base de personas
  base_fexp <- readRDS(paste0("./bases/tratadas/personas_fexp/personas_fexp_cal",
                              index[i],".rds"))
  
  
  # Base final que se envía con factor CEPAL calibrado (verdadero)
  fexp_ribd = base_fexp %>%
    mutate(sexo = as.character(sexo),
           panelm = as.character(panelm),
           vivienda = as.character(vivienda),
           hogar = as.character(hogar),
           p01 = as.character(p01)) %>%
    select(id_upm, vivienda, hogar, p01, area, dominio, estrato, panelm, edad, sexo, id_calib,
           fexp_cal_upm) %>%
    arrange(id_upm, panelm, vivienda, hogar, p01)
  
  saveRDS(fexp_ribd, paste0("productos/envio/FEXP_ENEMDU",index[i],".rds"))
  write.table(fexp_ribd, paste0("productos/envio/FEXP_ENEMDU",index[i],".txt"))
  
  print(index[i])
  
  print(sum(fexp_ribd$fexp))

sum(fexp_ribd$fexp_cal_upm)
n_distinct(fexp_ribd$id_upm)
apply(is.na(fexp_ribd), 2, sum)

