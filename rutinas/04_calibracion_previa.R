#
rm(list=ls())
#
library(rio)
library(srvyr)
library(tidyverse)
#
# Base de personas

index <- list.files("bases/ribd/"); i=1

  base <- readRDS(paste0("bases/tratadas/personas_fexp/personas_recortado",index[i],".rds"))
  # 1) creación de variables id_calib

  base <- base %>%
      rename(sexo = p02,
             edad = p03) %>% 
      mutate(gedad = ifelse(edad<15, 1,
                            ifelse(edad>=15 , 2, NA)),
             id_calib= paste0("00", "_", area, "_", sexo, "_", gedad))
  
    print(index[i])
  
  print("Número de grupos de calibración (dom_area_sexo_gedad):")
  n_distinct(base$id_calib)
  print(table(base$id_calib, useNA = "ifany"))
  
  # Creación del diseño de muestreo
  #
  est_pob <- base %>%
    as_survey_design(ids = id_upm,
                     strat = estrato, 
                     weights = fexp_aju,
                     nest = T)
  options(survey.lonely.psu="adjust")
  
  sum(weights(est_pob))
  
  # Coeficientes de variación 
  est_pob_v <- est_pob %>%
    group_by(id_calib) %>% 
    summarise(n = unweighted(n()),
              var1 = survey_total(vartype="cv", na.rm=T)) %>% 
    mutate(control = ifelse(var1_cv>0.10, 1, 0))
  
  print("CV máximo sobre grupos de calibración:")
  print(paste0(round(max(est_pob_v$var1_cv)*100,2),"%"))
  #
  # guardar en excel
  #
  saveRDS(est_pob_v, paste0("resultados/est_pob_v.rds"))
  
    
  export(est_pob_v, paste0("productos/medidas de calidad/calibracion/cv_cal_previo_",index[i],".xlsx"))


