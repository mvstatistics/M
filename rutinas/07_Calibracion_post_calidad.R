
rm(list = ls())
#
library(stringr)
library(srvyr)
library(tidyverse)
library(rio)
library(openxlsx)

index <- list.files("bases/ribd/"); i=1

  # Base de personas
  base_fexp <- readRDS(paste0("./bases/tratadas/personas_fexp/personas_fexp_cal",
                              index[i],".rds"))
 
  vis <- readRDS(paste0("./bases/tratadas/vis/vis_", index[i], ".rds"))
  
  
  cotas <- vis  %>% 
    group_by(id_calib) %>% 
    summarise(L = min(cotas),
              U = max(cotas),
              U3 = 3)
  sum(vis$t)
  
  # base_fexp = select(base_fexp, -t)
  
  
  sum(base_fexp$fexp_cal_upm)
  
  
  #
  # estimaciones
  #
  enemdu_upm <- base_fexp %>% 
    as_survey_design(ids = id_upm,
                     strata = estrato,
                     weights = fexp_cal_upm,
                     nest = T)
  
  
  
  options(survey.lonely.psu = "certainty")
  
  
  #
  # Medidas de calidad Silva A.G.
  #
  
  ##### M1 (para ap calcular T_xj en prov.area)
  M1_lista <- vis %>% 
    left_join(base_fexp, by="id_calib") %>% 
    group_by(id_calib) %>% 
    summarise(T_xjC_upm_c = sum(fexp_cal_upm),
              T_xj = mean(t)) %>% 
    group_by(id_calib) %>% 
    summarise(p = n(),
              er_upm_c = sum(abs(T_xjC_upm_c - T_xj)/T_xj)/p)
  
  
  
  
  ##### M2
  M2_lista <- enemdu_upm_c%>% 
    group_by(id_calib) %>% 
    summarise(var1 = survey_total(vartype="cv", na.rm=T)) %>% 
    summarise(n = n(),
              cv = sum(var1_cv)/n) %>% 
    mutate(metodo = "cal_upm_c")
  
  
  
  ##### M3 (minimos) y M4 (maximos)
  M3_lista <- base_fexp %>% 
    left_join(cotas, by = "id_calib") %>% 
    group_by(id_calib) %>% 
    summarise(L = 1,
              n = n(),
              M3_upm_c = sum(g_upm < L),
              M3_upm_prop_c = (M3_upm_c/n)*100)
  
  
  
  M4_lista <- base_fexp %>% 
    left_join(cotas, by = "id_calib") %>% 
    group_by(id_calib) %>% 
    summarise(n = n(),
              U = mean(U),
              U3 = mean(U3),
              M4_upm_U_c = sum(g_upm > U),
              M4_upm_U3_c = sum(g_upm > U3),
              M4_upm_U_prop_c = (M4_upm_U_c/n)*100,
              M4_upm_U3_prop_c = (M4_upm_U3_c/n)*100)
  
  ##### M5
  M5_lista <- base_fexp %>% 
    group_by(id_calib) %>% 
    summarise(n = n(),
              cv_g_upm_c = sd(g_upm)/mean(g_upm))
  
  
  
  
  ##### M6
  M6_lista <- base_fexp %>% 
    group_by(id_calib) %>% 
    summarise(n = n(),
              dist_g_upm_c = sum((fexp_cal_upm - fexp_rec45)^2/fexp_rec45)/n)
  
  
  
  cat(c("Suma: fexp_teo", round(sum(base_fexp$fexp_teo)),
        "Suma: fexp_aju", round(sum(base_fexp$fexp_aju)),
        "Suma: fexp_cal_upm", round(sum(base_fexp$fexp_cal_upm))),
      sep="\n")

  
  #
  Resultados <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(Resultados, "vis")
  addWorksheet(Resultados, "M1_lista")
  addWorksheet(Resultados, "M2_lista")
  addWorksheet(Resultados, "M3_lista")
  addWorksheet(Resultados, "M4_lista")
  addWorksheet(Resultados, "M5_lista")
  addWorksheet(Resultados, "M6_lista")
  
  # Write the data to the sheets
  writeData(Resultados, "vis", x = vis,row.names = F)
  writeData(Resultados, "M1_lista", x = M1_lista,row.names = F)
  writeData(Resultados, "M2_lista", x = M2_lista,row.names = F)
  writeData(Resultados, "M3_lista", x = M3_lista,row.names = F)
  writeData(Resultados, "M4_lista", x = M4_lista,row.names = F)
  writeData(Resultados, "M5_lista", x = M5_lista,row.names = F)
  writeData(Resultados, "M6_lista", x = M6_lista,row.names = F)
 
  #
  saveWorkbook(Resultados,overwrite =T,paste0("productos/medidas de calidad/1. Medidas de calidad Silva A.G_", index[i], ".xlsx"))
  readline(prompt="Presiona [enter] para continuar.")
  
#}



