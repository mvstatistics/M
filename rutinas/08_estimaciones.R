
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
  
  base_fexp = base_fexp %>% 
    mutate(one = 1) 
  
  ######
  base_fexp = base_fexp %>%
    mutate(tadec1 =ifelse(pean==1 & adec==1,100,NA))%>%
    mutate(tadec = ifelse(pean==1 & is.na(adec),0,tadec1)) %>%
    select(-tadec1)
  
  #####
  base_fexp = base_fexp %>%
    mutate(tsub1 =ifelse(pean==1 & sub==1,100,NA))%>%
    mutate(tsub = ifelse(pean==1 & is.na(sub),0,tsub1)) %>%
    select(-tsub1)
  
  
  #####
  base_fexp = base_fexp %>%
    mutate(tdesem1 =ifelse(pean==1 & desem==1,100,NA))%>%
    mutate(tdesem = ifelse(pean==1 & is.na(desem),0,tdesem1)) %>%
    select(-tdesem1)
  
  ####
  base_fexp = base_fexp %>%
    mutate(tpobre = ifelse(pobreza==0,0,
                           ifelse(pobreza==1,100,NA)),
           tepobre = ifelse(epobreza==0,0,
                            ifelse(epobreza==1,100,NA)))
  
  #####
  names(base_fexp)
  sum(base_fexp$fexp_cal_upm)
  
  
  # Estimaciones de la ENEMDU
  # Diseño de muestreo
  
  
  enemdu_cal_upm <- base_fexp %>% as_survey_design(ids = id_upm,
                                                     strata = estrato,
                                                     weights = fexp_aju,
                                                     nest = T)
  
  options(survey.lonely.psu = "certainty")
  
  
  # Indicadores partiendo desde el fexp ajustado45
  grupos <- list(enemdu_cal_upm$variables$one,
                 enemdu_cal_upm$variables$area,
                 enemdu_cal_upm$variables$dominio)
  
  for (i in 1:length(grupos)){
    tabla <- enemdu_cal_upm %>% 
      group_by(grupos[[i]]) %>%
      summarise(adec = survey_mean(tadec, vartype = c("se", "ci", "cv"), na.rm = T, deff = T),
                desem = survey_mean(tdesem, vartype = c("se", "ci", "cv"), na.rm = T, deff = T),
                sub = survey_mean(tsub, vartype = c("se", "ci", "cv"), na.rm = T, deff = T),
                pobre = survey_mean(tpobre, vartype = c("se", "ci", "cv"), na.rm = T, deff = T),
                epobre = survey_mean(tepobre, vartype = c("se", "ci", "cv"), na.rm = T, deff = T))
    if (i==1){
      resultados <- tabla
    } else {
      resultados <- rbind(resultados, tabla)
    }
  }
  
  names(resultados)[names(resultados) == 'grupos[[i]]'] <- 'grupos'
  
  
  resultados <- resultados %>% 
    mutate(grupos= as.character(seq(1, 15)))
  
  
  #creación variable ngrupos
  resultados <- mutate(resultados,ngrupos=ifelse(grupos=="1","Nacional",
                                                 ifelse(grupos=="2","Urbana",
                                                        ifelse(grupos=="3","Rural",
                                                               ifelse(grupos=="4","Quito",
                                                                      ifelse(grupos=="5","Guayaquil",
                                                                             ifelse(grupos=="6","Cuenca",
                                                                                    ifelse(grupos=="7","Machala",
                                                                                           ifelse(grupos=="8","Ambato",
                                                                                                  ifelse(grupos=="9","Resto Sierra Urbano",
                                                                                                         ifelse(grupos=="10","Resto Costa Urbano",
                                                                                                                ifelse(grupos=="11","Amazonia Urbano",
                                                                                                                       ifelse(grupos=="12","Sierra Rural",
                                                                                                                              ifelse(grupos=="13","Costa Rural",
                                                                                                                                     ifelse(grupos=="14","Amazonia Rural",
                                                                                                                                            ifelse(grupos=="15","Región Insular",NA))))))))))))))))
  
  
  resultados <-resultados %>% 
    relocate(ngrupos,.after = grupos)
  
  
    ###
  export(resultados, overwrite =T, paste0("./productos/Estimación_Empleo.xlsx"))
  
  
  resultados <-resultados %>% 
    select(-grupos)
  
  export(resultados, paste0("resultados/Estimacion_Empleo.rds"))
  



