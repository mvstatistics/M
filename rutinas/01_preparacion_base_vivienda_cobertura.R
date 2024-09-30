
#==============================================================================#.
# Instituto Nacional de Estadística y Censos (INEC)							   
#==============================================================================#.
# Fecha de elaboración: 14 de abril - 2022	 							   
# Fecha última modificación: 25 de mayo - 2023	   
#==============================================================================#.
# TÍTULO DE LA SINTAXIS: PROGRAMACIÓN CONFIDENCIAL
# CÁLCULO DEL FACTORES DE EXPANSION ()

# gc()

memory.profile()

 { rm(list=ls())



library(foreign)
library(stringr)
library(fastDummies)
library(tidyverse)
library(openxlsx)
library(haven)
library(rio)}
#==============================================================================#.
# 1. Base Cobertura de la Enemdu
#==============================================================================#.

index <- list.files("bases/ribd/")
i=1

  #  base de Marco de UPM
  mmm <- list.files(paste0("bases/marco upm/", substr(index[i], 1, 4),"/" ))
  marco_upm <- readRDS(paste0("bases/marco upm/", substr(index[i], 1, 4),"/", mmm ))
  
  marco_upm <- marco_upm %>% 
      select(id_upm, area, estrato)
  
  base_viv <- read.spss(file = paste0("bases/ribd/", index[i], "/Viviendas_Dies.sav"),
                      use.value.labels = F,
                      to.data.frame = T) %>%  mutate(resumen=as.numeric(resumen))
  
  
  base_viv <- data.frame(base_viv)
  
  base_per <- read.spss(file = paste0("bases/ribd/", index[i], "/Personas_Confidencial.sav"),
                       use.value.labels = F,
                       to.data.frame = T) %>% 
    rename_all(tolower)%>%
    select(-area)
  
    base_per <- data.frame(base_per)
  
  if(index[i] %in% c("202407")){
    cobertura = base_viv %>%
      rename_all(tolower) %>%
      mutate(ciudad = str_pad(str_trim(ciudad, side = "both"), 6, side = "left", pad ="0"),
             id_upm = str_pad(str_trim(id_conglomerado, "both"), 12, side = "left", pad = "0")) %>% 
      group_by(id_upm) %>% 
      mutate(numpers=sum(numpers, na.rm=T)) %>% 
      ungroup() %>% 
      # crear base a nivel de upm
      filter(hogar<=1) %>% 
      mutate(resumen = factor(resumen,
                              levels = 1:13,
                              labels = c("tcomp", "trecha", "tnadie", "ttemp",
                                         "tdeso", "tcons", "tdest", "tnego",
                                         "totras","tnc","tnapa","tnequi",
                                         "tninv"))) %>%  
      dummy_columns(select_columns = "resumen") %>% 
      group_by(id_upm) %>% 
      summarise(ciudad = first(ciudad),
                tcomp = sum(resumen_tcomp, na.rm=T),
                trecha = sum(resumen_trecha, na.rm=T),
                tnadie = sum(resumen_tnadie, na.rm=T),
                ttemp = sum(resumen_ttemp, na.rm=T),
                tdeso = sum(resumen_tdeso, na.rm=T),
                tcons = sum(resumen_tcons, na.rm=T),
                tdest = sum(resumen_tdest, na.rm=T),
                tnego = sum(resumen_tnego, na.rm=T),
                totras = sum(resumen_totras, na.rm=T),# 9
                tnc=sum(resumen_tnc),
                tnapa=sum(resumen_tnapa),
                tnequi=sum(resumen_tnequi),
                tninv=sum(resumen_tninv),
                totviv =n(),
                numpers = mean(numpers, na.rm=T))
  }
    
  base_per = base_per %>%
      mutate(ciudad = str_pad(str_trim(ciudad, side = "both"), 6, side = "left", pad ="0"),
             id_upm = str_pad(str_trim(id_conglomerado, "both"), 12, side = "left", pad = "0"))
    
  # emparejamiento de la varible area y estrato 
  cobertura <- cobertura %>% 
    left_join(marco_upm , by = "id_upm")
  
  #
  # Creación de variables auxiliares en la base de personas
  #
  base_per <- base_per %>%
    left_join(marco_upm, by="id_upm") %>% 
    mutate(prov=ifelse(ciudad != "080850",substr(id_upm,1,2),"23"), 
             # creacion variable rnatura
      rnatura = ifelse(prov=="01" | prov=="02" | prov=="03" |
                         prov=="04" | prov=="05" | prov=="06"|
                         prov=="10" | prov=="11"| prov=="17" |
                         prov=="18" | prov=="23", "1",
                       ifelse(prov=="07" |prov=="08" | prov=="09" | prov=="12" |
                                prov=="13" | prov=="24"| prov=="90", "2",
                              ifelse(prov=="14" | prov=="15" | prov=="16" |
                                       prov=="19" | prov=="21" | prov=="22", "3",
                                     ifelse(prov=="20", "4", NA))))) %>% 
    # creación variable ndominio
    mutate(dominio = ifelse(ciudad=="170150" & area==1,"01",
                            ifelse(ciudad=="090150" & area==1,"02",
                                   ifelse(ciudad=="010150" & area==1,"03",
                                          ifelse(ciudad=="070150" & area==1,"04",
                                                 ifelse(ciudad=="180150" & area==1,"05",
                                                        ifelse(rnatura=="1" & !(ciudad %in% c("010150", "170150", "180150")) & area==1,"06",
                                                               ifelse(rnatura=="2" & !(ciudad %in% c("070150", "090150")) & area==1,"07",
                                                                      ifelse(rnatura=="3" & area==1,"08",
                                                                             ifelse(rnatura=="1" & area==2,"09",
                                                                                    ifelse(rnatura=="2" & area==2,"10",
                                                                                           ifelse(rnatura=="3" & area==2,"11",
                                                                                                  ifelse(rnatura=="4","12", NA)))))))))))))
  
  print(index[i])
  print(apply(is.na(cobertura), 2, sum))
  print(summary(cobertura$tcomp))
  print(summary(cobertura$totviv))
  
  print(table(base_per$rnatura, useNA = "ifany"))
  print(table(base_per$dominio, useNA = "ifany"))
  
  #print(table(base_per$dominio, base_per$prov, useNA = "ifany"))
  print(table(base_per$dominio, base_per$area, useNA = "ifany"))
  
  
  cobertura <- cobertura %>% 
    mutate(totper=numpers) %>% 
    arrange(id_upm)
  
  
  aux <- base_per %>% 
    group_by(id_upm) %>% 
    summarise(totper_bbd_per = n()) %>% 
    left_join(select( cobertura, id_upm, totper_cob = totper), by="id_upm")
  
  upm <- aux$id_upm
  
  apoyo <-filter(cobertura , !id_upm  %in%  upm) 
  write.xlsx(select( apoyo, id_upm, area, estrato,tcomp, totras,totper),overwrite =T,
             "Productos/UPM_SIN_INFORMACIÓN.xlsx")
  
  a = sum(aux$totper_bbd_per != aux$totper_cob)
  print(paste0("diferencias entre número de personas de base de datos de personas y base de cobertura: ",a))
  
  
  if(a >0){
    write.xlsx(select( aux, id_upm, totper_bbd_per, totper_cob),
               "Productos/diferencia_totper_cob_bddper.xlsx")


  }
  saveRDS(base_per, file =paste0("./bases/tratadas/personas/per_",index[i],".rds"))
  saveRDS(cobertura, file =paste0("./bases/tratadas/cobertura/cob_",index[i],".rds"))




