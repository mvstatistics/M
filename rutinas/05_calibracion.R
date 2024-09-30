    #
    rm(list = ls())
    #
    library(stringr)
    library(sampling)
    library(srvyr)
    library(tidyverse)
    #
    # Lectura de la base de proyecciones
    
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
      
      # Agregado de las variables para la calibración por UPM
      
      cal_upm <- base %>%
        select(id_upm, id_calib, estrato, fexp_teo, fexp_aju) %>% 
        group_by(id_upm) %>% 
        mutate(totper = n()) %>% 
        group_by(id_upm, estrato, fexp_teo, fexp_aju, id_calib) %>% ## en caso de haber recorte cambiar el fexp_aju por el recortado
        summarise(totper = mean(totper),
                  n = n()) %>% 
        spread(id_calib, n) %>% 
        replace(., is.na(.), 0) %>% 
        ungroup()
      
      print("Verificación de valores perdidos - cal_upm")
      print(colSums(is.na(cal_upm)))
      
      # Calibración con metodologia propuesta por CEPAL (hogar integrado)
      
      apoyo_cal <- base %>%
        select(id_upm, panelm, vivienda, hogar, p01,
               id_calib, estrato, fexp_aju) %>%
        mutate(n = 1) %>%
        spread(id_calib, n) %>% 
        replace(., is.na(.), 0) %>% 
        ungroup()
      
      aux = base %>%
        select(id_upm, panelm, vivienda, hogar, 
               id_calib, estrato, fexp_aju) %>% # se utiliza el factor ajustado por cobertura
        group_by(id_upm, panelm, vivienda, hogar) %>% 
        mutate(totper = n()) %>% 
        group_by(id_upm, panelm, vivienda, hogar, estrato, fexp_aju, id_calib) %>% 
        summarise(totper = mean(totper),
                  n = n()) %>% 
        mutate(prom = n / totper) %>%
        select(-n) %>%
        spread(id_calib, prom) %>% 
        replace(., is.na(.), 0) %>% 
        ungroup() %>%
        select(-c(estrato, fexp_aju, totper))
      
      cal_hog_int = apoyo_cal %>%
        select(id_upm, panelm, vivienda, hogar, p01, estrato, fexp_aju) %>%
        left_join(aux, by = c("id_upm", "panelm", "vivienda", "hogar"))
      
      print("Verificación de valores perdidos - cal_hog_int")
      print(colSums(is.na(cal_hog_int)))
      
      # totales estimados por Horvitz-Thompson por dominio y/o area y/o sexo
      tp <- base %>% 
        group_by(id_calib) %>% 
        summarise(d = sum(fexp_aju))
    
      # poblaciones objetivo para la calibracion
      
      pob <- readRDS(paste0("proyecciones/", index[i], "/yk.rds"))
      
      print(sum(pob$ykn))
      
      pop <- pob %>%
        select( dominio, area, p02, gedad, t=ykn) %>% 
        mutate(id_calib = paste0(dominio, "_", area, "_", p02, "_", gedad)) %>% 
        select(id_calib, t)
      
      # comprobaciones: ver tp y t
      vis <- tp %>% 
        full_join(pop, by="id_calib") %>% 
        mutate(dif = d-t) %>% 
        arrange(id_calib)%>%
        mutate(cotas = t/d)
      print(vis)
      #
      # pesos de calibracion - hogar integrado
      #
      print(dim(as.matrix(cal_hog_int[,8:dim(cal_hog_int)[2]])));     print(length(vis$d));     print(length(vis$t))
      
      calibracion_hog_int <- cal_hog_int  %>% 
        mutate(g_hog_int = calib(Xs = as.matrix(.[,8:dim(.)[2]]),
                                 d = fexp_aju,
                                 total = vis$t,
                                 method ="raking"),
               fexp_cal_hi = fexp_aju*g_hog_int) %>% 
        select(id_upm, panelm, vivienda, hogar, p01, g_hog_int, fexp_cal_hi)
      
      print("Descriptivos de pesos de calibración y fexp_cal_hi")
      print(summary(calibracion_hog_int$g_hog_int))
      print(summary(calibracion_hog_int$fexp_cal_hi))
      
      #
      # pesos de calibracion - UPM
      #
      print(dim(as.matrix(cal_upm[,6:dim(cal_upm)[2]])));     print(length(vis$d));     print(length(vis$t))
      
      
      calibracion_upm <- cal_upm  %>%
        mutate(g_upm = calib(Xs = as.matrix(.[,6:dim(.)[2]]),
                             d = fexp_aju,
                             total = vis$t,
                             method ="raking"),
               fexp_cal_upm = fexp_aju*g_upm) %>% 
        select(id_upm, g_upm, fexp_cal_upm,)
      
      print("Descriptivos de pesos de calibración y fexp_cal_upm")
      
      print(summary(calibracion_upm$g_upm))
      print(summary(calibracion_upm$fexp_cal_upm))
      
      # emparejar factores en bases 
      
      base_fexp <- base %>% 
        full_join(calibracion_hog_int,
                  by = c("id_upm", "panelm", "vivienda", "hogar", "p01")) %>% 
        full_join(calibracion_upm,
                  by = "id_upm")
      
      print("Comprobación dimensiones")
      print(dim(base)[1]==dim(base_fexp)[1])
      
      print("Totales poblacionales estimados")
      print(sum(base_fexp$fexp_teo))
      print(sum(base_fexp$fexp_aju))
      print(sum(base_fexp$fexp_cal_hi))
      print(sum(base_fexp$fexp_cal_upm))
      print(sum(vis$t))
      
      # Verificacion de las poblaciones
      comp = base_fexp %>%
        group_by(id_calib) %>%
        summarise(pob_comp_hog_int = sum(fexp_cal_hi),
                  pob_comp_upm = sum(fexp_cal_upm))
      
      comp = comp %>% 
        full_join(select(vis, id_calib, t), 
                  by = "id_calib") %>%
        mutate(dif_comp_hog_int = pob_comp_hog_int - t,
               dif_comp_upm = pob_comp_upm - t)
      
      print(paste0("Diferencia máxima sobre grupos de calibración hogar: ",
                   print(max(comp$dif_comp_hog_int))))
      
      print(paste0("Diferencia máxima sobre grupos de calibración upm: ",
                   print(max(comp$dif_comp_upm))))
      
      # Preparacion de factores
      Fexp_ENEMDU = base_fexp %>%
        select(id_upm, panelm, vivienda, hogar, p01, area, 
               prov, rnatura, dominio, estrato, edad, sexo, id_calib,
               fexp_teo, fexp_aju, fexp_cal_hi, fexp_cal_upm)
      
      # Guardar las bases
      
      saveRDS(base_fexp, paste0("bases/tratadas/personas_fexp/personas_fexp_cal", index[i], ".rds"))
      saveRDS(Fexp_ENEMDU, paste0("bases/tratadas/personas_fexp/fexp_originales", index[i], ".rds"))
      saveRDS(vis, paste0("bases/tratadas/vis/vis_", index[i], ".rds"))     
  
    
    
    
    
