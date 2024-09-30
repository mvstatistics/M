
rm(list=ls())
#
library(tidyverse)
library(magrittr)

#  1. base de Marco de UPM
index <- list.files("bases/ribd/");i=1

mmm <- list.files(paste0("bases/marco upm/", substr(index[i], 1, 4),"/" ))
  
marco_upm <- readRDS(paste0("bases/marco upm/", substr(index[i], 1, 4),"/", mmm ))
  
  marco_upm <- marco_upm %>% 
      select(id_upm, area, estrato, viv, dom) %>%
    mutate(viv = as.numeric(viv))

  muestra <- marco_upm %>%
    group_by(estrato) %>% 
    mutate(Nh=n(),
           Mh=sum(viv)) %>% 
    ungroup() %>% 
    mutate(Mhi=viv) %>% 
    select(id_upm,estrato,Nh,Mhi,Mh,area,dom)
  
  # 2. Ajuste de cobertura y no elegibilidad 
  #
  
  # Base de cobertura
  cobertura <- readRDS(file =paste0("./bases/tratadas/cobertura/cob_",index[i],".rds")) %>% 
    filter(tcomp != 0)
  
  # Base de personas
  base <- readRDS(file =paste0("./bases/tratadas/personas/per_",index[i],".rds"))
  
  if(index[i] %in% c("202407")){
    # Cálculo de las Viviendas NO EFECTIVAS (por cambio de ocupación)
    cobertura <- cobertura %>% 
      # cálculo de las viviendas visitadas y no efectivas
      mutate(noeleg = ttemp + tdeso + tcons + tdest + tnego + totras,
             rechazo = trecha + tnc + tnapa + tnequi + tninv) %>%
      # Guardar variables necesarias para el cálculo
      select(id_upm, vvisita = totviv, tcomp, trecha = rechazo, tnadie, noeleg, totper) %>%
      mutate(control = tcomp + trecha + tnadie + noeleg - vvisita)
  }
  
  
  print(index[i])
  
  print("UPM sin diferencia en el total de viviendas: ")
  
  print(table(cobertura$control,useNA = "ifany"))
  
  cobertura <- cobertura %>% 
    select(-control)
  
  # Unión de la base del marco y la base número de conglomerados de la Muestra
  wk <- left_join(cobertura, muestra, by = "id_upm") %>% 
    # Cálculo del Número de conglomerados existentes en la muestra investigada
    group_by(estrato) %>%
    mutate(nh = n()) %>% 
    # Guardar variables necesarias para el proceso de cálculo
    select(id_upm, area, estrato,
           vvisita, tcomp, trecha, tnadie, noeleg, totper,
           Nh, nh, Mhi, Mh) %>% 
    ungroup()
  
  n_distinct(wk$estrato)
  
  # 3. Cálculo de Factor de Expansión (Fexp)
  #
  wk %<>% 
    group_by(id_upm) %>% 
    mutate(# Probabilidad a primera etapa UPM
      PPE = nh/Nh,
      # Probabilidad de Segunda Etapa UPM
      PSE = vvisita/Mhi,
      # Probabilidad de inclusion de la Viviend
      PIV = PPE*PSE,
      # Factor diseño (Peso basico de la Vivienda según el diseño Muestral)
      FD = 1/(PPE*PSE),
      # Ajuste por cambio de ocupación (no elegibles)
      CO = ifelse(noeleg>0, 1, 0),
      A1 = CO*(1 - noeleg/vvisita),
      a1 = ifelse(A1==0, 1, A1)) %>%
    select(-A1) %>%
    # Factor ajustado por cambio de ocupación
    mutate(d1 = FD*a1,
           # Ajuste por nadie en casa (elegibilidad desconocida),
           NC = ifelse(tnadie>0, 1, 0),
           A1 = NC*(vvisita/tcomp),
           a2 = ifelse(A1<1, 1, A1)) %>%
    select(-A1) %>%
    # Factor ajustado por nadie en casa
    mutate(d2 = d1*a2,
           # Ajuste  rechazo (no respondientes)
           NR = ifelse(trecha>=0 & a2==1, 1, 0),
           A1 = NR*(vvisita/tcomp),
           a3 = ifelse(A1<1, 1, A1),
           # Factor ajustado por rechazo (final)
           d3 = d2*a3) %>%
    select(-A1) %>%
    # Representación de personas por conglomerado para el factor de expansión
    mutate(Rp = d3*totper) %>% 
    select(id_upm, area, estrato, 
           vvisita, tcomp, trecha, tnadie, noeleg, totper,
           Nh, nh, Mh, Mhi, PPE, PSE, PIV, FD, CO, a1, d1, NC, a2, d2, NR, a3, d3, Rp) %>% 
    ungroup()
  
  print("Número de viviendas expandidas: ")
  print(sum(wk$vvisita*wk$FD))
  print("Número de viviendas en el marco: ")
  print(sum(marco_upm$viv))
  
  
  # Factores de expansion teorico, ajustado por cobertura y no elegibilidad
  Fexp_ENEMDU <- wk %>% 
    ungroup() %>% 
    select(id_upm,
           fexp_teo = FD, 
           fexp_aju = d3)
  
  sum(Fexp_ENEMDU$fexp_teo)
  sum(Fexp_ENEMDU$fexp_aju)
  
  base <- base %>% 
    left_join(Fexp_ENEMDU, by = c("id_upm"))
  
  print("Control perdidos base wk: ")
  print(apply(is.na(wk), 2, sum))
  
  print("Población expandida fexp_teo en base personas:")
  print(sum(base$fexp_teo))
  print("Población expandida fexp_aju en base personas:")
  print(sum(base$fexp_aju))
  
  
  # Comprobación del factor de expansión (Fexp)
  print("Control población expandida en wk:")
  wk %>%
    ungroup()%>%
    summarise(pop_teo = sum(totper*FD),
              pop_aju = sum(totper*d3)) %>% 
    print()

  
  # Análisis de Descriptivos de los factores
  print(summary(base$fexp_teo))
  print(summary(base$fexp_aju))
  
  saveRDS(wk, file=paste0("./bases/tratadas/wk/wk_",index[i],".rds"))
  
  saveRDS(base, file =paste0("bases/tratadas/personas_fexp/personas_fexp",index[i],".rds"))
  
  




