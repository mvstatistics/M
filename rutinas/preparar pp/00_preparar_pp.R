#
rm(list=ls())
#
library(tidyverse)
library(openxlsx)
#
index <- list.files("bases/ribd/")
#
for(i in 1:length(index)){
   
    
    ap <- list.files(paste0("proyecciones/", index[i]),pattern = "xlsx")
    pp20 <- read.xlsx(paste0("proyecciones/", index[i], "/", ap),
                      sheet = 1,
                      startRow = 5,
                      rowNames = F,
                      colNames = F,
                      rows=5:17,
                      cols = 1:6)
    
    colnames(pp20) = c("dominio", "ndominio", "menor_15_h",
                       "menor_15_m", "mayor_15_h", "mayor_15_m")
    pp20 <- pp20 %>%
        mutate(dominio =str_pad(dominio,2,"left",pad ="0"),
               area = ifelse(dominio =="01"| dominio =="02"| dominio =="03" |
                                 dominio =="04"| dominio =="05"| dominio =="06"|
                                 dominio =="07"| dominio =="08","1",
                             ifelse(dominio =="09"| dominio =="10"| dominio =="11","2","1")))%>%
        mutate(area = ifelse(dominio =="13","2",area))%>%
        group_by(area) %>%
        summarise(menor_15_h = sum(menor_15_h),
                  menor_15_m = sum(menor_15_m),
                  mayor_15_h = sum(mayor_15_h),
                  mayor_15_m = sum(mayor_15_m)) %>%
        gather(conjunto, ykn, -area) %>%
        mutate(gedad=ifelse(substr(conjunto, 1, 5)=="menor", 1, 2),
               p02=ifelse(substr(conjunto, 10, 10)=="h", 1, 2),
               dominio ="00") %>% 
        select(dominio, area, p02, gedad,ykn)%>% 
        ungroup()
    
    sum(pp20$ykn)

    saveRDS(pp20, paste0("proyecciones/", index[i], "/yk.rds"))
}

sum(pp20$ykn)





# # 


