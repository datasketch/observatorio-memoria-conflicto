library(rio)
library(tidyverse)
library(highcharter)
library(hgchmagic)
library(Hmisc)


#caso_ab <- import('data/original/Casos Acciones Bélicas.xlsx')
#caso_ab <- Filter(function(z) !all(is.na(z)), caso_ab)
caso_ab <- import('data/original2/CasosAB.xlsx')
# caso_ab$DIAH[caso_ab$DIAH == 0] <- NA
# caso_ab$DIAH[caso_ab$MESH == 0] <- NA
# caso_ab$DIAH[caso_ab$ANNOH == 0] <- NA
caso_ab <- caso_ab %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_ab <- caso_ab %>% 
            map_df(
              function(z) {
                gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
              }
            )
write_csv(caso_ab, 'data/clean/casos_acciones_belicas.csv', na = '')
#Victimas_ab <- import('data/original/Victimas Acciones Bélicas.xlsx')
#Victimas_ab <- Filter(function(z) !all(is.na(z)), Victimas_ab)
Victimas_ab <- import('data/original2/VictimasAB.xlsx')
# Victimas_ab$DIAH[Victimas_ab$DIAH == 0] <- NA
# Victimas_ab$DIAH[Victimas_ab$MESH == 0] <- NA
# Victimas_ab$DIAH[Victimas_ab$ANNOH == 0] <- NA
Victimas_ab <- Victimas_ab %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_ab <- Victimas_ab %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )
write_csv(Victimas_ab, 'data/clean/victimas_acciones_belicas.csv', na = '')
rm(list = ls())

#caso_as <- import('data/original/Casos Asesinatos Selectivos.xlsx')
#caso_as <- Filter(function(z) !all(is.na(z)), caso_as)
caso_as <- import('data/original2/CasosAS.xlsx')
caso_as <- caso_as %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_as <- caso_as %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_as, 'data/clean/casos_asesinatos_selectivos.csv', na = '')
#Victimas_as <- import('data/original/Victimas Asesinatos Selectivos.xlsx')
#Victimas_as <- Filter(function(z) !all(is.na(z)), Victimas_as)
Victimas_as <- import('data/original2/VictimasAS.xlsx')
Victimas_as[Victimas_as == ""] <- NA
Victimas_as <- Victimas_as %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_as <- Victimas_as %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_as, 'data/clean/victimas_asesinatos_selectivos.csv', na = '')

rm(list = ls())
#caso_atp <- import('data/original/Casos Ataques a Población.xlsx')
#caso_atp <- Filter(function(z) !all(is.na(z)), caso_atp)
caso_atp <- import('data/original2/CasosAP.xlsx')
caso_atp <- caso_atp %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_atp <- caso_atp %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )
write_csv(caso_atp, 'data/clean/casos_ataques_poblaciones.csv', na = '')
#Victimas_atp <- import('data/original/Victimas Ataques a Población.xlsx')
#Victimas_atp <- Filter(function(z) !all(is.na(z)), Victimas_atp)
Victimas_atp <- import('data/original2/VictimasAP.xlsx')
Victimas_atp <- Victimas_atp %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_atp <- Victimas_atp %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )
write_csv(Victimas_atp, 'data/clean/victimas_ataques_poblaciones.csv', na = '')

rm(list = ls())

#caso_att <- import('data/original/Casos Atentados Terroristas.xlsx')
#caso_att <- Filter(function(z) !all(is.na(z)), caso_att)

caso_att <- import('data/original2/CasosAT.xlsx')
caso_att <- caso_att %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_att <- caso_att %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_att, 'data/clean/casos_atentados_terroristas.csv', na = '')
#Victimas_att <- import('data/original/Victimas Atentados Terroristas.xlsx')
#Victimas_att <- Filter(function(z) !all(is.na(z)), Victimas_att)
Victimas_att <- import('data/original2/VictimasAT.xlsx')
Victimas_att <- Victimas_att %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_att <- Victimas_att %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_att, 'data/clean/victimas_atentados_terroristas.csv', na = '')
rm(list = ls())

#caso_dbc <- import('data/original/Casos Daños a Bienes Civiles.xlsx')
#caso_dbc <- Filter(function(z) !all(is.na(z)), caso_dbc)
caso_dbc <- import('data/original2/CasosDB.xlsx')
caso_dbc <- caso_dbc %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_dbc <- caso_dbc %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_dbc, 'data/clean/casos_danos_bienes.csv', na = '')
#Victimas_dbc <- import('data/original/Victimas Daño a Bienes Civiles.xlsx')
#Victimas_dbc <- Filter(function(z) !all(is.na(z)), Victimas_dbc)
Victimas_dbc <- import('data/original2/VictimasDB.xlsx')
Victimas_dbc <- Victimas_dbc %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_dbc <- Victimas_dbc %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_dbc, 'data/clean/victimas_danos_bienes.csv', na = '')
rm(list = ls())
#caso_df <- import('data/original/Casos Desaparición Forzada.xlsx')
#caso_df <- Filter(function(z) !all(is.na(z)), caso_df)
caso_df <- import('data/original2/CasosDF.xlsx')
caso_df <- caso_df %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_df <- caso_df %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_df, 'data/clean/casos_desaparicion.csv', na = '')
#Victimas_df <- import('data/original/Victimas Desaparición Forzada.xlsx')
#Victimas_df <- Filter(function(z) !all(is.na(z)), Victimas_df)
Victimas_df <- import('data/original2/VictimasDF.xlsx')
Victimas_df <- Victimas_df %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_df <- Victimas_df %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_df, 'data/clean/victimas_desaparicion.csv')
rm(list = ls())

#caso_muse <- import('data/original/Casos MAP MUSE.xlsx')
#caso_muse <- Filter(function(z) !all(is.na(z)), caso_muse)
caso_muse <- import('data/original2/CasosMI.xlsx')
caso_muse <- caso_muse %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_muse <- caso_muse %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_muse, 'data/clean/casos_map_muse.csv', na = '')
#Victimas_muse <- import('data/original/Victimas MAP MUSE.xlsx')
#Victimas_muse <- Filter(function(z) !all(is.na(z)), Victimas_muse)
Victimas_muse <- import('data/original2/VictimasMI.xlsx')
Victimas_muse <- Victimas_muse %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_muse <- Victimas_muse %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_muse, 'data/clean/victimas_map_muse.csv', na = '')
rm(list = ls())

#caso_mas <- import('data/original/Casos Masacres.xlsx')
#caso_mas <- Filter(function(z) !all(is.na(z)), caso_mas)
caso_mas <- import('data/original2/CasosMA.xlsx')
caso_mas <- caso_mas %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_mas <- caso_mas %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_mas, 'data/clean/casos_masacres.csv', na = '')
#Victimas_mas <- import('data/original/Victimas Masacres.xlsx')
#Victimas_mas <- Filter(function(z) !all(is.na(z)), Victimas_mas)
Victimas_mas <- import('data/original2/VictimasMA.xlsx')
Victimas_mas <- Victimas_mas %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_mas <- Victimas_mas %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_mas, 'data/clean/victimas_masacres.csv', na = '')

rm(list = ls())
#caso_recl <- import('data/original/Casos Reclutamiento.xlsx')
#caso_recl <- Filter(function(z) !all(is.na(z)), caso_recl)
caso_recl <- import('data/original2/CasosRU.xlsx')
caso_recl <- caso_recl %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_recl <- caso_recl %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_recl, 'data/clean/casos_reclutamiento.csv', na = '')
#Victimas_recl <- import('data/original/Victimas Reclutamiento.xlsx')
#Victimas_recl <- Filter(function(z) !all(is.na(z)), Victimas_recl)
Victimas_recl <- import('data/original2/VictimasRU.xlsx')
Victimas_recl <- Victimas_recl %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_recl <- Victimas_recl %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_recl, 'data/clean/victimas_reclutamiento.csv', na = '')


rm(list = ls())
# caso_runna <- import('data/original/Casos RUNNA.xlsx')
# caso_runna <- Filter(function(z) !all(is.na(z)), caso_runna)
# caso_runna <- caso_runna %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
# caso_runna <- caso_runna %>% 
#   map_df(
#     function(z) {
#       gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
#     }
#   )
# 
# write_csv(caso_runna, 'data/clean/casos_runna.csv', na = '')
# Victimas_runna <- import('data/original/Victimas RUNNA.xlsx')
# Victimas_runna <- Filter(function(z) !all(is.na(z)), Victimas_runna)
# Victimas_runna <- Victimas_runna %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
# Victimas_runna <- Victimas_runna %>% 
#   map_df(
#     function(z) {
#       gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
#     }
#   )
# 
# write_csv(Victimas_runna, 'data/clean/victimas_runna.csv', na = '')



#caso_sec <- import('data/original/Casos Secuestro.xlsx')
#caso_sec <- Filter(function(z) !all(is.na(z)), caso_sec)
caso_sec <- import('data/original2/CasosSE.xlsx')
caso_sec <- caso_sec %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_sec <- caso_sec %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_sec, 'data/clean/casos_secuestro.csv', na = '')
#Victimas_sec <- import('data/original/Victimas Secuestro.xlsx')
#Victimas_sec <- Filter(function(z) !all(is.na(z)), Victimas_sec)
Victimas_sec <- import('data/original2/VictimasSE.xlsx')
Victimas_sec <- Victimas_sec %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_sec <- Victimas_sec %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_sec, 'data/clean/victimas_secuestro.csv', na = '')

#caso_vs <- import('data/original/Casos Violencia Sexual.xlsx')
#caso_vs <- Filter(function(z) !all(is.na(z)), caso_vs)
caso_vs <- import('data/original2/CasoVSI.xlsx')
caso_vs <- caso_vs %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
caso_vs <- caso_vs %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(caso_vs, 'data/clean/casos_violencia_sexual.csv', na = '')
#Victimas_vs <- import('data/original/Victimas Violencia Sexual.xlsx')
#Victimas_vs <- Filter(function(z) !all(is.na(z)), Victimas_vs)
Victimas_vs <-import('data/original2/VictimasVS.xlsx')
Victimas_vs <- Victimas_vs %>% map_if(is.character, ~capitalize(tolower(.x))) %>% bind_rows()
Victimas_vs <- Victimas_vs %>% 
  map_df(
    function(z) {
      gsub('Otro ¿cuál\\?|Otra ¿cuál\\?', 'Otro', z)
    }
  )

write_csv(Victimas_vs, 'data/clean/victimas_violencia_sexual.csv', na = '')


rm(list = ls())






# dep_casos <- cab %>% select(name = DEPTO_CASO) %>% distinct() %>% drop_na()
# 
# mapV <- get_data_from_map(download_map_data("countries/co/co-all"))
# mapV$name[mapV$name == 'Bogota'] <- 'BOGOTÁ, D.C.'
# mapV$name[mapV$name == 'San Andrés y Providencia'] <- 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA' 
# mapV$name <- toupper(mapV$name)
# 
# dep_casos <- left_join(dep_casos, mapV, by = 'name')
# write_csv(dep_casos, 'data/aux/depto_code.csv')


#







files_all <- list.files('data/clean/')
files_casos <- files_all[grepl('casos', files_all)]

dic_casos <- map_df(files_casos, function(z){
                    a <- read_csv(paste0('data/clean/',z))
                    x <- names(a)
                    ctypes <- getCtypes(a)
                    data.frame(id = x, base = z, ctypes=ctypes)
                    })
dic_casos <- dic_casos %>% distinct(id, .keep_all = T)

write_csv(dic_casos, 'data/aux/dic_casosskdns.csv')
# 

files_Victimass <- files_all[grepl('victimas', files_all)]

dic_Victimass <- map_df(files_Victimass, function(z){
                       a <- read_csv(paste0('data/clean/',z))
                       x <- names(a)
                       ctypes <- getCtypes(a)
                       data.frame(id = x, base = z, ctypes)
                       })
dic_Victimass <- dic_Victimass %>% distinct(id, .keep_all = TRUE)

write_csv(dic_Victimass, 'data/aux/dic_Victimassnjkasns.csv')
#
#
sd <- read_csv('data/aux/dic_casosskdns.csv')
sd <- sd %>% distinct(id, .keep_all = TRUE)
vic <- read_csv('data/aux/dic_Victimassnjkasns.csv')
vic <- vic %>% distinct(id, .keep_all = TRUE)
sd <- bind_rows(sd, vic) %>% distinct(id, .keep_all = TRUE)
AS <- read_csv('data/aux/dic_casos2.csv') #%>% select(-ctypes)
sd <- sd %>% select(id, ctypes)
asd <- sd %>% left_join(AS)
asd$ctypes[asd$ctypes == 'Pct'] <- 'Num'
asd$label <- coalesce(asd$label, asd$id)
write_csv(asd, 'data/aux/dic_casos2.csv')

# Paleta de colores
# #87c440 (verde)
# #3082c4 (azul)
# #f5853f (naranja 1)
# #f69660 (naranja 2)
# #f9ab7b (naranja 3)
# #48b7b1 (verde azul)
# #2c82c1 (azul 2)
# #9e1a63 (morado)
# #f7931e (naranja letra)
# #662d91 (morado letra)
# #e24232 (rojo letra)
# #1b343d azul principal
# #2c82c1 azul cuadro
# #f7931e naranja cuadro
# #d9e021 amarillo cuadro
# #662d91 morado cuadro

# Fuente
# Source Sans Pro, sans-serif;
