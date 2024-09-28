#########################################################################################################
#banco_intervalo_atendimento_24
#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
#########################################################################################################
#########################################################################################################

# 1 PROCEDIMENTOS INICIAIS : LIMPAR OBJETOS

#rm(list=ls(all=TRUE))

# 2) DEFINIR DIRETÓRIO DE TRABALHO: usar Ctrl+Shift+H e escolher diretório
dir.create(file.path("~/diretorio_r/movimento_ciabh", "imagens"))
setwd(file.path("~/diretorio_r/movimento_ciabh/arq_fontes"))
#########################################################################################################
#########################################################################################################

excel_sheets("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx")


jan24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                       sheet = 1, skip = 2) 

jan24 = clean_names(jan24)


fev24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 2, skip = 2) 

fev24 = clean_names(fev24)



mar24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 3, skip = 2) 

mar24 = clean_names(mar24)


abr24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 4, skip = 2) 

abr24 = clean_names(abr24)



mai24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 5, skip = 2) 

mai24 = clean_names(mai24)


jun24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 6, skip = 2) 

jun24 = clean_names(jun24)


jul24 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2024.xlsx",  
                   sheet = 7, skip = 2) 

jul24 = clean_names(jul24)


jan24$hora_chegada = format(jan24$hora_chegada,"%H:%M:%S")
fev24$hora_chegada = format(fev24$hora_chegada,"%H:%M:%S")
mar24$hora_chegada = format(mar24$hora_chegada,"%H:%M:%S")
abr24$hora_chegada = format(abr24$hora_chegada,"%H:%M:%S")
mai24$hora_chegada = format(mai24$hora_chegada,"%H:%M:%S")
jun24$hora_chegada = format(jun24$hora_chegada,"%H:%M:%S")
jul24$hora_chegada = format(jul24$hora_chegada,"%H:%M:%S")



intervalo_atendimento_24 <- bind_rows(
  select(jan24, data_entrada, hora_chegada),
  select(fev24, data_entrada, hora_chegada),
  select(mar24, data_entrada, hora_chegada),
  select(abr24, data_entrada, hora_chegada),
  select(mai24, data_entrada, hora_chegada),
  select(jun24, data_entrada, hora_chegada),
  select(jul24, data_entrada, hora_chegada)
 
)

intervalo_atendimento_24 <- intervalo_atendimento_24[!is.na(intervalo_atendimento_24$hora_chegada),]


intervalo_atendimento_24$intervalo_01 <- ifelse(intervalo_atendimento_24$hora_chegada >= "00:00:00" & intervalo_atendimento_24$hora_chegada < "08:00:00" , "ENTRE 00:00 H e 07:59 H", "DESCONSIDERAR")
intervalo_atendimento_24$intervalo_02 <- ifelse(intervalo_atendimento_24$hora_chegada >= "08:00:00" & intervalo_atendimento_24$hora_chegada < "18:00:00" , "ENTRE 08:00 H e 17:59 H", "DESCONSIDERAR")
intervalo_atendimento_24$intervalo_03 <- ifelse(intervalo_atendimento_24$hora_chegada >= "18:00:00" & intervalo_atendimento_24$hora_chegada < "24:00:00" , "ENTRE 18:00 H e 23:59 H", "DESCONSIDERAR")


#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_intervalo_atendimento_24 =
  
  intervalo_atendimento_24 %>%
  pivot_longer(cols = starts_with("intervalo"), values_to = "PERIODO") |>
  #select(-name) %>%
  filter(!PERIODO %in% "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#############################################################################################################
#banco_intervalo_atendimento_24
#########################################################################################################

banco_intervalo_atendimento_24 =
  banco_intervalo_atendimento_24 |>
  select(PERIODO)

colnames(banco_intervalo_atendimento_24)[1]<-'banco_intervalo_atendimento_24'
#########################################################################################################
banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "NAO SABE"]<- "VNÃO SABE"
banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "INDIGENA"]<- "INDÍGENA"
#########################################################################################################
# salvando para gráfico
banco_intervalo_atendimento_24_bkp = banco_intervalo_atendimento_24

banco_intervalo_atendimento_24_bkp =
  banco_intervalo_atendimento_24_bkp %>%
  janitor::tabyl(banco_intervalo_atendimento_24) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_intervalo_atendimento_24_bkp$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24_bkp$banco_intervalo_atendimento_24 == "VNÃO SABE"]<- "NÃO SABE"
banco_intervalo_atendimento_24_bkp$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24_bkp$banco_intervalo_atendimento_24 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_intervalo_atendimento_24_bkp$PERCENTUAL2 <- str_replace (banco_intervalo_atendimento_24_bkp$percent, "%", "")
banco_intervalo_atendimento_24_bkp$PERCENTUAL2 = as.numeric(banco_intervalo_atendimento_24_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_intervalo_atendimento_24_bkp)[1]<-'banco_intervalo_atendimento_24_bkp'
colnames(banco_intervalo_atendimento_24_bkp)[2]<-'QUANTIDADE'
colnames(banco_intervalo_atendimento_24_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_intervalo_atendimento_24_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_intervalo_atendimento_24_bkp$PERCENTUAL))
banco_intervalo_atendimento_24_bkp_rmd = tail(banco_intervalo_atendimento_24_bkp,3)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "VNÃO SABE"]<- "UNÃO SABE"
#banco_intervalo_atendimento_24$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24$banco_intervalo_atendimento_24 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_intervalo_atendimento_24_TABELA =
  banco_intervalo_atendimento_24 %>%
  janitor::tabyl(banco_intervalo_atendimento_24) %>%
  arrange(banco_intervalo_atendimento_24) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_intervalo_atendimento_24_TABELA$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24_TABELA$banco_intervalo_atendimento_24 == "UNÃO SABE"]<- "NÃO SABE"
banco_intervalo_atendimento_24_TABELA$banco_intervalo_atendimento_24[banco_intervalo_atendimento_24_TABELA$banco_intervalo_atendimento_24 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_intervalo_atendimento_24_TABELA)[1]<-'PERÍODO'
colnames(banco_intervalo_atendimento_24_TABELA)[2]<-'QUANTIDADE'
colnames(banco_intervalo_atendimento_24_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_intervalo_atendimento_24 FIM
#########################################################################################################
#########################################################################################################
#banco_intervalo_atendimento_23
#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
#########################################################################################################
#########################################################################################################

# 1 PROCEDIMENTOS INICIAIS : LIMPAR OBJETOS

#rm(list=ls(all=TRUE))

# 2) DEFINIR DIRETÓRIO DE TRABALHO: usar Ctrl+Shift+H e escolher diretório
dir.create(file.path("~/diretorio_r/movimento_ciabh", "imagens"))
setwd(file.path("~/diretorio_r/movimento_ciabh/arq_fontes"))
#########################################################################################################
#########################################################################################################

excel_sheets("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx")


jan23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 1, skip = 0) 

jan23 = clean_names(jan23)


fev23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 2, skip = 0) 

fev23 = clean_names(fev23)



mar23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 3, skip = 0) 

mar23 = clean_names(mar23)


abr23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 4, skip = 0) 

abr23 = clean_names(abr23)



mai23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 5, skip = 0) 

mai23 = clean_names(mai23)


jun23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 6, skip = 0) 

jun23 = clean_names(jun23)


jul23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 7, skip = 0) 

jul23 = clean_names(jul23)


ago23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 8, skip = 0) 

ago23 = clean_names(ago23)

set23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 9, skip = 0) 

set23 = clean_names(set23)

out23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 10, skip = 0) 

out23 = clean_names(out23)

nov23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 11, skip = 0) 

nov23 = clean_names(nov23)

dez23 <-read_excel("/home/amikobh/diretorio_r/movimento_ciabh/arq_fontes/2023.xlsx",  
                   sheet = 12, skip = 0) 

dez23 = clean_names(dez23)







jan23$hora_chegada = format(jan23$hora_chegada,"%H:%M:%S")
fev23$hora_chegada = format(fev23$hora_chegada,"%H:%M:%S")
mar23$hora_chegada = format(mar23$hora_chegada,"%H:%M:%S")
abr23$hora_chegada = format(abr23$hora_chegada,"%H:%M:%S")
mai23$hora_chegada = format(mai23$hora_chegada,"%H:%M:%S")
jun23$hora_chegada = format(jun23$hora_chegada,"%H:%M:%S")
jul23$hora_chegada = format(jul23$hora_chegada,"%H:%M:%S")
ago23$hora_chegada = format(ago23$hora_chegada,"%H:%M:%S")
set23$hora_chegada = format(set23$hora_chegada,"%H:%M:%S")
out23$hora_chegada = format(out23$hora_chegada,"%H:%M:%S")
nov23$hora_chegada = format(nov23$hora_chegada,"%H:%M:%S")
dez23$hora_chegada = format(dez23$hora_chegada,"%H:%M:%S")


intervalo_atendimento_23 <- bind_rows(
  select(jan23, hora_chegada),
  select(fev23, hora_chegada),
  select(mar23, hora_chegada),
  select(abr23, hora_chegada),
  select(mai23, hora_chegada),
  select(jun23, hora_chegada),
  select(jul23, hora_chegada),
  select(ago23, hora_chegada),
  select(set23, hora_chegada),
  select(out23, hora_chegada),
  select(nov23, hora_chegada),
  select(dez23, hora_chegada)
)

intervalo_atendimento_23 <- intervalo_atendimento_23[!is.na(intervalo_atendimento_23$hora_chegada),]


intervalo_atendimento_23$intervalo_01 <- ifelse(intervalo_atendimento_23$hora_chegada >= "00:00:00" & intervalo_atendimento_23$hora_chegada < "08:00:00" , "ENTRE 00:00 H e 07:59 H", "DESCONSIDERAR")
intervalo_atendimento_23$intervalo_02 <- ifelse(intervalo_atendimento_23$hora_chegada >= "08:00:00" & intervalo_atendimento_23$hora_chegada < "18:00:00" , "ENTRE 08:00 H e 17:59 H", "DESCONSIDERAR")
intervalo_atendimento_23$intervalo_03 <- ifelse(intervalo_atendimento_23$hora_chegada >= "18:00:00" & intervalo_atendimento_23$hora_chegada < "24:00:00" , "ENTRE 18:00 H e 23:59 H", "DESCONSIDERAR")


#########################################################################################################
#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
banco_intervalo_atendimento_23 =
  
  intervalo_atendimento_23 %>%
  pivot_longer(cols = starts_with("intervalo"), values_to = "PERIODO") |>
  #select(-name) %>%
  filter(!PERIODO %in% "DESCONSIDERAR")

#########################################################################################################
#########################################################################################################
#############################################################################################################
#banco_intervalo_atendimento_23
#########################################################################################################

banco_intervalo_atendimento_23 =
  banco_intervalo_atendimento_23 |>
  select(PERIODO)

colnames(banco_intervalo_atendimento_23)[1]<-'banco_intervalo_atendimento_23'
#########################################################################################################
banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "SEM INFORMACAO"]<- "NÃO RESPONDEU"
banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "NAO SABE"]<- "VNÃO SABE"
banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "NÃO RESPONDEU"]<- "VNÃO RESPONDEU"
banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "INDIGENA"]<- "INDÍGENA"
#########################################################################################################
# salvando para gráfico
banco_intervalo_atendimento_23_bkp = banco_intervalo_atendimento_23

banco_intervalo_atendimento_23_bkp =
  banco_intervalo_atendimento_23_bkp %>%
  janitor::tabyl(banco_intervalo_atendimento_23) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
banco_intervalo_atendimento_23_bkp$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23_bkp$banco_intervalo_atendimento_23 == "VNÃO SABE"]<- "NÃO SABE"
banco_intervalo_atendimento_23_bkp$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23_bkp$banco_intervalo_atendimento_23 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#replace "%" with "" in the percentual column
banco_intervalo_atendimento_23_bkp$PERCENTUAL2 <- str_replace (banco_intervalo_atendimento_23_bkp$percent, "%", "")
banco_intervalo_atendimento_23_bkp$PERCENTUAL2 = as.numeric(banco_intervalo_atendimento_23_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(banco_intervalo_atendimento_23_bkp)[1]<-'banco_intervalo_atendimento_23_bkp'
colnames(banco_intervalo_atendimento_23_bkp)[2]<-'QUANTIDADE'
colnames(banco_intervalo_atendimento_23_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################
#########################################################################################################
#para script rmd:
banco_intervalo_atendimento_23_bkp$PERCENTUAL2 = as.numeric(gsub("%", "", banco_intervalo_atendimento_23_bkp$PERCENTUAL))
banco_intervalo_atendimento_23_bkp_rmd = tail(banco_intervalo_atendimento_23_bkp,3)
#########################################################################################################

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "VNÃO SABE"]<- "UNÃO SABE"
#banco_intervalo_atendimento_23$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23$banco_intervalo_atendimento_23 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
banco_intervalo_atendimento_23_TABELA =
  banco_intervalo_atendimento_23 %>%
  janitor::tabyl(banco_intervalo_atendimento_23) %>%
  arrange(banco_intervalo_atendimento_23) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
banco_intervalo_atendimento_23_TABELA$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23_TABELA$banco_intervalo_atendimento_23 == "UNÃO SABE"]<- "NÃO SABE"
banco_intervalo_atendimento_23_TABELA$banco_intervalo_atendimento_23[banco_intervalo_atendimento_23_TABELA$banco_intervalo_atendimento_23 == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(banco_intervalo_atendimento_23_TABELA)[1]<-'PERÍODO'
colnames(banco_intervalo_atendimento_23_TABELA)[2]<-'QUANTIDADE'
colnames(banco_intervalo_atendimento_23_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_intervalo_atendimento_23 FIM
#########################################################################################################

