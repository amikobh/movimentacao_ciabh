#########################################################################################################
#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#########################################################################################################

# 1 PROCEDIMENTOS INICIAIS : LIMPAR OBJETOS

#rm(list=ls(all=TRUE))

# 2) DEFINIR DIRETÓRIO DE TRABALHO: usar Ctrl+Shift+H e escolher diretório
dir.create(file.path("~/diretorio_r/estcomissariado", "imagens"))
setwd(file.path("~/diretorio_r/estcomissariado/arq_fontes"))
#########################################################################################################
#########################################################################################################
# 3 CARREGANDO O BANCO PARA TRATAMENTO NO R: observar se variaveis são iguais
#Ao salvar o banco como .csv escolher separador ":"

banco <- read.csv("comissariado_banco_atual.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8" ) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_inicial_bkp <- banco

##SALVANDO BANCO ORIGINAL
###write.csv(banco_inicial_bkp, file ="banco_inicial_bkp.csv",row.names=TRUE)
#write.xlsx(banco_inicial_bkp, file ="banco_inicial_bkp.xlsx")
#  verificando o nome das variaveis pertencentes ao objeto dados:

banco =
  banco |>
  clean_names()

#RENOMEANDO COLUNA
colnames(banco)[1]<-'DATA_SISTEMA'
colnames(banco)[2]<-'DATA_COMISSARIO'
colnames(banco)[3]<-'QUANT_AUDIENCIAS'
colnames(banco)[4]<-'TIPO_SERVICO'
colnames(banco)[5]<-'PERIODO'
colnames(banco)[6]<-'TOTAL_ADL_ATENDIDOS'
colnames(banco)[7]<-'TOTAL_TESTEMUNHA'
colnames(banco)[8]<-'QUANT_ENC_NAMSEP'
colnames(banco)[9]<-'QUANT_ENC_NEAF'
colnames(banco)[10]<-'QUANT_ENC_MASCULINO'
colnames(banco)[11]<-'QUANT_ENC_FEMININO'
colnames(banco)[12]<-'QUANT_ENC_CT'
colnames(banco)[13]<-'QUANT_ENC_PAIS'
colnames(banco)[14]<-'PRESENCA_PAIS_AUD'
colnames(banco)[15]<-'QUANT_ENC_PPCAAM'
colnames(banco)[16]<-'QUANT_ENC_OUTRAS_INST'
colnames(banco)[17]<-'QUANT_ENC_JR'
colnames(banco)[18]<-'QUANT_ENC_CEDIPRO'
colnames(banco)[19]<-'QUANT_ENC_DESEMBOLA'

#############################################################################################################
#excluir preenchimento teste:
banco = banco |>
  filter(!nome_completo_do_comissario_responsavel_pelo_preenchimento %in% "ELERSONTESTE")
#########################################################################################################
# Para converter um dataframe em tibble:

banco <- as_tibble(banco)

#########################################################################################################
#Permutar NA por 0
banco[is.na(banco)] <- 0
#############################################################################################################
#########################################################################################################
#########################################################################################################
# 4 TRATAMENTOS INICIAIS:

#EVITAR ERRO DE PREENCHIMENTO DE DATA USANDA A DO FORMULARIO GOOGLE

banco = banco %>%
  mutate(DATA_SISTEMA2 = substr(banco$DATA_SISTEMA, start = 1, stop = 10))


banco$DATA_COMISSARIO = str_replace(banco$DATA_COMISSARIO, "00", "20")

banco =
  banco |>
  mutate(DATA_COMISSARIO2 = str_replace_all(banco$DATA_COMISSARIO, "-", "/"))


#########################################################################################################
banco = banco %>%
  mutate(DIA_SEMANA = weekdays(as.Date(banco$DATA_SISTEMA2)))

banco$DATA_SISTEMA2 = as.Date(banco$DATA_SISTEMA2)
#########################################################################################################
#EVITAR ERRO DE PREENCHIMENTO

banco$PERIODO2 = ifelse(banco$PERIODO != "PLANTÃO" & banco$DIA_SEMANA == "domingo" | banco$DIA_SEMANA == "sábado",
                        "PLANTÃO", banco$PERIODO)

banco$PERIODO2 = ifelse(banco$PERIODO2 == "" & banco$TIPO_SERVICO == "PLANTÃO",
                        "PLANTÃO", banco$PERIODO2)

banco$PERIODO2 = ifelse(banco$PERIODO2 == "",
                        banco$TIPO_SERVICO, banco$PERIODO2)
#########################################################################################################
#df <- subset(df, DATA_ATO >= "2020-01-01" & DATA_ATO <= "2020-07-01")
banco = banco %>%
  filter(DATA_SISTEMA2 >= "2024-01-01" & DATA_SISTEMA2 <= "2024-06-30")

#write.csv(banco, file ="banco_recorte_data.csv",row.names=FALSE)

#entradas_PERIODO_sem_nomes_repetidos <- banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]

#export(banco, "banco1.csv")

##exemplo % com dois dígitos
#round(escolaridade01, 2)
#########################################################################################################
#EVITAR ERRO DE PREENCHIMENTO

banco$QUANT_ENC_JR = ifelse(banco$TOTAL_ADL_ATENDIDOS < banco$QUANT_ENC_JR,
                            banco$TOTAL_ADL_ATENDIDOS/2+1, banco$QUANT_ENC_JR)

banco$QUANT_AUDIENCIAS = ifelse(banco$TOTAL_ADL_ATENDIDOS < banco$QUANT_AUDIENCIAS,
                            banco$TOTAL_ADL_ATENDIDOS, banco$QUANT_AUDIENCIAS)
#########################################################################################################
#########################################################################################################
#TRATAMENTO TOTAL_ATENDIMENTOS_COM
#########################################################################################################

TOTAL_ATENDIMENTOS_COM = data.frame(rbind
                                    (cbind("Atendimento audiências", sum(banco$QUANT_AUDIENCIAS)),
                                      cbind("Atendimento adolescentes", sum(banco$TOTAL_ADL_ATENDIDOS)),
                                      cbind("Atendimento aos Pais/Responsáveis", sum(banco$PRESENCA_PAIS_AUD)),
                                      cbind("Atendimento testemunhas", sum(banco$TOTAL_TESTEMUNHA)),
                                      cbind("Encaminhamento NAMSEP", sum(banco$QUANT_ENC_NAMSEP)),
                                      cbind("Encaminhamento NEAF", sum(banco$QUANT_ENC_NEAF)),
                                      cbind("Encaminhamento aos CEIP's masculinos", sum(banco$QUANT_ENC_MASCULINO)),
                                      cbind("Encaminhamento ao CEIP feminino", sum(banco$QUANT_ENC_FEMININO)),
                                      cbind("Encaminhamento ao Conselho Tutelar", sum(banco$QUANT_ENC_CT)),
                                      cbind("Encaminhamento aos Pais/Responsáveis", sum(banco$QUANT_ENC_PAIS)),
                                      cbind("Encaminhamento ao PPCAAM", sum(banco$QUANT_ENC_PPCAAM)),
                                      cbind("Encaminhamento Justiça Restaurativa", sum(banco$QUANT_ENC_JR)),
                                      cbind("Encaminhamento CEDIPRO", sum(banco$QUANT_ENC_CEDIPRO)),
                                      cbind("Encaminhamento DESEMBOLA", sum(banco$QUANT_ENC_DESEMBOLA)),
                                      cbind("Encaminhamento outras instituições", sum(banco$QUANT_ENC_OUTRAS_INST))))


# renomeando variaveis
colnames(TOTAL_ATENDIMENTOS_COM)[1]<-'TIPO'
colnames(TOTAL_ATENDIMENTOS_COM)[2]<-'QUANTIDADE'

TOTAL_ATENDIMENTOS_COM = as_tibble(TOTAL_ATENDIMENTOS_COM)

TOTAL_ATENDIMENTOS_COM$QUANTIDADE <- as.numeric(TOTAL_ATENDIMENTOS_COM$QUANTIDADE)
#########################################################################################################

#usando a funcao criada:

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

TOTAL_ATENDIMENTOS_COM <- TOTAL_ATENDIMENTOS_COM %>%
  #arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#TOTAL_ATENDIMENTOS_COM$PERCENTUAL <- paste(TOTAL_ATENDIMENTOS_COM$PERCENTUAL, "%", sep=" ")

TOTAL_ATENDIMENTOS_COM

#salvando para utilizacao graficos
TOTAL_ATENDIMENTOS_COM_bkp = TOTAL_ATENDIMENTOS_COM

#########################################################################################################
#########################################################################################################

#script para o bookdown

TOTAL_ATENDIMENTOS_COM_rmark = TOTAL_ATENDIMENTOS_COM

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
TOTAL_ATENDIMENTOS_COM_rmark = TOTAL_ATENDIMENTOS_COM_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(TOTAL_ATENDIMENTOS_COM_rmark$QUANTIDADE)

#para escolher linhas e posicoes
TOTAL_ATENDIMENTOS_COM_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TOTAL_ATENDIMENTOS_COM_bkp = TOTAL_ATENDIMENTOS_COM #salvando para proximo modelo de tabela
#########################################################################################################
TOTAL_ATENDIMENTOS_COM<- rbind(TOTAL_ATENDIMENTOS_COM,
                               data.frame(TIPO = "TOTAL",
                                          QUANTIDADE = sum(TOTAL_ATENDIMENTOS_COM$QUANTIDADE),
                                          PERCENTUAL = sum(TOTAL_ATENDIMENTOS_COM$PERCENTUAL),
                                          stringsAsFactors = FALSE))

TOTAL_ATENDIMENTOS_COM
#########################################################################################################
#colnames(TOTAL_ATENDIMENTOS_COM) = c("TIPO","QUANTIDADE","%")
#########################################################################################################
TOTAL_ATENDIMENTOS_COM$PERCENTUAL = paste(TOTAL_ATENDIMENTOS_COM$PERCENTUAL,"%")
########################################################################################################
#########################################################################################################
#TRATAMENTO TOTAL_ATENDIMENTOS_COM FIM
#########################################################################################################
df_scr_banco = banco
#########################################################################################################
#TRATAMENTO QUANT_AUDIENCIAS
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_AUDIENCIAS = df_scr_banco %>%
  select(PERIODO2, QUANT_AUDIENCIAS)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_AUDIENCIAS$QUANT_AUDIENCIAS)

QUANT_AUDIENCIAS <- ddply(QUANT_AUDIENCIAS,
                                       c("PERIODO2"),
                                       summarise,
                                       QUANTIDADE = sum(QUANT_AUDIENCIAS))


QUANT_AUDIENCIAS = arrange(QUANT_AUDIENCIAS,desc(QUANTIDADE))

QUANT_AUDIENCIAS_bkp = QUANT_AUDIENCIAS


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_AUDIENCIAS

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_AUDIENCIAS <- QUANT_AUDIENCIAS %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_AUDIENCIAS$PERCENTUAL <- paste(QUANT_AUDIENCIAS$PERCENTUAL, "%", sep=" ")

QUANT_AUDIENCIAS

#salvando para utilizacao graficos
QUANT_AUDIENCIAS_bkp = QUANT_AUDIENCIAS

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_AUDIENCIAS_rmark = QUANT_AUDIENCIAS

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_AUDIENCIAS_rmark = QUANT_AUDIENCIAS_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_AUDIENCIAS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_AUDIENCIAS_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_AUDIENCIAS_bkp = QUANT_AUDIENCIAS #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_AUDIENCIAS<- rbind(QUANT_AUDIENCIAS,
                                      data.frame(PERIODO2 = "TOTAL",
                                                 QUANTIDADE = sum(QUANT_AUDIENCIAS$QUANTIDADE),
                                                 PERCENTUAL = sum(QUANT_AUDIENCIAS$PERCENTUAL),
                                                 stringsAsFactors = FALSE))

QUANT_AUDIENCIAS
#########################################################################################################
colnames(QUANT_AUDIENCIAS) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_AUDIENCIAS$PERCENTUAL = paste(QUANT_AUDIENCIAS$PERCENTUAL,"%")
#########################################################################################################
#QUANT_AUDIENCIAS= QUANT_AUDIENCIAS_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_AUDIENCIAS FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO TOTAL_ADL_ATENDIDOS
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

TOTAL_ADL_ATENDIDOS = df_scr_banco %>%
  select(PERIODO2, TOTAL_ADL_ATENDIDOS)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(TOTAL_ADL_ATENDIDOS$TOTAL_ADL_ATENDIDOS)

TOTAL_ADL_ATENDIDOS <- ddply(TOTAL_ADL_ATENDIDOS,
                             c("PERIODO2"),
                             summarise,
                             QUANTIDADE = sum(TOTAL_ADL_ATENDIDOS))


TOTAL_ADL_ATENDIDOS = arrange(TOTAL_ADL_ATENDIDOS,desc(QUANTIDADE))

TOTAL_ADL_ATENDIDOS_bkp = TOTAL_ADL_ATENDIDOS


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
TOTAL_ADL_ATENDIDOS

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

TOTAL_ADL_ATENDIDOS <- TOTAL_ADL_ATENDIDOS %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#TOTAL_ADL_ATENDIDOS$PERCENTUAL <- paste(TOTAL_ADL_ATENDIDOS$PERCENTUAL, "%", sep=" ")

TOTAL_ADL_ATENDIDOS

#salvando para utilizacao graficos
TOTAL_ADL_ATENDIDOS_bkp = TOTAL_ADL_ATENDIDOS

#########################################################################################################
#########################################################################################################

#script para o bookdown

TOTAL_ADL_ATENDIDOS_rmark = TOTAL_ADL_ATENDIDOS

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
TOTAL_ADL_ATENDIDOS_rmark = TOTAL_ADL_ATENDIDOS_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(TOTAL_ADL_ATENDIDOS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
TOTAL_ADL_ATENDIDOS_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TOTAL_ADL_ATENDIDOS_bkp = TOTAL_ADL_ATENDIDOS #salvando para proximo modelo de tabela
#########################################################################################################
TOTAL_ADL_ATENDIDOS<- rbind(TOTAL_ADL_ATENDIDOS,
                            data.frame(PERIODO2 = "TOTAL",
                                       QUANTIDADE = sum(TOTAL_ADL_ATENDIDOS$QUANTIDADE),
                                       PERCENTUAL = sum(TOTAL_ADL_ATENDIDOS$PERCENTUAL),
                                       stringsAsFactors = FALSE))

TOTAL_ADL_ATENDIDOS
#########################################################################################################
colnames(TOTAL_ADL_ATENDIDOS) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
TOTAL_ADL_ATENDIDOS$PERCENTUAL = paste(TOTAL_ADL_ATENDIDOS$PERCENTUAL,"%")
#########################################################################################################
#TOTAL_ADL_ATENDIDOS= TOTAL_ADL_ATENDIDOS_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO TOTAL_ADL_ATENDIDOS FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO TOTAL_TESTEMUNHA
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

TOTAL_TESTEMUNHA = df_scr_banco %>%
  select(PERIODO2, TOTAL_TESTEMUNHA)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(TOTAL_TESTEMUNHA$TOTAL_TESTEMUNHA)

TOTAL_TESTEMUNHA <- ddply(TOTAL_TESTEMUNHA,
                          c("PERIODO2"),
                          summarise,
                          QUANTIDADE = sum(TOTAL_TESTEMUNHA))


TOTAL_TESTEMUNHA = arrange(TOTAL_TESTEMUNHA,desc(QUANTIDADE))

TOTAL_TESTEMUNHA_bkp = TOTAL_TESTEMUNHA


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
TOTAL_TESTEMUNHA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

TOTAL_TESTEMUNHA <- TOTAL_TESTEMUNHA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#TOTAL_TESTEMUNHA$PERCENTUAL <- paste(TOTAL_TESTEMUNHA$PERCENTUAL, "%", sep=" ")

TOTAL_TESTEMUNHA

#salvando para utilizacao graficos
TOTAL_TESTEMUNHA_bkp = TOTAL_TESTEMUNHA

#########################################################################################################
#########################################################################################################

#script para o bookdown

TOTAL_TESTEMUNHA_rmark = TOTAL_TESTEMUNHA

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
TOTAL_TESTEMUNHA_rmark = TOTAL_TESTEMUNHA_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(TOTAL_TESTEMUNHA_rmark$QUANTIDADE)

#para escolher linhas e posicoes
TOTAL_TESTEMUNHA_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#TOTAL_TESTEMUNHA_bkp = TOTAL_TESTEMUNHA #salvando para proximo modelo de tabela
#########################################################################################################
TOTAL_TESTEMUNHA<- rbind(TOTAL_TESTEMUNHA,
                         data.frame(PERIODO2 = "TOTAL",
                                    QUANTIDADE = sum(TOTAL_TESTEMUNHA$QUANTIDADE),
                                    PERCENTUAL = sum(TOTAL_TESTEMUNHA$PERCENTUAL),
                                    stringsAsFactors = FALSE))

TOTAL_TESTEMUNHA
#########################################################################################################
colnames(TOTAL_TESTEMUNHA) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
TOTAL_TESTEMUNHA$PERCENTUAL = paste(TOTAL_TESTEMUNHA$PERCENTUAL,"%")
#########################################################################################################
#TOTAL_TESTEMUNHA= TOTAL_TESTEMUNHA_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO TOTAL_TESTEMUNHA FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_NAMSEP
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_NAMSEP = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_NAMSEP)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_NAMSEP$QUANT_ENC_NAMSEP)

QUANT_ENC_NAMSEP <- ddply(QUANT_ENC_NAMSEP,
                          c("PERIODO2"),
                          summarise,
                          QUANTIDADE = sum(QUANT_ENC_NAMSEP))


QUANT_ENC_NAMSEP = arrange(QUANT_ENC_NAMSEP,desc(QUANTIDADE))

QUANT_ENC_NAMSEP_bkp = QUANT_ENC_NAMSEP


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_NAMSEP

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_NAMSEP <- QUANT_ENC_NAMSEP %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_NAMSEP$PERCENTUAL <- paste(QUANT_ENC_NAMSEP$PERCENTUAL, "%", sep=" ")

QUANT_ENC_NAMSEP

#salvando para utilizacao graficos
QUANT_ENC_NAMSEP_bkp = QUANT_ENC_NAMSEP

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_NAMSEP_rmark = QUANT_ENC_NAMSEP

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_NAMSEP_rmark = QUANT_ENC_NAMSEP_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_NAMSEP_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_NAMSEP_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_NAMSEP_bkp = QUANT_ENC_NAMSEP #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_NAMSEP<- rbind(QUANT_ENC_NAMSEP,
                         data.frame(PERIODO2 = "TOTAL",
                                    QUANTIDADE = sum(QUANT_ENC_NAMSEP$QUANTIDADE),
                                    PERCENTUAL = sum(QUANT_ENC_NAMSEP$PERCENTUAL),
                                    stringsAsFactors = FALSE))

QUANT_ENC_NAMSEP
#########################################################################################################
colnames(QUANT_ENC_NAMSEP) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_NAMSEP$PERCENTUAL = paste(QUANT_ENC_NAMSEP$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_NAMSEP= QUANT_ENC_NAMSEP_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_NAMSEP FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_NEAF
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_NEAF = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_NEAF)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_NEAF$QUANT_ENC_NEAF)

QUANT_ENC_NEAF <- ddply(QUANT_ENC_NEAF,
                        c("PERIODO2"),
                        summarise,
                        QUANTIDADE = sum(QUANT_ENC_NEAF))


QUANT_ENC_NEAF = arrange(QUANT_ENC_NEAF,desc(QUANTIDADE))

QUANT_ENC_NEAF_bkp = QUANT_ENC_NEAF


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_NEAF

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_NEAF <- QUANT_ENC_NEAF %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_NEAF$PERCENTUAL <- paste(QUANT_ENC_NEAF$PERCENTUAL, "%", sep=" ")

QUANT_ENC_NEAF

#salvando para utilizacao graficos
QUANT_ENC_NEAF_bkp = QUANT_ENC_NEAF

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_NEAF_rmark = QUANT_ENC_NEAF

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_NEAF_rmark = QUANT_ENC_NEAF_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_NEAF_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_NEAF_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_NEAF_bkp = QUANT_ENC_NEAF #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_NEAF<- rbind(QUANT_ENC_NEAF,
                       data.frame(PERIODO2 = "TOTAL",
                                  QUANTIDADE = sum(QUANT_ENC_NEAF$QUANTIDADE),
                                  PERCENTUAL = sum(QUANT_ENC_NEAF$PERCENTUAL),
                                  stringsAsFactors = FALSE))

QUANT_ENC_NEAF
#########################################################################################################
colnames(QUANT_ENC_NEAF) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_NEAF$PERCENTUAL = paste(QUANT_ENC_NEAF$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_NEAF= QUANT_ENC_NEAF_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_NEAF FIM
#########################################################################################################


#########################################################################################################
#TRATAMENTO QUANT_ENC_MASCULINO
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_MASCULINO = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_MASCULINO)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_MASCULINO$QUANT_ENC_MASCULINO)

QUANT_ENC_MASCULINO <- ddply(QUANT_ENC_MASCULINO,
                             c("PERIODO2"),
                             summarise,
                             QUANTIDADE = sum(QUANT_ENC_MASCULINO))


QUANT_ENC_MASCULINO = arrange(QUANT_ENC_MASCULINO,desc(QUANTIDADE))

QUANT_ENC_MASCULINO_bkp = QUANT_ENC_MASCULINO


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_MASCULINO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_MASCULINO <- QUANT_ENC_MASCULINO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_MASCULINO$PERCENTUAL <- paste(QUANT_ENC_MASCULINO$PERCENTUAL, "%", sep=" ")

QUANT_ENC_MASCULINO

#salvando para utilizacao graficos
QUANT_ENC_MASCULINO_bkp = QUANT_ENC_MASCULINO

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_MASCULINO_rmark = QUANT_ENC_MASCULINO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_MASCULINO_rmark = QUANT_ENC_MASCULINO_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_MASCULINO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_MASCULINO_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_MASCULINO_bkp = QUANT_ENC_MASCULINO #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_MASCULINO<- rbind(QUANT_ENC_MASCULINO,
                            data.frame(PERIODO2 = "TOTAL",
                                       QUANTIDADE = sum(QUANT_ENC_MASCULINO$QUANTIDADE),
                                       PERCENTUAL = sum(QUANT_ENC_MASCULINO$PERCENTUAL),
                                       stringsAsFactors = FALSE))

QUANT_ENC_MASCULINO
#########################################################################################################
colnames(QUANT_ENC_MASCULINO) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_MASCULINO$PERCENTUAL = paste(QUANT_ENC_MASCULINO$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_MASCULINO= QUANT_ENC_MASCULINO_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_MASCULINO FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_FEMININO
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_FEMININO = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_FEMININO)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_FEMININO$QUANT_ENC_FEMININO)

QUANT_ENC_FEMININO <- ddply(QUANT_ENC_FEMININO,
                            c("PERIODO2"),
                            summarise,
                            QUANTIDADE = sum(QUANT_ENC_FEMININO))


QUANT_ENC_FEMININO = arrange(QUANT_ENC_FEMININO,desc(QUANTIDADE))

QUANT_ENC_FEMININO_bkp = QUANT_ENC_FEMININO


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_FEMININO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_FEMININO <- QUANT_ENC_FEMININO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_FEMININO$PERCENTUAL <- paste(QUANT_ENC_FEMININO$PERCENTUAL, "%", sep=" ")

QUANT_ENC_FEMININO

#salvando para utilizacao graficos
QUANT_ENC_FEMININO_bkp = QUANT_ENC_FEMININO

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_FEMININO_rmark = QUANT_ENC_FEMININO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_FEMININO_rmark = QUANT_ENC_FEMININO_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_FEMININO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_FEMININO_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_FEMININO_bkp = QUANT_ENC_FEMININO #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_FEMININO<- rbind(QUANT_ENC_FEMININO,
                           data.frame(PERIODO2 = "TOTAL",
                                      QUANTIDADE = sum(QUANT_ENC_FEMININO$QUANTIDADE),
                                      PERCENTUAL = sum(QUANT_ENC_FEMININO$PERCENTUAL),
                                      stringsAsFactors = FALSE))

QUANT_ENC_FEMININO
#########################################################################################################
colnames(QUANT_ENC_FEMININO) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_FEMININO$PERCENTUAL = paste(QUANT_ENC_FEMININO$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_FEMININO= QUANT_ENC_FEMININO_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_FEMININO FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_CT
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_CT = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_CT)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_CT$QUANT_ENC_CT)

QUANT_ENC_CT <- ddply(QUANT_ENC_CT,
                      c("PERIODO2"),
                      summarise,
                      QUANTIDADE = sum(QUANT_ENC_CT))


QUANT_ENC_CT = arrange(QUANT_ENC_CT,desc(QUANTIDADE))

QUANT_ENC_CT_bkp = QUANT_ENC_CT


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_CT

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_CT <- QUANT_ENC_CT %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_CT$PERCENTUAL <- paste(QUANT_ENC_CT$PERCENTUAL, "%", sep=" ")

QUANT_ENC_CT

#salvando para utilizacao graficos
QUANT_ENC_CT_bkp = QUANT_ENC_CT

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_CT_rmark = QUANT_ENC_CT

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_CT_rmark = QUANT_ENC_CT_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_CT_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_CT_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_CT_bkp = QUANT_ENC_CT #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_CT<- rbind(QUANT_ENC_CT,
                     data.frame(PERIODO2 = "TOTAL",
                                QUANTIDADE = sum(QUANT_ENC_CT$QUANTIDADE),
                                PERCENTUAL = sum(QUANT_ENC_CT$PERCENTUAL),
                                stringsAsFactors = FALSE))

QUANT_ENC_CT
#########################################################################################################
colnames(QUANT_ENC_CT) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_CT$PERCENTUAL = paste(QUANT_ENC_CT$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_CT= QUANT_ENC_CT_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_CT FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO QUANT_ENC_PAIS
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_PAIS = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_PAIS)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_PAIS$QUANT_ENC_PAIS)

QUANT_ENC_PAIS <- ddply(QUANT_ENC_PAIS,
                        c("PERIODO2"),
                        summarise,
                        QUANTIDADE = sum(QUANT_ENC_PAIS))


QUANT_ENC_PAIS = arrange(QUANT_ENC_PAIS,desc(QUANTIDADE))

QUANT_ENC_PAIS_bkp = QUANT_ENC_PAIS


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_PAIS

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_PAIS <- QUANT_ENC_PAIS %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_PAIS$PERCENTUAL <- paste(QUANT_ENC_PAIS$PERCENTUAL, "%", sep=" ")

QUANT_ENC_PAIS

#salvando para utilizacao graficos
QUANT_ENC_PAIS_bkp = QUANT_ENC_PAIS

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_PAIS_rmark = QUANT_ENC_PAIS

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_PAIS_rmark = QUANT_ENC_PAIS_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_PAIS_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_PAIS_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_PAIS_bkp = QUANT_ENC_PAIS #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_PAIS<- rbind(QUANT_ENC_PAIS,
                       data.frame(PERIODO2 = "TOTAL",
                                  QUANTIDADE = sum(QUANT_ENC_PAIS$QUANTIDADE),
                                  PERCENTUAL = sum(QUANT_ENC_PAIS$PERCENTUAL),
                                  stringsAsFactors = FALSE))

QUANT_ENC_PAIS
#########################################################################################################
colnames(QUANT_ENC_PAIS) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_PAIS$PERCENTUAL = paste(QUANT_ENC_PAIS$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_PAIS= QUANT_ENC_PAIS_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_PAIS FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO PRESENCA_PAIS_AUD
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

PRESENCA_PAIS_AUD = df_scr_banco %>%
  select(PERIODO2, PRESENCA_PAIS_AUD)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(PRESENCA_PAIS_AUD$PRESENCA_PAIS_AUD)

PRESENCA_PAIS_AUD <- ddply(PRESENCA_PAIS_AUD,
                           c("PERIODO2"),
                           summarise,
                           QUANTIDADE = sum(PRESENCA_PAIS_AUD))


PRESENCA_PAIS_AUD = arrange(PRESENCA_PAIS_AUD,desc(QUANTIDADE))

PRESENCA_PAIS_AUD_bkp = PRESENCA_PAIS_AUD


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
PRESENCA_PAIS_AUD

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

PRESENCA_PAIS_AUD <- PRESENCA_PAIS_AUD %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#PRESENCA_PAIS_AUD$PERCENTUAL <- paste(PRESENCA_PAIS_AUD$PERCENTUAL, "%", sep=" ")

PRESENCA_PAIS_AUD

#salvando para utilizacao graficos
PRESENCA_PAIS_AUD_bkp = PRESENCA_PAIS_AUD

#########################################################################################################
#########################################################################################################

#script para o bookdown

PRESENCA_PAIS_AUD_rmark = PRESENCA_PAIS_AUD

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
PRESENCA_PAIS_AUD_rmark = PRESENCA_PAIS_AUD_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(PRESENCA_PAIS_AUD_rmark$QUANTIDADE)

#para escolher linhas e posicoes
PRESENCA_PAIS_AUD_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#PRESENCA_PAIS_AUD_bkp = PRESENCA_PAIS_AUD #salvando para proximo modelo de tabela
#########################################################################################################
PRESENCA_PAIS_AUD<- rbind(PRESENCA_PAIS_AUD,
                          data.frame(PERIODO2 = "TOTAL",
                                     QUANTIDADE = sum(PRESENCA_PAIS_AUD$QUANTIDADE),
                                     PERCENTUAL = sum(PRESENCA_PAIS_AUD$PERCENTUAL),
                                     stringsAsFactors = FALSE))

PRESENCA_PAIS_AUD
#########################################################################################################
colnames(PRESENCA_PAIS_AUD) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
PRESENCA_PAIS_AUD$PERCENTUAL = paste(PRESENCA_PAIS_AUD$PERCENTUAL,"%")
#########################################################################################################
#PRESENCA_PAIS_AUD= PRESENCA_PAIS_AUD_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO PRESENCA_PAIS_AUD FIM
#########################################################################################################


#########################################################################################################
#TRATAMENTO QUANT_ENC_PPCAAM
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_PPCAAM = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_PPCAAM)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_PPCAAM$QUANT_ENC_PPCAAM)

QUANT_ENC_PPCAAM <- ddply(QUANT_ENC_PPCAAM,
                          c("PERIODO2"),
                          summarise,
                          QUANTIDADE = sum(QUANT_ENC_PPCAAM))


QUANT_ENC_PPCAAM = arrange(QUANT_ENC_PPCAAM,desc(QUANTIDADE))

QUANT_ENC_PPCAAM_bkp = QUANT_ENC_PPCAAM


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_PPCAAM

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_PPCAAM <- QUANT_ENC_PPCAAM %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_PPCAAM$PERCENTUAL <- paste(QUANT_ENC_PPCAAM$PERCENTUAL, "%", sep=" ")

QUANT_ENC_PPCAAM

#salvando para utilizacao graficos
QUANT_ENC_PPCAAM_bkp = QUANT_ENC_PPCAAM

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_PPCAAM_rmark = QUANT_ENC_PPCAAM

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_PPCAAM_rmark = QUANT_ENC_PPCAAM_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_PPCAAM_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_PPCAAM_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_PPCAAM_bkp = QUANT_ENC_PPCAAM #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_PPCAAM<- rbind(QUANT_ENC_PPCAAM,
                         data.frame(PERIODO2 = "TOTAL",
                                    QUANTIDADE = sum(QUANT_ENC_PPCAAM$QUANTIDADE),
                                    PERCENTUAL = sum(QUANT_ENC_PPCAAM$PERCENTUAL),
                                    stringsAsFactors = FALSE))

QUANT_ENC_PPCAAM
#########################################################################################################
colnames(QUANT_ENC_PPCAAM) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_PPCAAM$PERCENTUAL = paste(QUANT_ENC_PPCAAM$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_PPCAAM= QUANT_ENC_PPCAAM_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_PPCAAM FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_OUTRAS_INST
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_OUTRAS_INST = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_OUTRAS_INST)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_OUTRAS_INST$QUANT_ENC_OUTRAS_INST)

QUANT_ENC_OUTRAS_INST <- ddply(QUANT_ENC_OUTRAS_INST,
                               c("PERIODO2"),
                               summarise,
                               QUANTIDADE = sum(QUANT_ENC_OUTRAS_INST))


QUANT_ENC_OUTRAS_INST = arrange(QUANT_ENC_OUTRAS_INST,desc(QUANTIDADE))

QUANT_ENC_OUTRAS_INST_bkp = QUANT_ENC_OUTRAS_INST


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_OUTRAS_INST

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_OUTRAS_INST <- QUANT_ENC_OUTRAS_INST %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_OUTRAS_INST$PERCENTUAL <- paste(QUANT_ENC_OUTRAS_INST$PERCENTUAL, "%", sep=" ")

QUANT_ENC_OUTRAS_INST

#salvando para utilizacao graficos
QUANT_ENC_OUTRAS_INST_bkp = QUANT_ENC_OUTRAS_INST

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_OUTRAS_INST_rmark = QUANT_ENC_OUTRAS_INST

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_OUTRAS_INST_rmark = QUANT_ENC_OUTRAS_INST_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_OUTRAS_INST_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_OUTRAS_INST_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_OUTRAS_INST_bkp = QUANT_ENC_OUTRAS_INST #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_OUTRAS_INST<- rbind(QUANT_ENC_OUTRAS_INST,
                              data.frame(PERIODO2 = "TOTAL",
                                         QUANTIDADE = sum(QUANT_ENC_OUTRAS_INST$QUANTIDADE),
                                         PERCENTUAL = sum(QUANT_ENC_OUTRAS_INST$PERCENTUAL),
                                         stringsAsFactors = FALSE))

QUANT_ENC_OUTRAS_INST
#########################################################################################################
colnames(QUANT_ENC_OUTRAS_INST) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_OUTRAS_INST$PERCENTUAL = paste(QUANT_ENC_OUTRAS_INST$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_OUTRAS_INST= QUANT_ENC_OUTRAS_INST_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_OUTRAS_INST FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO QUANT_ENC_JR
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_JR = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_JR)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_JR$QUANT_ENC_JR)

QUANT_ENC_JR <- ddply(QUANT_ENC_JR,
                      c("PERIODO2"),
                      summarise,
                      QUANTIDADE = sum(QUANT_ENC_JR))


QUANT_ENC_JR = arrange(QUANT_ENC_JR,desc(QUANTIDADE))

QUANT_ENC_JR_bkp = QUANT_ENC_JR


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_JR

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_JR <- QUANT_ENC_JR %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_JR$PERCENTUAL <- paste(QUANT_ENC_JR$PERCENTUAL, "%", sep=" ")

QUANT_ENC_JR

#salvando para utilizacao graficos
QUANT_ENC_JR_bkp = QUANT_ENC_JR

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_JR_rmark = QUANT_ENC_JR

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_JR_rmark = QUANT_ENC_JR_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_JR_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_JR_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_JR_bkp = QUANT_ENC_JR #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_JR<- rbind(QUANT_ENC_JR,
                     data.frame(PERIODO2 = "TOTAL",
                                QUANTIDADE = sum(QUANT_ENC_JR$QUANTIDADE),
                                PERCENTUAL = sum(QUANT_ENC_JR$PERCENTUAL),
                                stringsAsFactors = FALSE))

QUANT_ENC_JR
#########################################################################################################
colnames(QUANT_ENC_JR) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_JR$PERCENTUAL = paste(QUANT_ENC_JR$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_JR= QUANT_ENC_JR_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_JR FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_CEDIPRO
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_CEDIPRO = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_CEDIPRO)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_CEDIPRO$QUANT_ENC_CEDIPRO)

QUANT_ENC_CEDIPRO <- ddply(QUANT_ENC_CEDIPRO,
                           c("PERIODO2"),
                           summarise,
                           QUANTIDADE = sum(QUANT_ENC_CEDIPRO))


QUANT_ENC_CEDIPRO = arrange(QUANT_ENC_CEDIPRO,desc(QUANTIDADE))

QUANT_ENC_CEDIPRO_bkp = QUANT_ENC_CEDIPRO


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_CEDIPRO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_CEDIPRO <- QUANT_ENC_CEDIPRO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_CEDIPRO$PERCENTUAL <- paste(QUANT_ENC_CEDIPRO$PERCENTUAL, "%", sep=" ")

QUANT_ENC_CEDIPRO

#salvando para utilizacao graficos
QUANT_ENC_CEDIPRO_bkp = QUANT_ENC_CEDIPRO

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_CEDIPRO_rmark = QUANT_ENC_CEDIPRO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_CEDIPRO_rmark = QUANT_ENC_CEDIPRO_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_CEDIPRO_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_CEDIPRO_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_CEDIPRO_bkp = QUANT_ENC_CEDIPRO #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_CEDIPRO<- rbind(QUANT_ENC_CEDIPRO,
                          data.frame(PERIODO2 = "TOTAL",
                                     QUANTIDADE = sum(QUANT_ENC_CEDIPRO$QUANTIDADE),
                                     PERCENTUAL = sum(QUANT_ENC_CEDIPRO$PERCENTUAL),
                                     stringsAsFactors = FALSE))

QUANT_ENC_CEDIPRO
#########################################################################################################
colnames(QUANT_ENC_CEDIPRO) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_CEDIPRO$PERCENTUAL = paste(QUANT_ENC_CEDIPRO$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_CEDIPRO= QUANT_ENC_CEDIPRO_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_CEDIPRO FIM
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_DESEMBOLA
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:

library(dplyr)

QUANT_ENC_DESEMBOLA = df_scr_banco %>%
  select(PERIODO2, QUANT_ENC_DESEMBOLA)


#########################################################################################################
#########################################################################################################
##JUNTANDO AS LINHAS
library(plyr)
sum(QUANT_ENC_DESEMBOLA$QUANT_ENC_DESEMBOLA)

QUANT_ENC_DESEMBOLA <- ddply(QUANT_ENC_DESEMBOLA,
                             c("PERIODO2"),
                             summarise,
                             QUANTIDADE = sum(QUANT_ENC_DESEMBOLA))


QUANT_ENC_DESEMBOLA = arrange(QUANT_ENC_DESEMBOLA,desc(QUANTIDADE))

QUANT_ENC_DESEMBOLA_bkp = QUANT_ENC_DESEMBOLA


#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
QUANT_ENC_DESEMBOLA

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

QUANT_ENC_DESEMBOLA <- QUANT_ENC_DESEMBOLA %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

#QUANT_ENC_DESEMBOLA$PERCENTUAL <- paste(QUANT_ENC_DESEMBOLA$PERCENTUAL, "%", sep=" ")

QUANT_ENC_DESEMBOLA

#salvando para utilizacao graficos
QUANT_ENC_DESEMBOLA_bkp = QUANT_ENC_DESEMBOLA

#########################################################################################################
#########################################################################################################

#script para o bookdown

QUANT_ENC_DESEMBOLA_rmark = QUANT_ENC_DESEMBOLA

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
QUANT_ENC_DESEMBOLA_rmark = QUANT_ENC_DESEMBOLA_rmark %>%
  top_n(3, QUANTIDADE) %>% arrange(desc(QUANTIDADE))

#somando
sum(QUANT_ENC_DESEMBOLA_rmark$QUANTIDADE)

#para escolher linhas e posicoes
QUANT_ENC_DESEMBOLA_rmark[1,3]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#QUANT_ENC_DESEMBOLA_bkp = QUANT_ENC_DESEMBOLA #salvando para proximo modelo de tabela
#########################################################################################################
QUANT_ENC_DESEMBOLA<- rbind(QUANT_ENC_DESEMBOLA,
                            data.frame(PERIODO2 = "TOTAL",
                                       QUANTIDADE = sum(QUANT_ENC_DESEMBOLA$QUANTIDADE),
                                       PERCENTUAL = sum(QUANT_ENC_DESEMBOLA$PERCENTUAL),
                                       stringsAsFactors = FALSE))

QUANT_ENC_DESEMBOLA
#########################################################################################################
colnames(QUANT_ENC_DESEMBOLA) = c("PERIODO","QUANTIDADE", "PERCENTUAL")
#########################################################################################################
QUANT_ENC_DESEMBOLA$PERCENTUAL = paste(QUANT_ENC_DESEMBOLA$PERCENTUAL,"%")
#########################################################################################################
#QUANT_ENC_DESEMBOLA= QUANT_ENC_DESEMBOLA_bkp
#########################################################################################################
#########################################################################################################

#########################################################################################################
#TRATAMENTO QUANT_ENC_DESEMBOLA FIM
#########################################################################################################












