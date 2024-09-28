#########################################################################################################
#pdf(file="TABELA_002_incidencia_comparada_alternativa.pdf", width = 5, height = 7, title = "INCIDENCIA COMPARADA")
setwd(file.path("~/diretorio_r/movimento_ciabh/img_nao_apagar/"))

TABELA <- read.csv("tabela.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#TABELA GT
#TABELA GT

#########################################################################################################
dir.create(file.path("~/diretorio_r/movimento_ciabh", "imagens"))
#########################################################################################################
#Atos infracionais
#########################################################################################################
#TABELA GT

#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/movimento_ciabh/imagens"))


banco_intervalo_atendimento_23_TABELA %>%
  gt() %>%
  
  tab_header(
    title = md((str_c(TABELA[1,],": Horário de entrada dos adolescentes no CIABH, ", format(Sys.Date()-365*1, "%Y")))),
    subtitle = "Período do dia") %>%
  
  tab_source_note("FONTE: SUASE/CIABH") %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_intervalo_atendimento_23_TABELA))) %>%
  
  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%
  
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%
  
  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%
  
  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%
  
  # fmt_number(
  #columns = 3) %>%
  
  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))
  
  gtsave(
    "TABELA[1,].png", expand = 10)
#########################################################################################################
#TABELA GT

#TABELA GT
#SALVANDO PNG:
setwd(file.path("~/diretorio_r/movimento_ciabh/imagens"))


banco_intervalo_atendimento_24_TABELA %>%
  gt() %>%
  
  tab_header(
    title = md((str_c(TABELA[2,],": Horário de entrada dos adolescentes no CIABH, ", "01/01/2024 a 15/07/24"))),
    subtitle = "Período do dia") %>%
  
  tab_source_note("FONTE: SUASE/CIABH") %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = "small")),
    locations = cells_body(rows = nrow(banco_intervalo_atendimento_24_TABELA))) %>%
  
  #tab_style(
  # style = cell_text(weight = "bold", align = "center" ),
  # locations = cells_column_labels(columns = c(2:3)))%>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(1:3)))%>%
  
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(2:3)))%>%
  
  #  tab_style(
  # style = cell_text(align = "center"),
  # locations = cells_body(
  # columns = c(2))) %>%
  
  tab_options(
    data_row.padding = px(1),
    table.font.size = 12,
    #heading.title.font.size = 20,
    heading.align = "center",
    #heading.title.font.size = "small",
    heading.subtitle.font.size = "small",) %>%
  
  # fmt_number(
  #columns = 3) %>%
  
  #cols_align(align = "center",
  #          columns = 1:2) %>%
  #summary_rows(fns = list(TOTAL = "sum"),
  #  columns = c(2, 3))
  
  gtsave(
    "TABELA[2,].png", expand = 10)
#########################################################################################################
#################################################################################################################################################################################################

#########################################################################################################
#########################################################################################################
#pdf(file="TABELA_002_incidencia_comparada_alternativa.pdf", width = 5, height = 7, title = "INCIDENCIA COMPARADA")
setwd(file.path("~/diretorio_r/movimento_ciabh/img_nao_apagar/"))

GRAFICO <- read.csv("grafico.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#TABELA GT
#TABELA GT

#########################################################################################################
dir.create(file.path("~/diretorio_r/movimento_ciabh", "imagens"))
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/movimento_ciabh/imagens"))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_intervalo_atendimento_23_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_incidencia_bkp_alternativo.pdf", title = "grafico_banco_incidencia_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/movimento_ciabh/imagens"))
#salvar png
library(forcats)

banco_intervalo_atendimento_23_bkp =
  banco_intervalo_atendimento_23_bkp |>
  mutate(banco_intervalo_atendimento_23_bkp = fct_reorder(banco_intervalo_atendimento_23_bkp, QUANTIDADE))

ggplot(banco_intervalo_atendimento_23_bkp, aes(x = banco_intervalo_atendimento_23_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[1,],": Horário de entrada dos adolescentes no CIABH, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por Atos Infracionais",
       caption = "FONTE: SUASE/CIABH",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 1285))
ggsave("GRAFICO[1,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_intervalo_atendimento_24_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_incidencia_bkp_alternativo.pdf", title = "grafico_banco_incidencia_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/movimento_ciabh/imagens"))
#salvar png
library(forcats)

banco_intervalo_atendimento_24_bkp =
  banco_intervalo_atendimento_24_bkp |>
  mutate(banco_intervalo_atendimento_24_bkp = fct_reorder(banco_intervalo_atendimento_24_bkp, QUANTIDADE))

ggplot(banco_intervalo_atendimento_24_bkp, aes(x = banco_intervalo_atendimento_24_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[2,],": Horário de entrada dos adolescentes no CIABH, ", "01/01/2024 a 15/07/24")),
       #subtitle = "Adolescentes por Atos Infracionais",
       caption = "FONTE: SUASE/CIABH",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 530))
ggsave("GRAFICO[2,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################