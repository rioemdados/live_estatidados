# Instalação dos Pacotes
# install.packages("ipeadatar")
# install.packages("sidrar")
# install.packages("remotes")
# remotes::install_github("rfsaldanha/microdatasus")

# Carregamento 
library(ipeadatar)
library(sidrar)
library(microdatasus)
library(geobr)

# alguns amigos de sempre
library(ggplot2)
library(dplyr)


# Microdatasus ------------------------------------------------------------

# Obtenção dos dados de nascimentos em 2019
# sinasc_data <- fetch_datasus(year_start = 2019,
#                              year_end = 2019,
#                              month_start,
#                              month_end,
#                              uf = "all",
#                              information_system = "SINASC"
#                              vars = NULL)

# Processamento
# sinasc_proc <- process_sinasc(sinasc_data)


# Importante pois a obtenção dos dados pode demorar um pouquinho e depende da
# internet
load("sinasc_proc.RData") 

sinasc_proc <- janitor::clean_names(sinasc_proc)


## Local de nascimento
table(sinasc_proc$locnasc) %>% prop.table()*100 

## Duração da gestação
table(sinasc_proc$gestacao) %>% 
  prop.table()*100 

sinasc_proc %>% 
  mutate(gestacao = forcats::fct_relevel(gestacao, "Menos de 22 semanas")) %>% 
  filter(!is.na(gestacao)) %>% 
  ggplot(aes(x = gestacao)) +
  geom_bar() +
  labs(x = "Duração da gestação", y = "Frequência") +
  coord_flip() +
  theme_bw()

## Idade da mãe
ylab <- c(0, 50, 100, 150, 200, 250)
sinasc_proc %>% 
  ggplot(aes(x = idademae)) +
  geom_histogram(fill = "orange", colour = "black") +
  theme_bw() +
  scale_y_continuous(labels = paste0(ylab, c("", rep(" Mil", (length(ylab)-1)))),
                     breaks = 10^3 * ylab
  ) + 
  scale_x_continuous(breaks = 10*1:6) + 
  labs(x = "Idade da mãe", y = "Frequência") +
  theme(axis.text.y.left = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 11))


# idade media da mae por anos de escolaridade
sinasc_proc <- sinasc_proc %>% 
  mutate(escmae = forcats::fct_relevel(escmae, c("Nenhum", "1 a 3 anos", 
                                                 "4 a 7 anos", "8 a 11 anos", 
                                                 "12 anos ou mais"))) 

sinasc_proc %>% 
  filter(!is.na(escmae)) %>% 
  group_by(escmae) %>% 
  summarise(idade_media = mean(idademae, na.rm = T),
            mediafilvivo = mean(qtdfilvivo, na.rm = T))


sinasc_proc %>% 
  filter(!is.na(escmae), !is.na(qtdfilvivo)) %>%
  group_by(escmae) %>% 
  summarise(mediafilvivo = mean(qtdfilvivo),
            mediagestant = mean(qtdgestant, na.rm = T))


# FILHOS
sinasc_proc %>% 
  group_by(mun_res_uf) %>% 
  summarise(m_filho = mean(qtdfilvivo, na.rm = TRUE)) -> filhos_nasc_vivos

states <- read_state(year=2019)

filhos_nasc_vivos$mun_res_uf <- sort(states$name_state)

states <- dplyr::left_join(states, filhos_nasc_vivos, by = c("name_state" = "mun_res_uf"))

(ggplot() +
  geom_sf(data=states, aes(fill=m_filho), size=.15) +
  theme_void() +
  scale_fill_distiller(palette = "Oranges", 
                       direction = 1, 
                       name="Média de filhos vivos por estado em 2019") +
  theme(legend.position = "bottom",
        legend.key.width = unit(3.5, "cm"),
        legend.text  = element_text(size=18),
        legend.title = element_text(size=18)) + 
  guides(fill = guide_colourbar(ticks.colour = "black",
                                frame.colour = "black",
                                title.position="top",
                                title.hjust = 0.5,
                                frame.linewidth = 2)) -> g)





## SIM

# # Obtenção dos dados (dados do SIM - declaração de óbitos)
# 
# dados_brutos <- fetch_datasus(year_start =  2019, year_end = 2019, 
#                               information_system = "SIM-DO")
# 
# # Processamento dos dados
# sim_proc <- process_sim(dados_brutos)


load("simproc.RData")

sim_proc <- janitor::clean_names(sim_proc)

colnames(sim_proc)

#table(sim_proc$circobito, sim_proc$racacor)

sim_proc %>% 
  filter(!is.na(circobito),
         !is.na(racacor)) %>% 
ggplot() +
  aes(x = racacor, fill = circobito) +
  geom_bar(position = "fill") +
  theme_bw()
  

# Ipeadatar ---------------------------------------------------------------

# Séries disponíveis
series <- search_series(terms = NULL, 
                         fields = c('name'), 
                         language = c("en", "br"))


# fields podem ser "code", "name", "theme", "source", "freq", 
# "lastupdate" e "status".

unique(series$theme)

# Suponha interece no IPCA mensal
pesquisa <- search_series(terms = "IPCA", # Termo de pesquisa
                          fields = c('name'), # Campo pelo qual queremos pesquisar
                          language = c("en", "br")) # Idioma

# Após identificar que o código da série desejada é PRECOS12_IPCA12
# podemos usar a função ipeadata para obter os dados
ipca <- ipeadata("PRECOS12_IPCA12") 


# Gráfico
ggplot(ipca, aes(x = date, y = value)) +
  geom_line(size = 1.2, color = "darkorange") +
  labs(x = "", y = "IPCA - Geral - Índice (dez. 1993 = 100)") +
  scale_x_date(
    date_breaks = "2 years", date_labels = "%m/%Y",
    limits = c(min(ipca$date) - 360, max(ipca$date) + 90),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 12, face = "bold")
  )

# Suponha interece no IPCA mensal
pesquisa <- search_series(terms = "Salário mínimo real", # Termo de pesquisa
                          fields = c('name'), # Campo pelo qual queremos pesquisar
                          language = c("en", "br")) # Idioma


salmin <- ipeadata("GAC12_SALMINRE12")

ggplot(salmin, aes(x = date, y = value)) +
  geom_line(size = 1.06, color = "darkorange") +
  labs(x = "", y = "Salário Mínimo Real") +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%m/%Y",
    limits = c(min(salmin$date) - 360, max(salmin$date) + 300),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 13, face = "bold")
  )




## Taxa de câmbio
pesquisa <- search_series(terms = "Taxa de câmbio",
                          fields = c('name'), 
                          language = c("en", "br"))


taxa_cambio <- ipeadata("GM366_ERV366")

ggplot(taxa_cambio, aes(x = date, y = value)) +
  geom_line(size = 1.06, color = "darkorange") +
  labs(x = "", y = "Taxa de câmbio - R$/US$ - comercial - venda") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%d/%m/%Y",
    limits = c(min(taxa_cambio$date) - 360, max(taxa_cambio$date) + 300),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 12, face = "bold")
  )


## Exemplo com uma série regional
states <- geobr::read_state(year = 2019)

pesquisa_homic <- search_series(terms = "HOMIC", 
                                fields = c('code'), 
                                language = c("en", "br")) 


#homicidios <- ipeadata("HOMIC")

load("homicidios.RData")

homicidios %>% 
  filter(uname == "States",
         date == max(date)) %>% 
  left_join(states, by = c("tcode" = "code_state")) -> homicidios

ggplot() +
  geom_sf(data = homicidios, aes(fill=value, geometry = geom), 
          color = NA, size = .15)+
  scale_fill_distiller(palette = "YlOrRd", 
                       direction = 1, 
                       name="Número de homicídios por estado em 2019") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(3.5, "cm"),
        legend.text  = element_text(size=18),
        legend.title = element_text(size=18)) + 
  guides(fill = guide_colourbar(ticks.colour = "black",
                                frame.colour = "black",
                                title.position="top",
                                title.hjust = 0.5,
                                frame.linewidth = 2))


### SÉRIES MACROECONOMICAS (Nível Brasil)

# - Diárias
# Código | Nome
# JPM366_EMBI366 | EMBI + Risco-Brasil
# GM366_ERV366 | Taxa de câmbio - R$ / US$ - comercial - venda - média
# BM366_TJOVER366 | Taxa de juros - Selic - fixada pelo Copom
# GM366_ERC366 | Taxa de câmbio - R$ / US$ - comercial - compra - média
# GM366_TJOVER366 | Taxa de juros - Over / Selic
# GM366_IBVSP366 | Índice de ações - Ibovespa - fechamento
#
# - Mensais
# Código | Nome
# GAC12_SALMINRE12 | Salário mínimo real
# PRECOS12_IPCA12 | IPCA - geral - Índice (dez. 1993 = 100)
# IGP12_IGPM12 | IGP-M - geral - Índice (ago. 1994 = 100)
# GAC12_TCERXTINPC12 | Taxa de câmbio - efetiva real - INPC - exportações - Índice (média 2010 = 100)
# FUNCEX12_XPT12 Exportações - preços - Índice (média 2018 = 100)
# FUNCEX12_MDPT12 | Importações - preços - Índice (média 2018 = 100)
# IGP12_IGPMG12 | IGP-M
# 
# ### SERIES REGIONAIS (Multiniveis)
# 
# Código | Nome
# POPTOT | População  residente -  total
# POPRU | População  residente - rural
# POPUR | População  residente - urbana
# EXPORTACAO | Exportações Brasileiras
# HOMIC | Número de homicídios
# TOTRB | Valor Total dos Rendimentos recebidos
# PIB | PIB Municipal a preços constantes
# PIBAGE | PIB Estadual - agropecuária - valor adicionado - preços básicos


# Sidrar ------------------------------------------------------------------

# Taxa de desocupação e subutilização da força de trabalho


pesquisa <- search_sidra("pessoas desocupadas")

pesquisa[28]

infos <- info_sidra(6468)

infos$table

infos$period  #Periodicidade: trimestral (note que são 4 observações por ano)
#201201 significa 1° trimestre de 2012, e 202102 é o 2° trimestre de 2021.


infos$variable[,2] # Descrições das variáveis disponíveis
infos$variable[,1] # Códigos das variáveis disponíveis
infos$variable[1,] # Variável de interesse

infos$classific_category #Nenhuma classificação disponível (exemplo: sexo, faixa etária, etc)

infos$geo #Níveis de desagregação disponíveis


tx_desocupacao_brasil <- get_sidra(x = 6468,
                                   variable = 4099, 
                                   period = "201201-202104",
                                   geo = "Brazil",
                                   header = T,
                                   digits = "max")

tx_desocupacao_estados_202104 <- get_sidra(x = 6468,
                                           variable = 4099,
                                           period = "202104",
                                           geo = "State",
                                           geo.filter = "all", #todos os estados
                                           header = T,
                                           digits = "max")


serie_tx_br <- ts(data = tx_desocupacao_brasil$Valor, 
                  start = 2012, frequency = 4)
serie_tx_br


plot(serie_tx_br, 
     main = "Taxa de desocupação no Brasil", 
     type = "l",
     col = "orange1",
     xlab = "Tempo", 
     ylab = "Taxa de desocupação", 
     ylim = c(5,16), lwd = 2, bty = "n", xlim = c(2012,2022),
     cex.lab = 1.5)
axis(side = 1, at = seq(2013, 2021, by = 2))

#Máximo no 3° trimestre de 2020, com 14.6%.
  
# estados <- geobr::read_state(year = 2020)
# tx_desocupacao_estados_202102 <- tx_desocupacao_estados_202102[, c("Valor", "Unidade da Federação (Código)")]
# tx_desocupacao_estados_202102 <- arrange(tx_desocupacao_estados_202102, "Unidade da Federação (Código)")
# estados$Valor <- tx_desocupacao_estados_202102$Valor

tx_desocupacao_estados_202104 %>% 
  mutate(`Unidade da Federação (Código)` = as.numeric(`Unidade da Federação (Código)`)) %>% 
  inner_join(states, 
             by = c("Unidade da Federação (Código)" = "code_state")) -> data_graph

(ggplot(data_graph) +
  geom_sf(aes(fill = Valor, geometry = geom, 
              text = paste("Estado:", name_state, 
                           "\nTaxa:", Valor)), size = .15) +
  scale_fill_distiller(palette = "Oranges", 
                       direction = 1,
                       name = "Taxa") +
  theme_void() -> g)


plotly::ggplotly(g, tooltip = "text")


search_sidra("pessoas desalentadas")

info2 <- info_sidra(6813)


p_desalentada_brasil<-get_sidra(x = 6813,
                                    variable = 9869,
                                    period = "201201-202104",
                                    geo = "Brazil",
                                    header = T,
                                    digits = "max")


p_desalentada_estados<-get_sidra(x = 6813,
                                     variable = 9869,
                                     period = "202104",
                                     geo = "State",
                                     header = T,
                                     digits = "max")


serie_desalento_br <- ts(p_desalentada_brasil$Valor, start = 2012, frequency = 4)

plot(serie_desalento_br,main = c("Percentual de pessoas desalentadas na",
                                 "força de trabalho potencial no Brasil"),
     type = "o", col = "orange1", xlab = "Tempo", 
     ylab = "Percentual de pessoas desalentadas", ylim = c(0,6), lwd = 2, bty = "n", 
     xlim = c(2012, 2022))
axis(side = 1, at = seq(2013, 2021, by = 2))


(p_desalentada_estados %>% 
    select("Valor","Unidade da Federação (Código)") %>% 
    mutate(`Unidade da Federação (Código)` = as.numeric(`Unidade da Federação (Código)`)) %>% 
  inner_join(states, by = c("Unidade da Federação (Código)" = "code_state")) %>% 
ggplot() +
  geom_sf(aes(fill=Valor, geometry = geom), size=.15) +
  scale_fill_distiller(palette = "Oranges", 
                       direction = 1,
                       name = "Percentual") +
  theme_void() -> g)

plotly::ggplotly(g)








