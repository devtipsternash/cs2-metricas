library(googlesheets4)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(showtext)
library(lubridate)
library(scales)

# Fonte estilo The Athletic
font_add_google("Rajdhani", "optatext")
showtext_auto()

# Carregar e tratar dados
dados <- read_sheet("https://docs.google.com/spreadsheets/d/1rPfzUtFSTtFP4qGK2IYiVy8KaXK0i9OUBXUEuMgP0So/edit?gid=0") %>%
  separate(`K - D`, into = c("Kills", "Deaths"), sep = " - ") %>%
  mutate(
    Rating = as.numeric(str_replace(Rating2.1, ",", ".")),
    Rating = if_else(is.na(Rating), 0, Rating),
    `diff` = as.numeric(`diff`),
    Data = as.Date(Date, tryFormats = c("%y/%m/%d", "%Y-%m-%d", "%d/%m/%Y")), 
    Kills = as.integer(Kills),
    Deaths = as.integer(Deaths),
    Team_rounds = as.integer(str_extract(`Player team`, "\\((\\d+)\\)") %>% str_remove_all("[()]")),
    Opp_rounds = as.integer(str_extract(Opponent, "\\((\\d+)\\)") %>% str_remove_all("[()]")),
    `Player team` = str_remove(`Player team`, " \\(.*\\)"),
    Opponent = str_remove(Opponent, " \\(.*\\)"),
    Mes = floor_date(Data, "month")
  )

# Calcular médias mensais e normalizar para escala 0-99
dados_mes <- dados %>%
  group_by(Mes) %>%
  summarise(
    Kills_avg = mean(Kills, na.rm = TRUE),
    Deaths_avg = mean(Deaths, na.rm = TRUE),
    diff_avg = mean(`diff`, na.rm = TRUE),
    Rating_avg = mean(Rating, na.rm = TRUE),
    Team_rounds_avg = mean(Team_rounds, na.rm = TRUE),
    Opp_rounds_avg = mean(Opp_rounds, na.rm = TRUE)
  ) %>%
  pivot_longer(-Mes, names_to = "Metrica", values_to = "Valor") %>%
  mutate(Valor = rescale(Valor, to = c(0, 99))) # normaliza 0-99

# Categorias para cores
categoria_map <- c(
  "Kills_avg" = "Attack",
  "Rating_avg" = "Attack",
  "Deaths_avg" = "Defence",
  "Opp_rounds_avg" = "Defence",
  "Team_rounds_avg" = "Possession",
  "diff_avg" = "Progression"
)

# Paleta similar à do The Athletic
cores <- c(
  "Attack" = "#08306B",      # azul
  "Defence" = "#A33D5A",     # vermelho
  "Possession" = "#01735C",  # verde
  "Progression" = "#F4A300"  # amarelo
)

dados_mes <- dados_mes %>%
  mutate(Categoria = categoria_map[Metrica])

# Gráfico estilo The Athletic refinado

p <- ggplot(dados_mes, aes(x = Mes, y = Valor, group = Metrica, color = Categoria)) +
  geom_line(size = 0.9) +
  geom_point(shape = 21, fill = "#F4F1EB", size = 6.5, stroke = 1.5) +
  geom_text(aes(label = round(Valor, 0)), size = 3, fontface = "bold", 
            vjust = 0.5, hjust = 0.5, color = "black") +
  facet_wrap(~Metrica, ncol = 3) +
  scale_color_manual(values = cores) +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, 25), expand = c(0.02, 0.02)) +
  scale_x_date(date_labels = "%y-%m", date_breaks = "3 months") +
  theme_minimal(base_family = "optatext") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    axis.text.x = element_text(size = 8, color = "#444444", face = "bold"),  # eixo X em negrito
    axis.text.y = element_text(size = 8, color = "#444444", face = "bold"),  # eixo Y em negrito
    axis.title = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "#222222"),
    strip.background = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold", color = "#111111", hjust = 0),
    plot.subtitle = element_text(size = 13, color = "#444444", hjust = 0),
    plot.caption = element_text(size = 9, color = "#555555", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Evolução das métricas de jogadores CS",
    subtitle = "Comparação mensal ",
    caption = "DataviZ: @tipsternash DEFENCE: Mortes médias por jogador | Rounds perdidos do time\nPOSSESSION: Rounds ganhos do time\nPROGRESSION: Diferença média entre kills e deaths\nATTACK: Kills e Rating médio"
  )



p

ggsave("variacao_metricas_theathletic_refinado.png", p, width = 10, height = 8, dpi = 100, units = "in", bg ="#F4F1EB")
