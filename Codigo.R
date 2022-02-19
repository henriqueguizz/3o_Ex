library(rvest)
library(readr)
library(dplyr)
library(reshape2)

#coletando dados de temperatura da web e gerando dataframe
forecasts <- read_html("https://g1.globo.com/previsao-do-tempo/rj/rio-de-janeiro.ghtml") %>%
  html_nodes(".forecast-next-days__item-value") %>%
  html_text()

df <- as.data.frame(parse_number(forecasts))

row_odd <- seq_len(nrow(df)) %% 2

df_row_odd <- df[row_odd == 1, ]
maximas <- as.data.frame(df_row_odd)
df_row_even <- df[row_odd == 0, ]
minimas <- as.data.frame(df_row_even)

maximas <- mutate(maximas, dia = c(1:8), .before = df_row_odd)
minimas <- mutate(minimas, dia = c(1:8), .before = df_row_even)

temperatura <- merge(maximas, minimas, by="dia")
colnames(temperatura)[2] <- "máximas"
colnames(temperatura)[3] <- "mínimas"

#gráfico de linhas 
ggplot(temperatura, aes(dia)) + 
  geom_line(aes(y = máximas, colour="Máximas")) + 
  geom_point(y = temperatura$máximas, color = "red") +
  geom_text(y = temperatura$máximas, aes(label = máximas, vjust = "inward", hjust = "inward"), color = "red") +
  geom_line(aes(y = mínimas, colour="Mínimas")) + 
  geom_point(y = temperatura$mínimas, color = "dodgerblue") +
  geom_text(y = temperatura$mínimas, aes(label = mínimas, vjust = "inward", hjust = "inward"), color = "dodgerblue") +
  labs(title="Temperaturas mínima e máxima nos próximos 8 dias - Rio de Janeiro, RJ", subtitle= "Em graus Celsius",x="Dia", y = "Temperatura (ºC)", caption = "Fonte: Climatempo", col="Legenda") +
  theme(plot.title = element_text(face = "bold"))

#tabela de temperatura média
nova_df <- temperatura %>%
  summarise(media_maximas = mean(máximas), media_minimas = mean(mínimas))

df_barra <- melt(nova_df[, c("media_maximas", "media_minimas")])

df_barra %>%
  ggplot(aes(x=variable, y=value)) +
  geom_bar(stat = "identity", color=c("red", "dodgerblue"), fill=c("red", "dodgerblue")) +
  geom_text(aes(label= round(value, 0)), vjust=-1, hjust=0.5, size=4, color="black") +
  labs(title="Média das temperaturas mínima e máxima nos próximos 8 dias - Rio de Janeiro, RJ", subtitle= "Em graus Celsius",x="Médias", 
       y = "Temperatura média", caption = "Fonte: Climatempo") +
  ylim(0, 40)
