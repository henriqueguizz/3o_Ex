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
colnames(temperatura)[2] <- "m�ximas"
colnames(temperatura)[3] <- "m�nimas"

#gr�fico de linhas 
ggplot(temperatura, aes(dia)) + 
  geom_line(aes(y = m�ximas, colour="M�ximas")) + 
  geom_point(y = temperatura$m�ximas, color = "red") +
  geom_text(y = temperatura$m�ximas, aes(label = m�ximas, vjust = "inward", hjust = "inward"), color = "red") +
  geom_line(aes(y = m�nimas, colour="M�nimas")) + 
  geom_point(y = temperatura$m�nimas, color = "dodgerblue") +
  geom_text(y = temperatura$m�nimas, aes(label = m�nimas, vjust = "inward", hjust = "inward"), color = "dodgerblue") +
  labs(title="Temperaturas m�nima e m�xima nos pr�ximos 8 dias - Rio de Janeiro, RJ", subtitle= "Em graus Celsius",x="Dia", y = "Temperatura (�C)", caption = "Fonte: Climatempo", col="Legenda") +
  theme(plot.title = element_text(face = "bold"))

#tabela de temperatura m�dia
nova_df <- temperatura %>%
  summarise(media_maximas = mean(m�ximas), media_minimas = mean(m�nimas))

df_barra <- melt(nova_df[, c("media_maximas", "media_minimas")])

df_barra %>%
  ggplot(aes(x=variable, y=value)) +
  geom_bar(stat = "identity", color=c("red", "dodgerblue"), fill=c("red", "dodgerblue")) +
  geom_text(aes(label= round(value, 0)), vjust=-1, hjust=0.5, size=4, color="black") +
  labs(title="M�dia das temperaturas m�nima e m�xima nos pr�ximos 8 dias - Rio de Janeiro, RJ", subtitle= "Em graus Celsius",x="M�dias", 
       y = "Temperatura m�dia", caption = "Fonte: Climatempo") +
  ylim(0, 40)
