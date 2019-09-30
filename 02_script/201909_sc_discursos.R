# Paquetes ----
require(pacman)
p_load(
  rvest, # scrapping
  tm, tidytext, # text mining
  haven, foreign, # abrir bd
  wordcloud2,
  gganimate, ggcorrplot, gridExtra, ggthemes, hrbrthemes, magick, scales, RColorBrewer, # Animación y elementos extras para ggplot
  tidyverse # infaltable
)

# Directorios ----
inp <- "Parametria/cemefi_201908/01_datos/"
out <- "Parametria/cemefi_201908/03_productos/"
# Leyenda de todas las gráficas
fiuffi <- "Elaboración propia con #Rstats | @guzmart_ | @parametria"

# Scrapping ----
# Sólo seguir este paso si se desea replicar la extracción de datos.
# Los discursos utilizados se encuentran en la carpeta '02_datos'

# DF que contendrá todas las observaciones
final <- data.frame()

# x = número de página y loop
for(x in 1:30){ 
  # urls de las páginas en dónde se guardan las versiones estenográficas
  amlos <- read_html(paste0("https://lopezobrador.org.mx/secciones/version-estenografica/",
                            "page/",as.character(x),"/"))
  
  # urls de las versiones estenográficas
  urls <- amlos %>% 
    html_nodes("div.entry-media a") %>% 
    html_attr("href")
  
  # extraer fechas de url
  fechas <- str_sub(urls,29,38)
  fechas <- as.Date.character(fechas, format = "%Y/%m/%d")
  
  # base temporal por página
  tempo <- data.frame(
    url = urls,
    fecha = fechas
  )
  
  titles <- c()
  bodies <- c()
  # for para extraer títulos y textos
  for(i in tempo$url){
    
    wbpg <- read_html(i)
    title <- wbpg %>%
      html_node("title") %>%
      html_text()
    titles <- append(titles, title)
    
    wbpg <- read_html(i)
    body <- wbpg %>%
      html_nodes("p") %>%
      html_text()
    one_body <- paste(body, collapse=" ")
    bodies <- append(bodies, one_body)
    
  }
  
  tempo$título <- titles
  tempo$texto <- bodies
  tempo$loop <- as.character(x)
  
  # se guarda cada loop en final
  final <- bind_rows(final, tempo)
  rm(tempo,urls,fechas,titles,title,bodies,wbpg,body,one_body)
}

# write_csv(sent_amlo_sent, path = paste0(inp,"discursos_amlo_20190924_20181204.csv"))

# Data ----
# Se recopilaron 435 discursos
d <- read_csv(paste0(inp,"discursos_amlo_20190924_20181204.csv"))

# El siguiente código ya fue utilizado para limpiar el texto; no es necesario aplicarlo a 'discursos_amlo_20190924_20181204.csv' 
# Limpiar un poco más y guardar
# d$texto <- gsub("Año del Caudillo del Sur Emiliano Zapata", "", d$texto)
# d$texto <- gsub("Copyright Derechos Reservados", "", d$texto)
# d$texto <- gsub("Sitio Oficial de Andrés Manuel López Obrador", "", d$texto)
# d$texto <- str_replace_all(d$texto,"PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR:", "~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR")
# d$texto <- str_replace_all(d$texto,"INTERLOCUTOR:", "~~~INTERLOCUTORA")                     
# d$texto <- str_replace_all(d$texto,"INTERLOCUTORA:", "~~~INTERLOCUTORA")
# d$texto <- str_replace_all(d$texto,"PREGUNTA:", "~~~INTERLOCUTORA")

# En 91 de éstos, se mencionó, al menos una vez, el término 'sociedad civil'
sc <- d %>% 
  mutate(sc = ifelse(str_detect(tolower(texto),"sociedad civil"),1,
                     ifelse(str_detect(tolower(texto),"organizaciones civiles"),1,0))) %>% 
  subset(sc==1)

# Limpiar texto ----
# Reemplazar 'Presidente...' por 'AMLO'
sc$texto <- gsub("PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR", "AMLO", sc$texto)

# Mantener sólo intrevenciones de AMLO
sc_amlo <- sc %>% 
  # Una observación por intervención
  unnest_tokens("txt", input = texto, token = stringr::str_split, pattern = "~~~") %>% 
  mutate(amlo = ifelse(str_detect(txt, "amlo"),1,0)) %>% 
  subset(amlo==1) %>% select(-amlo) %>% 
  mutate(sc = ifelse(str_detect(tolower(txt),"sociedad civil"),1,
                     ifelse(str_detect(tolower(txt),"organizaciones civiles"),1,0)),
         sc_lag = lag(sc, default = 0)) %>% 
  # Mantener sólo intervenciones que hablen de SC, y una posterior a ésta | sc_lag==1
  subset(sc==1)

# Quitamos caracteres especiales hasta esta línea porque nuestro separador es '~~~'
# Quitar caracteres que no sean palabras
sc_amlo$txt <- gsub("[^[:alpha:][:space:]]*", "", sc_amlo$txt)
# Quitar espacios al principio y al final de cada string
sc_amlo$txt <- gsub("^\\s+|\\s+$", "", sc_amlo$txt)

# ¿Qué dice el presidente cuando habla de SC? ----
stopwords <- c("va", "van", "mil", "entonces", "si", "dos", "así", "año", "años",
               "amlo", "emiliano", "zapata", "presidente","ahora","aquí", "tres",
               "ahí")
stop_words <-  c(tm::stopwords("spanish"), stopwords)
custom_stop_words <- data_frame(stop_words, lexicon = "custom")
custom_stop_words %>% arrange(stop_words) 

sc_amlo_word <- sc_amlo %>% 
  select(txt) %>% 
  mutate(txt = toupper(txt)) %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 1) %>% 
  filter(!word %in% custom_stop_words$stop_words) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

fiuf <- "20 palabras más frecuentes en discursos de AMLO"
fiuff <- "referentes a Sociedad Civil"
sc_amlo_word %>% head(20) %>% 
  ggplot(aes(reorder(word,n), n, fill=n)) +
  geom_col() +
  coord_flip() +
  theme_ipsum(grid="X") +
  scale_fill_distiller(palette="Spectral") +
  guides(fill=FALSE)+
  labs(title=fiuf,
       subtitle = fiuff,
       y="Número de Intervenciones",
       x="",
       caption=fiuffi) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 15))
ggsave(filename = paste0("01_palabras.png"), 
       path = out, width = 15, height = 10, dpi = 100)

sc_amlo_bigram <- sc_amlo %>%
  select(txt) %>% 
  mutate(txt = toupper(txt)) %>% 
  unnest_tokens(bigram, txt, token = "ngrams", n = 2) %>% 
  separate(bigram, c("palabra_1", "palabra_2"), sep = " ", remove = F) %>% 
  filter(!palabra_1 %in% custom_stop_words$stop_words) %>%
  filter(!palabra_2 %in% custom_stop_words$stop_words) %>% 
  count(bigram, palabra_1, palabra_2, sort = TRUE) %>% 
  group_by(bigram) %>% 
  mutate(ranking = rank(-n, ties.method = "first")) %>% 
  ungroup

fiuf <- "20 bigramas más frecuentes en discursos de AMLO"
fiuff <- "referentes a Sociedad Civil"
sc_amlo_bigram %>% head(20) %>% 
  ggplot(aes(reorder(bigram,n), n,
             fill=n)) +
  geom_col() +
  coord_flip() +
  theme_ipsum(grid="X") +
  scale_fill_distiller(palette="Spectral") +
  guides(fill=FALSE)+
  labs(title=fiuf,
       subtitle = fiuff,
       y="Número de Intervenciones",
       x="",
       caption=fiuffi) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 15))
ggsave(filename = paste0("02_bigramas.png"), 
       path = out, width = 15, height = 10, dpi = 100)

rm(sc_amlo_bigram, sc_amlo_word)

# ¿Qué sentimiento transmite? ----
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

sc_amlo_general <- subset(sc_amlo, sc==1) %>% 
  mutate(id = 1:length(txt),
         id = formatC(id, width = 3, format="d", flag="0")) %>% 
  select(id, txt, fecha)

sc_amlo_afinn <- select(sc_amlo_general, id, txt, fecha) %>%
  unnest_tokens(input = "txt", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

sc_amlo_sent <- sc_amlo_afinn %>%
  group_by(id) %>% 
  summarise(puntuacion = mean(Puntuacion)) %>%
  right_join(sc_amlo_general, ., by = "id") %>% 
  mutate(puntuacion = ifelse(is.na(puntuacion), 0, puntuacion),
         colour = ifelse(puntuacion>0,"pos",
                         ifelse(puntuacion<0,"neg","neu")))

fiuf <- "Análisis de sentimiendo de intervenciones de AMLO"
fiuff <- "Referentes a Sociedad Civil"
ggplot(sc_amlo_sent,
       aes(x = fecha,
           y = puntuacion,
           fill = colour)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#FC4E07", "#E7B800", "#00AFBB")) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 month") +
  xlab("") + ylab("Puntuación") +
  labs(title = fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_ipsum(grid="Y") +
  theme(plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12))
ggsave(filename = paste0("03_01_sent_fecha.png"), 
       path = out, width = 15, height = 10, dpi = 100)


# 02 Análisis de encuesta ----
d <- read_sav(paste0(inp,"n201903_sc.sav"))

# 02.1 Conocimiento ----
oscs_con <- d %>% 
  select(P37, Pond)%>% 
  group_by(P37) %>% 
  summarise(tot = sum(Pond, na.rm = TRUE)) %>% 
  mutate(fac = sum(tot),
         porc = tot/fac*100,
         P37 = ifelse(P37==1, "Sí ha escuchado", "No ha escuchado")) %>% 
  select(P37, porc)

nrows <- 10
oscs_con <- expand.grid(y = 1:nrows, x = 1:nrows) %>% 
  mutate(conocimiento = c(rep("Sí ha escuchado",21),
                          rep("No ha escuchado",79)))

fiuf <- "¿Usted sabe o ha escuchado de las Organizaciones de la Sociedad Civil, es decir, instituciones que trabajan para fines públicos, pero que no pertenecen al gobierno y actúan sin fines de lucro?"
ggplot(oscs_con, 
       aes(x = x, y = y, fill = conocimiento)) + 
  geom_tile(color = "black", size = 0.5)  +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("",values = c("#FC4E07", "#00AFBB")) +
  labs(title=str_wrap(fiuf,95), subtitle="",
       caption=fiuffi) + 
  theme_ipsum(grid="Y") +
  theme(axis.text = element_blank(),
        plot.title = element_text(size = 22),
        plot.caption = element_text(size = 12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")
ggsave(filename = paste0("04_conocimiento.png"), 
       path = out, width = 15, height = 10, dpi = 100)  

# 02.2 Importancia ----
oscs_imp <- d %>% 
  select(P38, Pond)%>% 
  mutate(P38 = ifelse(P38>4,NA,P38)) %>% 
  drop_na() %>% 
  group_by(P38) %>% 
  summarise(tot = sum(Pond, na.rm = TRUE)) %>% 
  mutate(fac = sum(tot),
         porc = tot/fac*100,
         p38 = ifelse(P38==1, "Muy importantes",
                      ifelse(P38==2, "Algo importantes",
                             ifelse(P38==3,"Poco importantes","Nada importantes"))),
         
         alpha = ifelse(P38==1, "1",
                        ifelse(P38==2, "0",
                               ifelse(P38==3,"1","0")))) %>% 
  rename("orden" = P38) %>% 
  select(p38, porc, alpha, orden) 

fiuf <- "¿Qué tan importantes cree usted que son las Organizaciones de la Sociedad Civil para nuestro país?"
ggplot(oscs_imp,
       aes(x = reorder(p38, orden),
           y = porc,
           label = paste(round(porc,2),"%"),
           fill = p38,
           alpha = as.numeric(alpha))) +
  geom_col(show.legend = FALSE, width = 0.5) +
  geom_text(show.legend = FALSE, nudge_y = 2, size = 5) +
  scale_fill_manual(values = c("#00AFBB", "#00AFBB",
                               "#FC4E07","#FC4E07")) +
  scale_alpha(range = c(0.7,1)) +
  xlab("") + ylab("") +
  labs(title = fiuf,
       caption = fiuffi) +
  theme_ipsum(grid=F) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(size = 22),
        plot.caption = element_text(size = 12))
ggsave(filename = paste0("05_importancia.png"), 
       path = out, width = 15, height = 10, dpi = 100)

# 02.3 Aprobación presidencial ----
oscs_aprpres <- d %>% 
  select(P38, P12A, Pond)%>% 
  mutate(P38 = ifelse(P38>4,NA,P38),
         P12A = ifelse(P12A>97,NA,P12A)) %>% 
  drop_na() %>% 
  group_by(P12A, P38) %>% 
  summarise(tot = sum(Pond, na.rm = TRUE)) %>% 
  mutate(fac = sum(tot),
         porc = tot/fac*100,
         p38 = ifelse(P38==1, "Muy importantes",
                      ifelse(P38==2, "Algo importantes",
                             ifelse(P38==3,"Poco importantes","Nada importantes"))),
         apr_pres = ifelse(P12A==1, "Aprueba mucho",
                           ifelse(P12A==2, "Aprueba poco",
                                  ifelse(P12A==3,"Desaprueba poco","Desaprueba mucho"))),
         alpha = ifelse(P12A==1, "1",
                        ifelse(P12A==2, "0",
                               ifelse(P12A==3,"1","0")))) %>% 
  rename("orden" = P12A) %>% 
  select(p38, apr_pres, porc, alpha, orden) 

fiuf <- "Importancia de las OSC's, por aprobación presidencial"
ggplot(oscs_aprpres, 
       aes(area = porc, 
           fill = reorder(apr_pres,orden), 
           label = paste(p38,"\n", round(porc,2)),
           subgroup = apr_pres)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre",
                    grow = TRUE) +
  scale_fill_manual("",
                    values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00")) +
  labs(title = fiuf,
       caption = fiuffi) +
  theme_ipsum(grid=F) +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size = 12),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0("06_imp_aprpres.png"), 
       path = out, width = 15, height = 10, dpi = 100)
