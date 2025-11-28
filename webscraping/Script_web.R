
install.packages(c(
  "httr", "xml2",
  "dplyr", "stringr", "purrr", "readr", "lubridate", "tidyr",
  "tidytext", "quanteda", "topicmodels",
  "ggplot2", "forcats", "scales", "quanteda.textmodels", "quanteda.textstats"
))

#Web scraping
library(httr)
library(xml2)

#Manipulación de datos
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(lubridate)
library(tidyr)

#Análisis de texto
library(tidytext)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(topicmodels)

#Visualización 
library(ggplot2)
library(forcats)
library(scales)

#1.2: DEFINIR PARÁMETROS
OUT_DIR  <- "salidas"
CSV_OUT  <- file.path(OUT_DIR, "everygirl_only.csv")
pause_sec        <- 0.8
max_pages_cat    <- 200   # categorías
max_pages_search <- 60    # búsquedas
YEAR_MIN <- 2017; YEAR_MAX <- 2024

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
zzz <- function() Sys.sleep(pause_sec)


#PASO 2: FUNCIONES PARA EXTRAER INFORMACIÓN DE LOS FEEDS
read_feed_page <- function(feed_page_url){
  zzz()
  resp <- tryCatch(httr::GET(feed_page_url, httr::user_agent("Mozilla/5.0")), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) return(tibble())
  xml  <- xml2::read_xml(httr::content(resp, as="raw"))
  items <- xml2::xml_find_all(xml, ".//item")
  if (!length(items)) return(tibble())
  tibble(
    title    = xml2::xml_text(xml2::xml_find_all(items, "title")),
    link     = xml2::xml_text(xml2::xml_find_all(items, "link")),
    pub_date = xml2::xml_text(xml2::xml_find_all(items, "pubDate")),
    author   = xml2::xml_text(xml2::xml_find_all(items, "dc:creator")),
    summary  = xml2::xml_text(xml2::xml_find_all(items, "description"))
  )
}

read_feed_paged <- function(feed_base, max_pages, site, feed_kind, save_tag=NULL){
  message(">>> ", feed_base)
  out <- list()
  for (p in 1:max_pages){
    u <- if (p == 1) feed_base else if (grepl("\\?", feed_base)) paste0(feed_base, "&paged=", p) else paste0(gsub("/$", "", feed_base), "/?paged=", p)
    message("   página ", p, ": ", u)
    df <- read_feed_page(u)
    if (nrow(df) == 0) { message("   (sin ítems) fin"); break }
    if (length(out)){
      prev <- unique(unlist(lapply(out, `[[`, "link"))); cur <- unique(df$link)
      if (length(intersect(prev, cur))/max(length(cur),1) > 0.8){ message("   (>80% repetidos) fin"); break }
    }
    df$feed <- feed_base; df$site <- site; df$feed_kind <- feed_kind
    out[[length(out)+1]] <- df
    if (!is.null(save_tag) && (p %% 10 == 0)){
      tmp <- dplyr::bind_rows(out)
      readr::write_csv(tmp, file.path(OUT_DIR, paste0("partial_", save_tag, ".csv")))
      message("   guardado parcial: ", nrow(tmp), " filas (", save_tag, ")")
    }
  }
  if (!length(out)) return(tibble())
  dplyr::bind_rows(out)
}

safe_bind <- function(x) if (length(x)) dplyr::bind_rows(x) else dplyr::tibble()

# PASO 3: DEFINIR LAS FUENTES RSS (CATEGORÍAS Y BÚSQUEDA POR PALABRAS CLAVES)
feeds_eg_cat <- c(
  "https://theeverygirl.com/category/wellness/feed/",
  "https://theeverygirl.com/category/career-finance/feed/",
  "https://theeverygirl.com/category/career-finance/productivity/feed/",
  "https://theeverygirl.com/category/lifestyle/feed/",
  "https://theeverygirl.com/category/beauty/feed/",
  "https://theeverygirl.com/category/home/feed/",
  "https://theeverygirl.com/category/travel/feed/",
  "https://theeverygirl.com/category/food-drink/feed/"
)

keywords <- c(
  "self care","self-care","wellness","health","mental health","mindfulness","therapy","healing","rest","sleep",
  "gratitude","journaling","stress","anxiety","burnout","boundaries","confidence","esteem","calm","balance",
  "productivity","focus","routine","morning routine","evening routine","time blocking","goal setting","discipline",
  "motivation","attention","organization","planner","bullet journal","to-do list","notion","time management",
  "work-life balance","career","work","success","habits","habit","reset","energy","nutrition","movement",
  "beauty","skin","fitness","home","declutter","minimalism","slow living","sustainability","budget","money"
)
eg_search_feeds <- paste0("https://theeverygirl.com/?s=", URLencode(keywords), "&feed=rss2")

#PASO 4: EXTRACCIÓN COMPLETA DE ARTÍCULOS Y CONSTRUCCIÓN DEL CORPUS

cat_eg <- purrr::map(feeds_eg_cat, ~ read_feed_paged(.x, max_pages_cat, site="everygirl", feed_kind="category", save_tag="eg_cat"))
sea_eg <- purrr::map(eg_search_feeds, ~ read_feed_paged(.x, max_pages_search, site="everygirl", feed_kind="search",  save_tag="eg_search"))

rss_df <- dplyr::bind_rows(safe_bind(cat_eg), safe_bind(sea_eg)) %>%
  dplyr::mutate(
    summary  = stringr::str_squish(stringr::str_replace_all(summary, "<.*?>", "")),
    category = stringr::str_extract(feed, "(?<=/category/)[^/]+"),
    source   = dplyr::if_else(!is.na(category) & category != "", paste(site, category, sep=":"), paste(site, feed_kind, sep=":"))
  ) %>%
  dplyr::distinct(link, .keep_all = TRUE)

readr::write_csv(rss_df, CSV_OUT)
message("CSV final guardado en: ", normalizePath(CSV_OUT, winslash = "/"))


#ENTREGA 2
B. PROCESAMIENTO DE DATOS
#PASO 5: CARGA DEL CORPUS Y FECHAS
#5.1: CARGAR CORPUS
df <- read_csv("salidas/everygirl_only.csv")

#5.2: FECHAS Y FILTRO TEMPORAL
df <- df %>%
  mutate(
    pub_date_parsed = parse_date_time(pub_date, 
                                      orders = "a, d b Y H:M:S z",
                                      locale = "C"),
    year = year(pub_date_parsed)
  ) %>%
  filter(year >= 2017, year <= 2024)

#PASO 5.3: DESCRIPTIVOS
# Total de artículos
n_total <- nrow(df)
cat("Total de artículos en el corpus:", n_total, "\n")

# Distribución por año
by_year <- df %>% count(year) %>% arrange(year)
cat("\nArtículos por año:\n")
print(by_year)

df %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "#c4a3e8") +
  geom_text(aes(label = n), 
            vjust = -0.5, 
            size = 4) +
  scale_x_continuous(breaks = 2017:2024) +
  ylim(0, 1400) +   # <<--- AUMENTA EL ESPACIO (puedes subirlo más si quieres)
  labs(
    title = "Cantidad de artículos por año",
    x = "Año",
    y = "Número de artículos"
  ) +
  theme_minimal(base_size = 14)


#PASO 6: LIMPIEZA DE TEXTO
df <- df %>%
  mutate(
    text = paste(title, summary, sep = ". "),
    text = str_replace_all(text, "&#8217;", "'"),
    text = str_replace_all(text, "&#8211;", "-"),
    text = str_replace_all(text, "&amp;", "&"),
    text = str_remove_all(text, "This post.*$"),
    text = str_remove_all(text, "The Everygirl.*$"),
    text = str_replace_all(text, "appeared", " "),
    text = str_replace_all(text, "everygirl", " "),
    text = str_replace_all(text, "post", " "),
    text = str_squish(text)
  )


df <- df %>%
  mutate(
    text = str_replace_all(text, "\\bve\\b", " "),
    text = str_replace_all(text, "\\bve’\\b", " "),
    text = str_replace_all(text, "’ve", " ")   # por si viene de "we've"
  )

df <- df %>% mutate(doc_id = row_number())


# PASO 7: RECONSTRUCCIÓN Y FILTRADO DE CATEGORÍAS
#7.1: RECONSTRUCCIÓN 
df <- df %>%
  mutate(
    category2 = case_when(
      str_detect(feed, "wellness") ~ "wellness",
      str_detect(feed, "career-finance/productivity") ~ "productivity",
      str_detect(feed, "career-finance") ~ "career",
      str_detect(feed, "beauty") ~ "beauty",
      str_detect(feed, "lifestyle") ~ "lifestyle",
      str_detect(feed, "home") ~ "home",
      TRUE ~ "bienestar_transversal"
    )
  )

#7.2 FILTRAR
valid_cats <- df %>% count(category2) %>% filter(n > 150) %>% pull(category2)
df <- df %>% filter(category2 %in% valid_cats)

#7.3 VARIABLE GROUP
df <- df %>%
  mutate(
    group = if_else(category2 == "wellness",
                    "wellness",
                    "bienestar_transversal")
  )

df %>% 
  count(group) %>% 
  ggplot(aes(x = group, y = n, fill = group)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), 
            vjust = -0.3, 
            size = 6, 
            color = "black") +
  scale_fill_manual(values = c(
    "bienestar_transversal" = "#F4A7B9",  # rosa pastel
    "wellness" = "#71D0D4"               # turquesa suave
  )) +
  expand_limits(y = max(df %>% count(group) %>% pull(n)) * 1.15) +
  labs(
    title = "Cantidad de artículos por grupo",
    x = "Grupo",
    y = "Número de artículos"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title.x = element_text(),
    axis.title.y = element_text()
  )

#PASO 8: STOPWORDS
data("stop_words")

custom_stops <- tibble(word = c(
  "im","ive","youre","its","cant","dont","didnt","theres",
  "8217","8220","8221","â","€","™","’","‘","“","”"
))

all_stops <- bind_rows(stop_words, custom_stops)

#PASO 9: TOKENIZACIÓN
tokens_unigram <- df %>%
  select(doc_id, year, group, text) %>%
  unnest_tokens(word, text) %>%
  filter(
    !word %in% all_stops$word,
    !str_detect(word, "^[0-9]+$"),
    nchar(word) > 2
  )

#TOKENIZACIÓN EN BIGRAMAS
tokens_bigram <- df %>% select(doc_id, text) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_sep <- tokens_bigram %>%
  separate(bigram, into = c("w1","w2"), sep = " ") %>%
  filter(
    !w1 %in% all_stops$word,
    !w2 %in% all_stops$word,
    nchar(w1) > 2,
    nchar(w2) > 2
  ) %>%
  mutate(
    w1 = tolower(w1),
    w2 = tolower(w2),
    w1 = gsub("s$", "", w1),
    w2 = gsub("s$", "", w2),
    bigram = paste(w1, w2)
  ) %>%
  count(bigram, sort = TRUE)


C. ANÁLISIS DE DATOS 
# PASO 10: TF-IDF por grupo: wellness vs bienestar_transversal

tfidf_words <- tokens_unigram %>%
  count(group, word, sort = TRUE) %>%
  bind_tf_idf(word, group, n) %>%
  arrange(desc(tf_idf))
top_tfidf <- tfidf_words %>%
  group_by(group) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()

#GRÁFICO
library(scales)

# Reordenar los grupos
top_tfidf$group <- factor(top_tfidf$group,
                          levels = c("wellness", "bienestar_transversal"))

ggplot(top_tfidf,
       aes(x = reorder_within(word, tf_idf, group),
           y = tf_idf,
           fill = group)) +
  
  geom_col(show.legend = FALSE, width = 0.7) +
  
  facet_wrap(~ group, scales = "free", ncol = 2) +
  
  coord_flip() +
  tidytext::scale_x_reordered() +
  
  scale_fill_manual(values = c(
    "bienestar_transversal" = "#c9addb",
    "wellness" = "#a7d8f0"
  )) +
  
  scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
  
  labs(
    title = "Palabras más distintivas por grupo",
    subtitle = "TF-IDF para Wellness vs Bienestar Transversal",
    x = NULL,
    y = "Valor TF-IDF"
  ) +
  
  theme(text = element_text(size = 8)) +
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

#PASO 11: BIGRAMAS POR GRUPO: WELLNESS Y BIENESTAR_TRANSVERSAL + GRÁFICO
bigrams_group <- tokens_bigram %>%
  separate(bigram, into = c("w1", "w2"), sep = " ") %>%
  filter(
    !w1 %in% all_stops$word,
    !w2 %in% all_stops$word,
    nchar(w1) > 2,
    nchar(w2) > 2
  ) %>%
  mutate(
    w1 = tolower(w1),
    w2 = tolower(w2),
    w1 = gsub("s$", "", w1),
    w2 = gsub("s$", "", w2),
    bigram = paste(w1, w2)
  ) %>%
  inner_join(df %>% select(doc_id, group), by = "doc_id") %>%
  count(group, bigram, sort = TRUE)


top_bi <- bigrams_group %>%
  group_by(group) %>%
  slice_max(n, n = 15) %>%
  ungroup()

ggplot(top_bi,
       aes(x = n, 
           y = fct_reorder(bigram, n),
           fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free_y") +
  scale_fill_manual(values = c(
    "bienestar_transversal" = "#F7B5C8",  
    "wellness" = "#F7D9A8"                
  )) +
  labs(
    title = "Bigramas más frecuentes en Wellness vs Bienestar Transversal",
    x = "Frecuencia",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +   
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),
    strip.text = element_text(size = 8),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 6)
  )

#PASO 12: KEYNESS
#13.1: LIMPIEZA ESPECIAL
limpiar <- function(x){
  x %>%
    str_replace_all("[[:punct:]]", " ") %>%    
    str_replace_all("[0-9]+", " ") %>%        
    str_replace_all("â|€|™", " ") %>%         
    str_replace_all("8217|8220|8221", " ") %>% 
    str_squish() %>%
    tolower()
}

df$text_clean <- limpiar(df$text)

#12.2 CREAR CORPUS Y TOKENS
corp_key <- df %>%
  mutate(group = if_else(category2 == "wellness", "wellness", "bienestar_transversal")) %>%
  corpus(text_field = "text_clean")

tokens_key <- tokens(corp_key,
                     remove_punct = TRUE,
                     remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(all_stops$word)

dfm_key <- dfm(tokens_key)

dfm_sub <- dfm_key[
  docvars(dfm_key, "group") %in% c("wellness", "bienestar_transversal"), ]


# 12.3 CÁLCULO DE KEYNESS
key_clean <- textstat_keyness(dfm_sub, target = docvars(dfm_sub, "group") == "wellness")

key20 <- key_clean %>%
  slice_max(abs(chi2), n = 20) %>%
  mutate(
    group_label = ifelse(chi2 > 0, "Wellness", "Bienestar transversal")
  )

#12.4: GRÁFICO
ggplot(key20, aes(x = reorder(feature, chi2), y = chi2, fill = group_label)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Wellness" = "#1f78b4", "Bienestar transversal" = "grey60"))+
  labs(
    title = "Términos distintivos del discurso (wellness vs bienestar_transversal)",
    subtitle = "Comparación basada en chi-cuadrado",
    x = "Término",
    y = "Asociación (χ²)",
    fill = "Grupo"
  ) +
  theme_minimal(base_size = 14)

#PASO 13: TOPIC MODELING (LDA) EN WELLNESS
#13.1 FILTRAR SOLO WELLNESS
df_well <- df %>% filter(category2 == "wellness")

corp_well <- corpus(df_well, text_field = "text_clean")

dfm_well <- tokens(corp_well,
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(all_stops$word) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)

dtm_well <- convert(dfm_well, to = "topicmodels")

#13.2 AJUSTAR MODELO LDA
set.seed(123)
lda_well <- LDA(dtm_well, k = 4, control = list(seed = 123))

beta_well <- tidy(lda_well, matrix = "beta")

top_terms_well <- beta_well %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

#13.3 GRÁFICO
ggplot(top_terms_well,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(
    title = "Términos más probables por tópico (LDA, wellness)",
    subtitle = "Subdiscursos dentro de la categoría wellness",
    x = NULL,
    y = "Probabilidad β"
  )

#PASO 14: EVOLUCIÓN TEMPORAL 
palabras_clave <- c("anxiety","stress","routine","balance",
                    "health","mental","productivity","self")

freq_year <- tokens_unigram %>%
  filter(word %in% palabras_clave) %>%
  count(year, word)

ggplot(freq_year,
       aes(x = year, y = n, color = word)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2017:2024) +
  labs(
    title = "Evolución temporal de términos asociados al bienestar (2017–2024)",
    x = "Año",
    y = "Frecuencia absoluta",
    color = "Término"
  ) +
  theme_minimal(base_size = 14)

