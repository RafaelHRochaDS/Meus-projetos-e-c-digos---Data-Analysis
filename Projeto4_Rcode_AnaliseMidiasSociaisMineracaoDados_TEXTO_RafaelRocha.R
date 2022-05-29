################################################################
# "Análise de Mídias Sociais e Mineracão de Texto - Avaliação Final - T8"
# "Rafael Rocha - A56660250"
# "Maio/2021"
# "Professor: Fernando Ferreira"
################################################################

################################################################
# AGENDA
# 
# Acessarei os links sobre as notícias do BNDES presentes na página de buscas do portal G1;
# Apresentarei o passo a passo do processo de web scraping, criação da nuvem de palavras e desambiguação de entidades;
# Estruturarei um grafo com a coocorrência de entidades e analisarei por comunidades e relevâncias;
# Utilizaremos o GEPHI para extrair características, gerar métricas e visualizar as redes.
#           
################################################################

##### Usando o ambiente Python no R

# instalando e carregando o pacote reticulate
install.packages("reticulate")
library(reticulate)

# verificando a versao reticulada do python e a env
py_config()

# verificando todas as virtual envs
conda_list()

#conda_remove("r-spacy")
#conda_create(envname = "~/.rstudio-desktop/virtualenvs/spacy")

# instalando os pacotes na env
conda_install(envname = "r-reticulate", "spacy")

# instalando e carregando o pacote spacyr
install.packages("spacyr")
library(spacyr)

#find_spacy_env()

# instalando os modelos de linguagem na env
spacy_download_langmodel(model = "en", envname = "r-reticulate")
spacy_download_langmodel(model = "pt_core_news_sm", envname = "r-reticulate")
#model = c('pt_core_news_sm', 'en'), 
#virtualenv_root = "~/.rstudio-desktop/virtualenvs/spacy" )

# especificando o uso do modelo em portugues
spacy_initialize(model = "pt_core_news_sm",
                 save_profile = TRUE)

#virtualenv = "~/.rstudio-desktop/venv/spacy_virtualenv",


###### Carregando outros pacotes importantes para esse projeto ######

set.seed(123)

library(rvest) #web Scraping
library(stringr) #manipulacao dos dados
library(tidyverse) #colecao de pacotes e acessorios
library(tm) #mineracao de texto
library(igraph) #grafos
library(wordcloud)#nuvem de palavras
library(urltools) #manipulacao de url
library(gtools) #funcoes de suporte

##### Acessando as Páginas #####
#Abaixo uma função auxiliar para acessar os links

scrape_post_links <- function(site) {
  # coletando dados da HTML 
  source_html <- read_html(site)
  # pegando atributos do titulo dos links
  # buscando tages nos H2 header tags
  links <- source_html %>%
    html_nodes("div.widget--info__text-container") %>%
    html_nodes("a") %>%
    html_attr("href")
  # filtrando quaisquer titulo que sejam NA
  links <- links[!is.na(links)]
  # retornando vetor com filtros
  return(links)
}

##### Fazendo interações em 20 páginas #####

root <- "https://g1.globo.com/busca/?q=BNDES"
# obtendo o URL da pagina que vamos explorar
all_pages <- c(root, paste0(root, "&page=", 1:20))
# usando a funcao auxiliar que criamos para explorar o titulo de cada postagem
all_links <- lapply(all_pages, scrape_post_links)
# colapsar em um vetor
all_links <- unlist(all_links)


##### A URL real está contida em parâmetro do link “u” #####
# Criar uma função para extrair esses links

extract_urls <- function(raw_url) {
  params <- urltools::param_get(raw_url)
  scraped_url <- params$u
  return (url_decode(scraped_url))
}

cleaned_links <- lapply(all_links, extract_urls)
# Nao estamos interessados em videos do globoplay app
cleaned_links <- Filter(function(x) !any(grepl("globoplay", x)),
                        cleaned_links)


##### Acessar de cada link #####

scrape_post_body <- function(site) {
  # Escape 404 Not Found Errors
  try(
    text <- site %>%
      read_html %>%
      html_nodes("article") %>%
      html_nodes("p.content-text__container") %>%
      html_text
  )
}
data <- lapply(cleaned_links, scrape_post_body)
data <- lapply(data,
               function(item) paste(unlist(item),
                                    collapse = ''))
 
##### Pré-processar os textos para nuvens de palavras(1/2)
# convertendo todos os titulos em minusculas
cleaned <- tolower(data)
# removendo numeros dos titulos 
cleaned <- removeNumbers(cleaned)
# removendo stopwords e algumas palavras especificas que nao sao relevantes para a nuvem
cleaned <- removeWords(cleaned, c(stopwords("pt"), "bndes", "banco", "empresa", "empresas", 
                                  "desenvolvimento", "social", 
                                  "afirmou", "ano", "dois", "fim", 
                                  "disse", "rio", "janeiro", "dia", 
                                  "ter", "antes", "ser", "meses", "segundo", 
                                  "nacional", "presidente", "ainda", "nesta", "sobre",
                                  "parte", "total", "sido", "desde", "agora", "nota",
                                  "vai", "pode", "informou", "havia", "outros", "diz", "atual",
                                  "junto", "feitos", "grandes", "menos", "quase", "novos", "porque",
                                  "apenas", "ativar", "acesso", "abril", "pediu", "acrescentou", "pessoas",
                                  "blocos", "apoio", "financiar", "quase", "anos", "afirma", "desta", 
                                  "capacidade", "pequenas", "maior", "sob", "segunda", "trabalho", "cargo",
                                  "outras", "outro", "abaixo", "alta", "todo", "contra", "primeira", "fazer",
                                  "cidade", "marco", "onde", "principais", "equipes", "outra", "duas", "nada",
                                  "feita", "destacou", "feira", "partir", "forma", "importante",
                                  "privado", "longo", "exemplo", "equipe", "aumento",
                                  "podem", "novo", "conforme", "entanto", "maiores", "medida", "garantir",
                                  "final", "atualmente", "neste", "plano", "conta", "deste", "primeiro",
                                  "assim", "deve", "valores", "modelo", "primeiro", "vez", "quanto", "feito",
                                  "brasileira", "disso", "meio", "disso", "qualquer", "todas", "estado", "valor", "projetos"
                                  ))
# removendo pontuacao
cleaned <- removePunctuation(cleaned)
# removendo espacos no comeco e final de cada titulo
cleaned <- str_trim(cleaned)


##### Pré-processar os textos para nuvens de palavras (2/2)
# convertendo o vetor dos titulos em corpus
cleaned_corpus <- Corpus(VectorSource(cleaned))
# vaporizando cada palavra em cada titulo
# cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)
doc_object <- TermDocumentMatrix(cleaned_corpus)
doc_matrix <- as.matrix(doc_object)
# contando aparicoes de cada palavra
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)
# filtrando quaisquer palavras que nao contenham letras 
counts <- counts[grepl("^[a-z]+$", names(counts))]
# criando quadro de dados a partir de informacoes de frequencia de palavras
frame_counts <- data.frame(word = names(counts), freq = counts)


##### Criar a nuvem de palavras
wordcloud(words = frame_counts$word,
          freq = frame_counts$freq,
          scale=c(4, .2),
          min.freq = 4,
          max.words=100, random.order=FALSE,
          rot.per=0,
          colors=brewer.pal(8, "Dark2"))

##### Extrair todas as entidades dos textos

entities <- spacy_extract_entity(unlist(data))
head(entities)
tail(entities)

##### Criar lista de adjacencias

# Criando funcao auxiliar para dar suporte da lista de adjacencias

get_adjacent_list <- function(edge_list) {
  if(length(edge_list)>2)
    adjacent_matrix <- combinations(
      length(edge_list), 2, edge_list)
  #return(adjacent_matrix)
}

adjacent_matrix <- edges %>%
  lapply(get_adjacent_list) %>%
  reduce(rbind)


##### Criar objeto grafo que será exportado para o Gephi
df <- as_tibble(adjacent_matrix, colnames=c('source', 'target'))
weighted_edgelist <- df %>%
  group_by(V1, V2) %>%
  summarise(weight=n())
news_graph <- weighted_edgelist %>% graph_from_data_frame(directed=F)
write_graph(news_graph, 'news_graph.graphml', 'graphml')

install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = frame_counts)




