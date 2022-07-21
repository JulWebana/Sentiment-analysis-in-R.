# Nous utiliserons le package tidytext qui comprend des lexiques de sentiments présents dans le jeu
# de données 'sentiments'.

library(tidytext)
sentiments

# Nous ferons usage de trois lexiques d'usage général tels que : AFINN, bing, loughran.

# Ces trois lexiques utilisent les unigrammes. Les unigrammes sont un type de modèle n-gram qui se 
# compose d'une séquence de 1 élément, c'est-à-dire un mot collecté à partir de données textuelles 
# données. Dans le modèle de lexique de l'AFINN, les mots sont notés dans une fourchette de -5 à 5. 
# L'augmentation de la négativité correspond au sentiment négatif tandis qu'une augmentation de la 
# positivité correspond au sentiment positif. Le modèle lexical Bing, quant à lui, classe le 
# sentiment dans une catégorie binaire de négatif ou de positif. Et enfin, le modèle loughran qui 
# effectue une analyse des rapports d'actionnaires. 

# On utilisera les lexiques de Bing pour extraire les sentiments de nos données. Nous pouvons
# récupérer ces lexiques en utilisant la fonction get_sentiments()


get_sentiments("bing")


# A cette étape, nous allons importer nos bibliothèques 'janeaustenr', 'stringr' ainsi que 'tidytext'. 
# Le package janeaustenr nous fournira les données textuelles sous la forme de livres écrits par la 
# romancière Jane Austen. Tidytext nous permettra d'effectuer une analyse textuelle efficace sur nos
# données. On convertira le texte de nos livres dans un format tidy en utilisant la fonction 
# unnest_tokens().

library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)

tidy_data <- austen_books() %>%
  group_by(book) %>% 
  mutate (linenumber = row_number(),
          chapter = cumsum (str_detect(text, regex ('^chapter [\\divxlc]',
                                              ignore_case = TRUE)))) %>%
  
  ungroup() %>%
  unnest_tokens(word,text)


# Nous avons effectué l'opération tidy sur notre texte de sorte que chaque ligne contient un seul mot.
# On va maintenant utiliser le lexique "bing" et implémenter filter() sur les mots qui correspondent
# à la joie et utiliser le livre Sense and Sensibility et par la suite dériver ses mots pour 
# implémenter notre modèle d'analyse des sentiments.


positive_sentiment <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_sentiment) %>%
  count(word, sort = TRUE)


# Dans le résultat ci-dessus, nous observons de nombreux mots positifs comme “good”, “happy”, “love",
# etc. Dans l'étape suivante, nous allons utiliser la fonction spread() pour séparer nos données en
# colonnes distinctes de sentiments positifs et négatifs. Nous utiliserons ensuite la fonction mutate()
# pour calculer le sentiment total, c'est-à-dire la différence entre les sentiments positifs et 
# négatifs.


library(tidyr)
bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


# Dans la prochaine étape, nous allons visualiser les mots présents dans le livre "Emma" en fonction
# de leurs notes positives et négatives correspondantes.


library(ggplot2)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Procédons maintenant au comptage des mots positifs et négatifs les plus courants présents dans 
# le roman.


counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

head(counting_words)


# Effectuons maintenant la visualisation de notre score de sentiment. On va tracer les scores le 
# long de l'axe qui est étiqueté avec les mots positifs et négatifs et on utilisera la
# fonction ggplot() pour visualiser nos données en fonction de leurs scores.


counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

# Dans la visualisation finale, nous allons créer un nuage de mots qui délimitera les mots positifs
# et négatifs les plus récurrents. On utilisera la fonction comparision.cloud() pour tracer les mots
# négatifs et positifs dans un seul nuage de mots.

library(reshape2)
library(wordcloud)

tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)

# Ce nuage de mots nous permet de visualiser efficacement les groupes de données négatifs et positifs.
# Nous sommes maintenant en mesure de voir les différents groupes de données en fonction de leurs 
# sentiments correspondants. 
