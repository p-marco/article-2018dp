library(dplyr)
library(ggplot2)
library(udpipe)

# Define dataframe
# 
# df <- read.csv('../data/caseStudyDP.csv', header = T, stringsAsFactors = F)
df <- read.csv('../data/sentences.csv', header = T, stringsAsFactors = F)

#during first time model download execute the below line too
# We do need to execute the next code before running udmodel_english
#model <- udpipe_download_model(language = "italian")



udmodel_italian <- udpipe_load_model(file = "italian-ud-2.0-170801.udpipe")

df <- select(df, "content")
txt <- as.character(df)
s <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))

x <- data.frame(s)
#head(x)

write.table(x, "dataframe.txt", sep="\t") 


### STATS

y <- document_term_frequencies(x[, c("doc_id", "token")])
y <- document_term_frequencies(x[, c("doc_id", "lemma")])
write.table(y, "datastats.txt", sep="\t") 


### PLOTS

library(lattice)



stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")
	

## NOUNS
statsN <- subset(x, upos %in% c("NOUN")) 
statsN <- txt_freq(statsN$token)
statsN$key <- factor(statsN$key, levels = rev(statsN$key))
barchart(key ~ freq, data = head(statsN, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


## NOUNS LEMMA
statsNL <- subset(x, upos %in% c("NOUN")) 
statsNL <- txt_freq(statsNL$lemma)
statsNL$key <- factor(statsNL$key, levels = rev(statsNL$key))
barchart(key ~ freq, data = head(statsNL, 20),  
         main = "Most occurring nouns lemma", xlab = "Freq")



## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
statsNPVP <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
statsNPVP <- subset(statsNPVP, ngram > 1 & freq > 3)
statsNPVP$key <- factor(statsNPVP$keyword, levels = rev(statsNPVP$keyword))
barchart(key ~ freq, data = head(statsNPVP, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")



## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
statsNPVP <- keywords_phrases(x = x$phrase_tag, term = tolower(x$lemma), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
statsNPVP <- subset(statsNPVP, ngram > 1 & freq > 3)
statsNPVP$key <- factor(statsNPVP$keyword, levels = rev(statsNPVP$keyword))
barchart(key ~ freq, data = head(statsNPVP, 20), col = "grey", 
         main = "Keywords - simple noun phrases LEMMA", xlab = "Frequency")



  