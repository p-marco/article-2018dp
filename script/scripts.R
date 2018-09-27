df <- read.csv('data/sentences.csv', header = T, stringsAsFactors = F)
df.czechit.ces <- read.csv('data/partials/CZECH-IT.ces.csv', header = T, stringsAsFactors = F)
df.czechit.slk <- read.csv('data/partials/CZECH-IT.slk.csv', header = T, stringsAsFactors = F)

df.merlin.ces <- read.csv('data/partials/MERLIN.ces.csv', header = T, stringsAsFactors = F)
df.merlin.slk <- read.csv('data/partials/MERLIN.slk.csv', header = T, stringsAsFactors = F)
df.valico.ces <- read.csv('data/partials/VALICO.ces.csv', header = T, stringsAsFactors = F)
df.valico.slk <- read.csv('data/partials/VALICO.slk.csv', header = T, stringsAsFactors = F)


cleanCorpus <- function(dataname){
df <- select(dataname, "content")
txt <- as.character(df)
x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
x  <- as.data.frame(x)
# Remove punctuation tokens
x.clean = subset(x, upos!="PUNCT")
# Print the output
	# SHAME 1 : Needed to put out the count from the function
	# SHAME 2 : The output is reduced by 1 which represent the token of the header ('content')  
as.numeric(sum(nrow(x.clean) -1))
}


corpora <- c('Czech-IT', 'Merlin', 'Valico')
texts.ces <- c(toString(nrow(df.czechit.ces)), toString(nrow(df.merlin.ces)), toString(nrow(df.valico.ces)))
texts.slk <- c(toString(nrow(df.czechit.slk)), toString(nrow(df.merlin.slk)), toString(nrow(df.valico.slk)))
tokens.ces <- c(as.numeric(cleanCorpus(df.czechit.ces)), toString(cleanCorpus(df.merlin.ces)), toString(cleanCorpus(df.valico.ces)))
tokens.slk <- c(toString(cleanCorpus(df.czechit.slk)), toString(cleanCorpus(df.merlin.slk)), toString(cleanCorpus(df.valico.slk)))
collection <- data.frame(corpora, texts.ces, texts.slk, tokens.ces, tokens.slk)





df <- select(df, "content")
txt <- as.character(df)
x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
x <- as.data.table(x)
x <- subset(x, upos!="PUNCT")

x <- x[, pos_sequence := txt_nextgram(x = upos, n = 2), by = list(doc_id, sentence_id)]
# tail(sort(table(x$pos_sequence)))

# df.out <- as.data.table(x$pos_sequence, sep=",")

df.out <- as.data.table(sort(table(x$pos_sequence)), sep=",")




df.out <- df.out %>% separate(col = V1, into = c("A", "B"), sep = " ")

#data.table::fwrite(df.out, file = "data/out/pos.csv", sep = ",")

df.noun.begin  <- subset(df.out, A=='NOUN')
df.noun.end  <- subset(df.out, B=='NOUN')


data.table::fwrite(df.noun.begin, file = "data/out/pos.noun.begin.csv", sep = ",")
data.table::fwrite(df.noun.end, file = "data/out/pos.noun.end.csv", sep = ",")

#barplot(df.noun.begin$N, main="Noun+POS", names.arg=df.noun.begin$B)
#barplot(df.noun.end$N, main="POS+Noun", names.arg=df.noun.end$A)

df.noun.begin.clean  <- df.noun.begin[order(df.noun.begin$B), ]
df.noun.end.clean  <- df.noun.end[order(df.noun.end$A), ]

comparison <- data.frame(df.noun.end.clean$N, df.noun.begin.clean$N) 
barplot(t(as.matrix(comparison)), beside=TRUE, names.arg = c(df.noun.begin.clean$B), log="y", las=2, legend=c('Pre Noun', 'Post Noun'), args.legend = list(x="topright"))


table.c <- data.frame(df.noun.begin.clean$B, df.noun.begin.clean$N,df.noun.end.clean$N) 
table.c <- as.data.table(table.c)

data.table::fwrite(table.c, file = "data/out/pos.comparison.csv", sep = ",")





```{r graphCorpora, echo=FALSE, fig.width=7, fig.height=4, message=FALSE, fig.cap='Number of texts by different Corpora'}
ggplot(df, aes(fill=df$L1, y=abs(rnorm(nrow(df))), x=df$Corpus)) + 
	geom_bar( stat="identity") + labs(x="Corpora", y="Number of texts in the collection", color="Native Language")
```
