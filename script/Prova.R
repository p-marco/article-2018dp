df <- select(df, "content")
txt <- as.character(df)
x <- udpipe_annotate(udmodel_italian, x = txt, doc_id = seq_along(txt))
library(data.table)
x <- as.data.table(x)
x <- subset(x, upos!="PUNCT")

x <- x[, pos_sequence := txt_nextgram(x = upos, n = 2), by = list(doc_id, sentence_id)]
# tail(sort(table(x$pos_sequence)))

# df.out <- as.data.table(x$pos_sequence, sep=",")

df.out <- as.data.table(sort(table(x$pos_sequence)), sep=",")




df.out <- df.out %>% separate(col = V1, into = c("A", "B"), sep = " ")

data.table::fwrite(df.out, file = "data/out/pos.csv", sep = ",")

df.noun.begin  <- subset(df.out, A=='NOUN')
df.noun.end  <- subset(df.out, B=='NOUN')


data.table::fwrite(df.noun.begin, file = "data/out/pos.noun.begin.csv", sep = ",")
data.table::fwrite(df.noun.end, file = "data/out/pos.noun.end.csv", sep = ",")

barplot(df.noun.begin$N, main="Noun+POS", names.arg=df.noun.begin$B)
barplot(df.noun.end$N, main="POS+Noun", names.arg=df.noun.end$A)








df.noun.begin.clean  <- df.noun.begin[order(df.noun.begin$B), ]
df.noun.end.clean  <- df.noun.begin[order(df.noun.end$A), ]






comparison <- data.frame(df.noun.begin.clean$B, df.noun.begin.clean$N,df.noun.end.clean$N) 
comparison <- table(df.noun.begin.clean$N,df.noun.end.clean$N) 
comparison[order(comparison$A), ]



mydata = t(comparison)
mydata = data.frame(mydata)
rownames(mydata)<- c()
colnames(mydata)<- c()

> comparison <- data.table(df.noun.begin.clean$N,df.noun.end.clean$N) 
> barplot(t(as.matrix(comparison)), beside=TRUE)
