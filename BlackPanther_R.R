setwd("C:/Users/ukton/Documents/Masters/MIT 5702/R Exercises")

library(lattice)
library(igraph)
library(tm)
library(wordcloud)
library(wordcloud2)
library(devtools)
library(syuzhet)
library(ggplot2)
install.packages("arules")
library(qplot)
library(topicmodels)
library(arules)
edges = read.csv("BlackPantherEdges.csv")
View(edges)
vertice1 = read.csv("BlackPantherVertices.csv")
View(vertice1)


edgeGraph = graph_from_data_frame(edges, directed = T)
plot(simplify(edgeGraph), main = "Simple Edge Plot")
edgeGraph2 = graph_from_data_frame(edges, directed=TRUE, vertices=vertice1)
edge2 = simplify(edgeGraph2)
plot(simplify(edgeGraph2))
plot(edge2, layout=layout.davidson.harel(edge2), vertex.shape='sphere', 
     vertex.color='white', 
     vertex.label.color='black', vertex.label.cex=0.75, vertex.label=V(edge2)$vertice1,
     main = "Layout Davidson Harel")

graph.density(edge2)
reciprocity(edge2, ignore.loops = TRUE)
degree(edge2)
closeness(edge2)
betweenness(edge2)
evcent(edge2)
evcent(edge2)$vector

assortativity(edge2, vertice1$Language, directed=TRUE)
assortativity(edge2, vertice1$Time.Zone, directed = TRUE)
assortativity(edge2, vertice1$Followers, directed = TRUE)

cluster_walktrap(edge2)
cluster_infomap(edge2)

#Created a custom searchreplace function
SearchReplace=content_transformer(function(x,pattern1, pattern2) gsub(pattern1,pattern2,x))
tweet = read.csv("BlackPantherTweets.csv", header = T)
tweetcorp = Corpus(VectorSource(tweet$Tweets))

#To clean and convert entire tweets to lowercase
tweetcorp = tm_map(tweetcorp, content_transformer(tolower))

#Converting to document matrix and removing punctuation and numbers
tweetDTM = DocumentTermMatrix(tweetcorp, control = list(removePunctuation = T, removeNumbers = T, weighting = weightTfIdf))

#Remove common words and twitter handles
tweetcorp = tm_map(tweetcorp, removeWords, c("https", "and", "because", "some", "just", "the", "for", "are", "this","got", "that", "again", "with", "aldenrichards", "aldubnation", "amypot","arabia", "bismonte","borjeoyam", "cruz", "danielleketh", "elmer", "islabarbaraisa", "jaysondmx", "maia", "maidengraffix", "mainedcm", "maineden", "ofctrendsetter", "rcruz", "rom", "shela", "sherriemvl", "suzette", "admc", "aldenrichards", "amypot", "aicraglime", "danielleketh"))
tweetcorp = tm_map(tweetcorp, removeWords, c("aldenrichards", "admc", "danielleketh", "rcruz", "sherriemvl", "elmer", "maia", "maineden"))

tweetDTM = DocumentTermMatrix(tweetcorp, control = list(removePunctuation = T, removeNumbers = T))

#Removing Stopwords, punctuations and white space
tweetcorp = tm_map(tweetcorp, removeWords, stopwords("english"))
tweetcorp=tm_map(tweetcorp, removePunctuation)
tweetcorp=tm_map(tweetcorp, removeNumbers)
tweetcorp=tm_map(tweetcorp, stripWhitespace)

#Running search and replace, we had to search and repalce some twitter handles since R refused to remove them using removewords
tweetcorp = tm_map(tweetcorp, SearchReplace, "theblackpanther", "the black panther")
tweetcorp = tm_map(tweetcorp, SearchReplace, "blackpanther", "black panther")
tweetcorp = tm_map(tweetcorp, SearchReplace, "chadwickboseman", "chadwick boseman")
tweetcorp = tm_map(tweetcorp, SearchReplace, "becauseofthem", "because of them")
tweetcorp = tm_map(tweetcorp, SearchReplace, "wakandaforever", "wakanda forever")
tweetcorp = tm_map(tweetcorp, SearchReplace, "superheroes", "superhero")
tweetcorp = tm_map(tweetcorp, SearchReplace, "albums", "album")
tweetcorp = tm_map(tweetcorp, SearchReplace, "aldenrichards", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "admc", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "elmer", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "danielleketh", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "sherriemvl", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "shela", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "maineden", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "maia", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "rcruz", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "rom", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "xiuhanforexo", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "amypot", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "bismonte", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "cruz", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "suzette", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "kpopqween", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "bretmanrock", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "yihougan", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "iloveparkjimln", "")
tweetcorp = tm_map(tweetcorp, SearchReplace, "tcoyogsueoml", "")

tweetDTM = DocumentTermMatrix(tweetcorp, control = list(removePunctuation = T, removeNumbers = T))


#Refining the wordcloud and making it prettier
tweetTerms=colSums(as.matrix(tweetDTM))
tweetTerms=sort(tweetTerms, decreasing=T)
wordcloud(words=names(tweetTerms), freq=tweetTerms, max.words=300, random.order= F, random.color=F, colors=brewer.pal(8, 'Dark2'))


#Since the words black and panther were so large in frequency,
#we decided to filter them out to get a better wordcloud
tweetcorp2 = tm_map(tweetcorp, SearchReplace, "black", "")
tweetcorp2 = tm_map(tweetcorp2, SearchReplace, "panther", "")
tweetDTM2 = DocumentTermMatrix(tweetcorp2, control = list(removePunctuation = T, removeNumbers = T))
findFreqTerms(tweetDTM2, 30)

tweetTerms2=colSums(as.matrix(tweetDTM2))
tweetTerms2=sort(tweetTerms2, decreasing=T)
wordcloud(words=names(tweetTerms2), freq=tweetTerms2, max.words=150, random.order= F, random.color=F, colors=brewer.pal(8, 'Dark2'))


#Running the sentiment analysis on the dataset
tweetDF=data.frame(text=sapply(tweetcorp, as.character), stringsAsFactors = FALSE)
tweetDF
tweetPolarity=data.frame(Syuzhet=cbind(get_sentiment(as.character(tweetDF$text),method='syuzhet')))
View(tweetPolarity)

#Creating a new coloumn in the Syuzhet dataframe
tweetPolarity$Tone = "Neutral"

for (i in (1:7531))
{
  
  if(tweetPolarity[i,] > 0)
  {
    tweetPolarity$Tone[i] = "Positive"
  }
  
  else if(tweetPolarity[i,] < 0)
  {
    tweetPolarity$Tone[i] = "Negative"
  }
}

#Histogram for our Sentiment Analysis
attach(tweetPolarity)
hist(Tone, col = "green")
ggplot(tweetPolarity, aes(Tone)) + 
  geom_bar(aes(fill = Tone)) +
  ggtitle("Sentiment Histogram") +
  theme(plot.title = element_text(hjust = 0.5))

#Here we decided to combine the words black and panther back tto form black panther
#that way we could create the word association for black panther
tweetcorp = tm_map(tweetcorp, SearchReplace, "black panther", "blackpanther")
tweetTDM=removeSparseTerms(TermDocumentMatrix(tweetcorp),.975)
findAssocs(tweetTDM, 'blackpanther', 0.05)

#We then graphed the clusters for our tweets to help see the word associations
tweetDist = dist(tweetTDM,method='euclidean')
tweetClust=hclust(d=tweetDist, method='ward.D')
plot(tweetClust)
rect.hclust(tweetClust, k=6, border='red2')
tweetTopics=cutree(tweetClust,k=4)
tweetTopics
tweetKClust=kmeans(tweetDist, 6)
tweetKClust$cluster 
tweetKClust$centers

tweetCooccurMatrix=as.matrix(tweetTDM)%*%t(as.matrix(tweetTDM))
tweetGraph=simplify(graph.adjacency(tweetCooccurMatrix, weighted=T, mode='undirected'))
V(tweetGraph)$color='yellow'
plot(tweetGraph, layout=layout.gem(tweetGraph), vertex.shape='sphere')

evcent(tweetGraph)$vector 			
mc=multilevel.community(tweetGraph)
wc=walktrap.community(tweetGraph)
cbind(mc$names,mc$membership)
plot(mc, tweetGraph, vertex.shape='sphere',layout=layout.davidson.harel, vertex.label.color= 'black')

#We then creating a complete DTM and fing the top ten words associated with each topic
tweetDTM=DocumentTermMatrix(tweetcorp)
tweetTopics=LDA(tweetDTM, method='Gibbs', k=6, control=list(seed = 77))
terms(tweetTopics,10)
#data(tweetTopics)
#View(tweetTopics)
#tweetTopics = as.data.frame(tweetTopics)
output = Corpus(tweetTopics)
write.csv(tweetTopics, file = "tweetTopics.csv")
?Corpus
