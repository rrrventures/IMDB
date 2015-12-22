##Scrapeo IMDB

library(XML)
library(lubridate)
library(ggplot2)
library(RCurl)
library(plyr)

##URL root

urlroot<-"http://www.imdb.com/search/title?num_votes=5000,&sort=user_rating,desc&title_type=tv_series"

#GetURL

urlrootbruta<-getURL(urlroot)
root_parsed<-htmlParse(urlrootbruta)

#Sacar links de las series y nombres

links<-xpathSApply(root_parsed,"//td[@class='title']/a/@href")
ratings<-xpathSApply(root_parsed,"//span[@class='value']",xmlValue)
nombres<-xpathSApply(root_parsed,"//td[@class='title']/a",xmlValue)

#Generar paginas de las series

links.series<-paste("www.imdb.com",links,sep="")


#Organizar y parsear paginas de las series

seriesbruta<-list()

for (i in 1:length(links.series)){
seriesbruta[i]<-getURL(links.series[i])
}

series_parsed<-list()
for (i in 1:length(links.series)){
series_parsed[[i]]<-htmlParse(seriesbruta[[i]])
}


#Sacar numero de temporadas

lista<-llply(series_parsed,function(t) xpathSApply(t,"//div[@class='seasons-and-year-nav']/div/a",xmlValue))
ntemporadas<-ldply(lista,function(t) t[1])



#Obtener links de las temporadas en particular y ya me salí de control con los nombres de las variables

pags<-paste("http://www.imdb.com",links,"episodes?season=",sep="")
asdf<-list()

for (i in 1:length(links)){
asdf[[i]]<-paste(pags[i],1:ntemporadas[i,1],"&ref_=tt_eps_sn_",1:ntemporadas[i,1],sep="")
}



#Obtener los links de los caps de cada temporada

asdf2<-llply(asdf,function(t) htmlParse(getURL(t)))

capslinks<-llply(asdf2,function(t) xpathSApply(t,"//div[@class='info']/strong/a/@href"))
capslinks2<-llply(capslinks,function(t) paste("www.imdb.com",t,sep=""))

#Robar el rating de cada capitulo 

capsparsed<-list()

for (i in 1:50){

capsparsed[[i]]<-htmlParse(getURL(capslinks2[[i]]))
Sys.sleep(5)
i
}

capsrating<-llply(capsparsed,function(t) xpathSApply(t,"//div[@class='star-box-details']/strong/span",xmlValue))
### alternativa al for capsparsed<-llply(capslinks2,function(t) htmlParse(getURL(t)))


#Convertir a numéricos y comparar

series_ratings<-as.numeric(ratings)
caps_ratings<-llply(capsrating,function(t) as.numeric(t))

caps_suma<-ldply(caps_ratings,sum)
caps_largo<-ldply(caps_ratings,length)
caps_promedio<-caps_suma/caps_largo

series_ratings-caps_promedio

df<-data.frame(Nombre=nombres,Nota.serie=ratings,Caps.promedio=caps_promedio)
df2<-melt(df,id="Nombre")


##Graficar

ggplot(data=df2,aes(x=Nombre,y=value,colour=variable,group=variable))+geom_line()+theme(axis.text.x = element_text(angle=55,hjust=1))+scale_x_discrete(limits=nombres)






###Esto es para el analisis de proporcion de votos

caps_notas<-list()
caps_votos<-list()

for (i in 1:50){

temp<-lapply(paste("http://",capslinks2[[i]],sep=""),htmlParse)
caps_notas[[i]]<-ldply(temp,function(t) xpathSApply(t,"//div[@class='star-box-details']/strong/span",xmlValue))
caps_votos[[i]]<-ldply(temp,function(t) xpathSApply(t,"//div[@class='star-box-details']/a/@title")[1])
temp<-0
Sys.sleep(20)
print(i)
}

prom_pesos<-0

for (i in 1:50){

prom_pesos[i]<-sum(listatemp[[i]]*(votos_final[[i]]/sum(votos_final[[i]])))
}


listatemp<-list()
for (i in 1:50){

listatemp[[i]]<-as.numeric(caps_notas[[i]][,1])

}