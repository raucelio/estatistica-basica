library(rvest)
library(stringi)
pag="http://www.scielo.br/scielo.php?script=sci_issuetoc&pid=0102-690920190001&lng=pt&nrm=iso"

web <- read_html(pag)

teste <- html_nodes(web,"a, dt, font")
teste2 <-unlist(strsplit(as.character(teste),split = "<img"))

teste3 <- teste2[grep(".pdf",teste2)]

teste4 <- unlist(strsplit(as.character(teste3),split = "src="))
teste5 <- teste4[grep("href",teste4)]

teste5<-(gsub("<a href=\"","",teste5))
teste5<-(gsub("\">","",teste5))


teste6 <- paste("http://www.scielo.br",teste5,sep="")

for (i in 1:length(teste6))
{
  download.file(teste6[i], destfile = paste("arq",i,".pdf",sep=""))
}


