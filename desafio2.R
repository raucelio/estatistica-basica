
pag2 ="http://www.itamaraty.gov.br/pt-BR/discursos-artigos-e-entrevistas-categoria/presidente-da-republica-federativa-do-brasil-discursos"
web <- read_html(pag2, encoding="UTF-8"  )
discursos <- html_nodes(web,"h2 a")
discursos <- html_attr(discursos,"href")
discursos <- paste("http://www.itamaraty.gov.br/",discursos,sep="")
n=0
for ( i in discursos)
{
  n = n +1
  web <- read_html(i)
  teste <- html_nodes(web,"p")
  teste <- html_text(teste)  
  write(teste, paste("discurso",n,".txt",sep=""))
  
}
