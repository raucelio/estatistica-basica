###
### representacao tabular
###


#Tabelas das Notas de Aula

tabela <- function (x)
{
  f   <-table (x)                             # frequencia simples 
  fr  <- round(prop.table(f)*100,2)           # frequencia relativa  
  Fab  <- cumsum(f)                           # frequencia acumulada "abaixo de"
  Fabr <- cumsum(fr)                          # frequencia acumulada "abaixo de" relativa
  
  
  cbind(f,fr,Fab,Fabr)     # montando a tabela, colando as colunas

}

# o rol de dados do n?mero de acertos em uma prova de 
# 10 quest?es aplicadas a 50 alunos


nota<-c(2 , 3 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 4 , 
        4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 ,
        4 , 4 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 
        5 , 5 , 5 , 5 , 5 , 5 , 6 , 6 , 6 , 6 , 
        6 , 6 , 6 , 7 , 7 , 7 , 8 , 8 , 8 , 9 )



print (tabela(nota), digits=3)


# tablea de dados agrupados para as notas dos 50 alu-
# no.

## construindo as classes

faixa <- cut(nota,breaks = seq(0,10,by=2),dig.lab=4)

print (tabela(faixa), digits=3)



## Massa de 140 gr?o de feijao

massa <- 
  c(0.1188	,	0.1590	,	0.1732	,	0.1817	,	0.1917	,	0.1996	,	0.2089	,	0.2200	,	0.2365	,	0.2535	,
    0.1202	,	0.1596	,	0.1733	,	0.1823	,	0.1930	,	0.1996	,	0.2091	,	0.2215	,	0.2369	,	0.2585	,
    0.1340	,	0.1606	,	0.1736	,	0.1826	,	0.1935	,	0.2008	,	0.2096	,	0.2237	,	0.2380	,	0.2593	,
    0.1340	,	0.1629	,	0.1740	,	0.1828	,	0.1950	,	0.2012	,	0.2097	,	0.2254	,	0.2382	,	0.2595	,
    0.1399	,	0.1642	,	0.1746	,	0.1848	,	0.1965	,	0.2025	,	0.2098	,	0.2262	,	0.2423	,	0.2620	,
    0.1402	,	0.1644	,	0.1766	,	0.1860	,	0.1965	,	0.2045	,	0.2126	,	0.2266	,	0.2455	,	0.2657	,
    0.1409	,	0.1663	,	0.1769	,	0.1865	,	0.1968	,	0.2048	,	0.2126	,	0.2285	,	0.2456	,	0.2660	,
    0.1421	,	0.1666	,	0.1773	,	0.1869	,	0.1970	,	0.2051	,	0.2137	,	0.2296	,	0.2458	,	0.2666	,
    0.1462	,	0.1677	,	0.1793	,	0.1874	,	0.1971	,	0.2054	,	0.2143	,	0.2309	,	0.2459	,	0.2673	,
    0.1463	,	0.1683	,	0.1795	,	0.1875	,	0.1980	,	0.2055	,	0.2146	,	0.2311	,	0.2465	,	0.2674	,
    0.1470	,	0.1696	,	0.1795	,	0.1902	,	0.1985	,	0.2058	,	0.2153	,	0.2326	,	0.2482	,	0.2790	,
    0.1496	,	0.1701	,	0.1799	,	0.1904	,	0.1987	,	0.2060	,	0.2173	,	0.2327	,	0.2504	,	0.2833	,
    0.1561	,	0.1708	,	0.1810	,	0.1910	,	0.1988	,	0.2080	,	0.2184	,	0.2335	,	0.2505	,	0.3043	,
    0.1573	,	0.1722	,	0.1814	,	0.1911	,	0.1995	,	0.2087	,	0.2186	,	0.2344	,	0.2507	,	0.3141	)

## numero de dados

n <- length (massa)

## numero de classe
## uma sugestão para o numero de classes é a proximacaoo
## da raiz quadrada dos elementos a serem classificados 

(k <- sqrt(n))

## construindo as classes

faixa <- cut(massa,breaks = seq(0.10,0.32,l=k),dig.lab=4)

print (tabela(faixa), digits=3)


###
### medidas de tendencia central
###

fraude <- c(3,5,4,5,2,5,6,3,4,4,3,4,3,5,5,6,
            2,2,3,2,3,6,4,3,1,8,5,3,3,2,3)

sum(fraude)
length(fraude)
mean(fraude)

sort(fraude)


###
### medidas de dispersao
###

# Amplitude
dados <- c( 149 , 150 , 152 , 154 ,  155, 157,  157,  157,  158,  158,
            158 , 159 , 160 , 162 ,  162, 163,  163,  163,  164,  164, 
            164 , 165 , 165 , 165 ,  165, 166,  166,  166,  168,  169,
            169 , 170 , 170 , 170 ,  171, 172,  172,  175,  176,  178)


range(dados)



###
### Graficos
###

#grafico capa
old <- par()
x <- seq(-3,3, by=0.1)
jpeg("capa3.jpeg",bg="transparent")
par(bg="red")
plot (x, dnorm(x), type="h", lwd=2, col="white", axes=FALSE , xlab="", ylab="", bty="n")
dev.off()

# Altura dos homens

par(mar=c(2, 4, 4, 2) + 0.1)

altura <- c(1.47	,	1.48	,	1.51	,	1.53	,	1.55	,	1.55	,	1.59	,	1.60	,	1.60	,	1.62	,	1.62,
            1.63	,	1.63	,	1.64	,	1.65	,	1.65	,	1.68	,	1.68	,	1.68	,	1.69	,	1.69	,	1.69,
            1.70	,	1.70	,	1.71	,	1.71	,	1.71	,	1.72	,	1.72	,	1.72	,	1.73	,	1.75	,	1.76,
            1.76	,	1.76	,	1.77	,	1.77	,	1.78	,	1.78	,	1.79	,	1.79	,	1.79	,	1.80	,	1.81,
            1.82	,	1.82	,	1.83	,	1.85	,	1.85	,	1.86	,	1.89	,	1.90	,	1.92	,	1.93	,	1.99	)

limites <- seq(1.45,2.01,by=0.08)

faixa <- cut(altura,breaks = seq(1.45,2.01,by=0.08),dig.lab=4, right = FALSE)

print (tabela(faixa), digits=3)




# histograma frequencia simples
jpeg("gr_hist1.jpeg", bg="transparent")
a <- hist(altura,breaks=limites, main="",
                            xlab = "Altura", 
                            ylab = "N", 
                            labels =TRUE, 
                            axes   =FALSE,
                            right = FALSE)
axis(1, at=limites, cex=0.8)
axis(2)
dev.off()



# histograma frequencia acumulada
aux <- rep(a$mids,a$counts)
aux <- cumsum(table(aux))
aux <- rep(a$mids,aux)

jpeg("gr_hist2.jpeg", bg="transparent")
a<-hist(aux,breaks=limites, main="",
          xlab = "Altura", 
          ylab = "N", 
          labels =TRUE, 
          axes   =FALSE, ylim=c(0,60),
          right = FALSE)
axis(1, at=limites, cex=0.8)
axis(2)
dev.off()

# ploigono de frequencia

jpeg("gr_hist3.jpeg", bg="transparent")
a <- hist(altura,breaks=limites, main="",
          xlab = "Altura", 
          ylab = "N", 
          labels =TRUE, 
          axes   =FALSE,
          right = FALSE)
axis(1, at=limites, cex=0.8)
axis(2)
lines(c(min(limites),a$mids,max(limites)), c(0,a$counts,0), col="red",lwd=2)
dev.off()

# ogiva de Galton

jpeg("gr_hist4.jpeg", bg="transparent")
aux <- rep(a$mids,a$counts)
aux <- cumsum(table(aux))
aux <- rep(a$mids,aux)

a<-hist(aux,breaks=limites, main="",
        xlab = "Altura", 
        ylab = "N", 
        labels =TRUE, 
        axes   =FALSE, ylim=c(0,60),
        right = FALSE)
axis(1, at=limites, cex=0.8)
axis(2)
lines(c(min(limites),a$mids,max(limites)), c(0,a$counts,max(a$counts)), col="red",lwd=2)
dev.off()


# grafico de hastes

dados <- table(rbinom(1000,10,0.5))

jpeg("gr_haste1.jpeg", bg="transparent")
plot (dados, type="h", col = "red", lwd = 2, xlab = "X", ylab = "Y")
dev.off()


# grafico dispersao

library(MASS)

jpeg("gr_dispersao1.jpeg", bg="transparent")
plot (cats$Bwt, cats$Hwt, pch=16 , col = "black", lwd = 2, xlab = "X", ylab = "Y")
dev.off()

# grafico boxplot

library(MASS)


jpeg("gr_boxplot1.jpeg", bg="transparent")
a<-boxplot (iris$Sepal.Width, pch=16 ,  lwd = 2, xlab = "X")
text (1.4,c(a$stats, a$out), c( "min", "1º Q", "mediana","3º Q", "max", "out", "out", "out", "out"), cex = 0.8)
arrows (1.3, c(a$stats, a$out), 1.22,c(a$stats, a$out) )
dev.off()



# grafico boxplot

library(MASS)

a <- as.data.frame(Titanic)

jpeg("gr_barra1.jpeg", bg="transparent")
barplot(table(cats$Sex),ylim=c(0,100), col="white")
dev.off()






## Grafico do Apendice R



## Grafico do Apendice R

jpeg("figa2.jpeg",bg="transparent")
x <- iris[,"Sepal.Width"]
y <- iris[,"Petal.Width"]
plot (x, y, type="p", main="PLANTA IRIS", xlab = "Sepal Width", ylab="Petal Width")
dev.off()

jpeg("figa3.jpeg",bg="transparent")
x <- iris[,"Sepal.Width"]
y <- iris[,"Petal.Width"]
especie <- iris[,"Species"]
plot (x, y, type="p", main="PLANTA IRIS", xlab = "Sepal Width", ylab="Petal Width")
text(x,y,labels = especie, cex=0.5,adj=c(0,-1))
dev.off()

x <- iris[,"Sepal.Width"]
y <- iris[,"Petal.Width"]
especie <- iris[,"Species"]

jpeg("figa4.jpeg",bg="transparent")
plot (x, y, type="p", main="PLANTA IRIS", xlab = "Sepal Width", ylab="Petal Width")
points(x[1:50], y[1:50], col=2, cex=.8,pch=19)
points(x[51:100], y[51:100], col=3, cex=.8,pch=1)
points(x[101:150], y[101:150], col=4, cex=.8,pch=19)
legend(3.5,1.5, c("setosa","versicolor","virginica"), 
       pch=c(19,1,19),col=c(2,3,4), cex=0.8)
dev.off()

X <- rep(1:5,5)
Y <- rep (5:1,rep(5,5))
jpeg("figa5.jpeg",bg="transparent")
par(mar=c(0,0,0,0))
plot(X,Y, pch=1:25,xlim=c(0,6), ylim=c(0,6),
     xlab = "", ylab = "",axes=FALSE)
text(X,Y, labels = 1:25 , adj = c(1,-1),cex=0.8)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

x<-iris[,"Sepal.Width"]
jpeg("figa6.jpeg",bg="transparent")
plot(x,type="l",xlab = "índice",ylab="Sepal.Width")
dev.off()

quantidade <- table(iris["Species"])
jpeg("figa7.jpeg",bg="transparent")
barplot(quantidade, xlab="Espécie", ylab="Frequencia", col="white")
dev.off()

quantidade <- table(iris["Species"])
jpeg("figa8.jpeg",bg="transparent")
pie(quantidade)
dev.off()

jpeg("figa9.jpeg",bg="transparent")
boxplot(iris[,-5], main="",
     xlab = "",ylab = "")
dev.off()

jpeg("figa10.jpeg",bg="transparent")
hist(iris[,"Petal.Width"], main="Histograma de Petal.Width separado em faixas",
     xlab = "Petal.Width",ylab = "Frequência")
dev.off()
