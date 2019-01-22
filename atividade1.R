# Execute e análise o programa.

# Defina  a  area  de  trabalho para a pasta que
# armazena o arquivo: MICRODADOS_ENEM_ESCOLA.csv

#setwd()


dados <-read.csv2("MICRODADOS_ENEM_ESCOLA.csv", header = T, dec=".")

dados <- dados [ , c("NU_ANO",
                     "SG_UF_ESCOLA", 
                     "CO_ESCOLA_EDUCACENSO",
                     "NO_ESCOLA_EDUCACENSO",
                     "TP_DEPENDENCIA_ADM_ESCOLA",
                     "NU_MEDIA_CN",
                     "NU_MEDIA_CH",
                     "NU_MEDIA_LP",
                     "NU_MEDIA_MT",
                     "NU_MEDIA_RED") ]

# TP_DEPENDENCIA_ADM_ESCOLAR
# 1 - FEDERAL
# 2 - ESTADUAL
# 3 - MUNICIPAL
# 4 - PRIVADA

#SELECIONAR ANOS DE 2009 A 2015

dados <- dados [ dados$NU_ANO >= "2009", ]

# CALCULAR AS ESTATÍSTICAS BÁSICAS DE TODAS 
# AS NOTAS.

summary (dados[ , c("NU_MEDIA_CN",
                   "NU_MEDIA_CH",
                   "NU_MEDIA_LP",
                   "NU_MEDIA_MT",
                   "NU_MEDIA_RED") ])



# CALCULAR A MÉDIA DAS NOTAS POR ESCOLA. 

# NE - NOTAS POR ESCOLA

NE <- aggregate(dados [ , c(6:10)], dados[,c(2:5)], mean, na.rm=T )

# FAZER HISTOGRAM DAS NOTAS MÉDIAS DE MATEMÁTICA.

hist (NE$NU_MEDIA_MT, 
      col = "tomato4",
      main = "Média de matemática",
      ylab = "N",
      xlab = "Média", labels=T,
      ylim = c(0,15000))
 

# FAZER UM BOXPLOT DAS NOTAS MÉDIAS DE MATEMÁTICA
# POR TIPO DE ADM. ESCOLAR.

boxplot (NE$NU_MEDIA_MT ~ NE$TP_DEPENDENCIA_ADM_ESCOLA,
         main="Média de matemática",
         xlab="Tipo de escola. ")

# FAZER UMA TABELA ONDE SE CONTA O TIPO DE DEPEN-
# DENCIA ADM ESCOLAR.

table(NE$TP_DEPENDENCIA_ADM_ESCOLA)

