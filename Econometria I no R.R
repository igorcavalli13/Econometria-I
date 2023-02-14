##Econometria I

##uso básico do R

# <- é a mesma coisa que =

# fazer comparativos (útil no uso de base de dados grandes)
# se tal valor é igual a tal valor
x==4

#diferente
x>4

# somatoria
sum(x)
sum(x*y)

# transformar o vetor numa matriz
x=as.matrix(x)
y=as.matrix(y)

# fazer vetores/matrizes mais simplesmente
x=c(1:5)
y=c(2:6)
x=matrix(1:9,3,3)

# selecionar apenas um elemento do vetor
x[3,3]
x[1:3]
y[c(1,2,3)] #apenas os elementos 1, 2 e 3 de y

# selecionar todos menos um elemento especifico
x[-3]

# sequencias mais gerais
seq(from=1, to=10, by=2) #partida, fim, incremento
seq(2,1342,9)
c=seq(9,18,0.4)
c
plot(c)

seq(from=0,to=1,length.out=500)

# sequencias repetidas
rep(2, times=5)
sample(c(1,2,3,4,5), size = 5, replace = FALSE)
sample(c(1,2,3,4,5), size = 5, replace = T)
x=rnorm(1000,0,1)
x

#criar uma sequencia com distribuição normal
set.seed(123)
x=rnorm(1000000,0,1)
mean(X)
hist(x)

#tirar uma amostra de x
y=sample(x,100)
mean(y)

#gerar sequencias de numeros aleatorios
#100 numeros aleatoriamente distribuidos entre -1 e 1
x=runif(100,min=-1,max=1)
#seq de numeros pseudoaleatorios normalmente distribuidos com mean=0 e sd=1
rnorm(10,mean=0,sd=1)

###########
#regressao#
###########

n=10
x=runif(n)
b=c(2,3)
x
b
u=rnorm(n,0,1)
u
x=cbind(1,x) #concatenar colunas
x
y=x%*%b+u
y

solve(t(x)%*%x)%*%t(x)%*%y

# ver a propriedade de consistência em ação (aumentar n)

n=10000
x=runif(n)
u=rnorm(n,0,1)
x=cbind(1,x)
y=x%*%b+u
solve(t(x)%*%x)%*%t(x)%*%y

#modelo linear
lm(y~x)
lm(y~x-1) #removendo a constante
plot(y~x[,2]) #plotar apenas a 2a coluna
abline(lm(y~x[,2])) #acrescentar uma linha no grafico ja plotado
abline(lm(y~x[,2]),col='pink',lwd=10) #abline com cor rosa e mais grosso

## aula 2

#definir a semente do gerador aleatório
#todo pacote estatístico usa um gerador para 
#numeros aleatorios
#numeros aleatorios = conceito teórico
#ele cria um pseudoaleatório
#ele gera uma sequência de números e o primeiro 
#número que ele vai gerar

#amostra aleatória uniforme
#random uniform n=5
runif(5)

#amostra n=1000
x=runif(1000)
hist(x)

#delimitando de -1 a 1 n=100
x=runif(100, -1, 1)
hist(x)

#normal(n, média, dp)
x=rnorm(1000, 0, 1)
hist(x)
plot(density(x))

#distr t(n, gl) (teste de hip)
x=rt(100000,2)
plot(density(x))

#repetição (o que repetir, vezes)
rep(1,10)

rep('w',4)

rep(c(1:4),3)

# modelo y=xb+U

#tamanho da amostra
n=100

#fixar semente
set.seed(123)

#amostra dos erros normalmente distribuido
u=rnorm(n)

#matriz dos betas populacionais
b=c(2,0.5,-1)

#variaveis explicativas
x1=runif(n,0,10)
x2=runif(n,0,10)
c=1

#matriz dos regressores
#juntar colunas
X=cbind(c,x1,x2)

#Xb
X%*%b


#modelo populacional
y=X%*%b+u

#estimador de MQO
solve(t(X)%*%X)%*%t(X)%*%y

#estimador de modelos lineares
lm(y~X)

#por padrao o r inclui a constante na reg
#duas possibilidades
#dizer que nao quer a constante
lm(y~X-1)
#removera coluna da matriz dos regressores
lm(y~X[,-1])

#outro modelo
#testando a consistência
#quanto mais aumenta o n, mais a amostra
#expressa as caraterísticas da população
#nesse caso, beta estimado chega mais
#perto da matriz de beta criada (pop)
n=100000
u=rnorm(n)
x1=runif(n,0,10)
x2=runif(n,0,10)
X=cbind(c,x1,x2)
y=X%*%b+u
solve(t(X)%*%X)%*%t(X)%*%y

#loop - repetir
#(indexador in qtd de avaliações)
for(i in 1:5){print(i)}
for(i in 1:5){print(2*i)}

#outro modelo
#usando loop
#em cada momento se usa uma amostra de erro
#e isso interfere em todo os modelos criados
n=10
x1=runif(n,0,10)
x2=runif(n,0,10)
X=cbind(c,x1,x2)
for(i in 1:5)
{y = X%*%b +rnorm(n)
print(solve(t(X)%*%X) %*% t(X)%*%y)}

#lugar onde vão ser armazenados os valores dos parâmetros
#e vai ser composta uma matriz
res=NULL

for(i in 1:5)
{y = X%*%b +rnorm(n)
bhat = solve(t(X)%*%X) %*% t(X)%*%y
res = rbind(res, t(bhat))}

#agora o res tem outro conteúdo
res

mean(X[,1])
mean(res[,1])
mean(res[,2])
mean(res[,3])

#esse calculo serve para pegar a média de várias tentativas (loop)
#o tamanho da amostra permanece igual
#essa é a propriedade do enviasamento
#nesse caso tem-se um modelo não enviesado

#teorema central do limite
#se vc pegar uma amostra grande ou varias amostras 
#pequenas, a distrubição da média ou da soma dessas amostrar 
#converge para uma distribuição normal, independentemente da 
#forma da distribuição

#criar uma função quadrada
quad=function(x){
  res=x^2 
  return(res)}

quad(2)
quad(pi)

bhat= function(y,x){
  res=solve(t(x)%*%x)%*%t(x)%*%y
  return(res)}

require(ggplot2)

#dstribuição gamma (distrb assimetrica)
#soma das linhas (transforma a distr assimetrica numa normal)
CLT <- function(n, plot = FALSE){
  N <- 10000
  u <- matrix(rgamma(n*N, 1), N, n)
  z <- rowSums(u)
  z <- (z - mean(z))/sd(z)
  z <- data.frame(z)
  if(plot){
    ggplot(z) +
      geom_histogram(aes(x=z, y=..density..), binwidth = 0.1, fill = "grey", color = "black") +
      geom_density(aes(x=z,y=..density..))
  }
  else{
    return(z)
  }
}


#retomando o modelo
n=10
x1=runif(n,0,10)
x2=runif(n,0,10)
b=c(2,0.5,-1)
X=cbind(1,x1,x2)
head(res)
for(i in 1:1000){
  y=X%*%b+rnorm(n)
  bhat= solve(t(X)%*%X)%*%t(X)%*%y
  res=rbind(res,t(bhat))
}

hist(X[,2])
hist(res[,2])

# u se distribui normalmente, consequentemente
# y se distribui normalmente, consequentemente
# b se distribui normalmente com media em b pop

#estimador da matriz de covariancia
#1a coisa é obter o resíduo

n=30
x1=runif(n,0,10)
x2=rnuif(n,0,10)
X=cbind(1,x1,x2)
u=rnorm(n)
y=X%*%b + u

bhat = solve(t(X)%*%X) %*% t(X)%*%y
bhat

uhat = y - X%*%bhat
uhat

#comparar u com uhat
cbind(u,uhat)

sigma2=t(uhat) %*% uhat
sigma2

sigma2/n-2

sigma2=as.numeric(sigma2)

cov=(sigma2/(n-3))*solve(t(X)%*%X)
cov

#pegar só a diagonal
diag(cov)

#dp
sqrt(diag(cov))

bhat

c(bhat)/sqrt(diag(cov))
summary(lm(y~X-1))



## aula 3

b <- c(2,0.5)
n <- 5
x1 <- runif(n, 0, 10)
X <- cbind(1,x1)
u <- rnorm(n)
y <- X%*%b + u

bhat=solve(t(X)%*%X)%*%t(X)%*%y

#duas maneiras de calcular o uhat

#primeira maneira, usando bhat

uhat=y-X%*%bhat
head(uhat,4)

#segunda maneira, sem usar bhat

A=solve(t(X)%*%X)%*%t(X)

I=diag(n)
M=I-X%*%A
uhat=M%*%y
head(uhat,4)
M=I-X%*%A
uhat=M%*%y

# matriz de variancia e covariancia

sigma2 = t(uhat) %*% uhat
sigma2=as.numeric(sigma2) #transformar o sigma num numero para poder executar a multiplicação seguinte
cov=(sigma2/(n-2))*solve(t(X)%*%X)

#tirar apenas a diagonal principal
diag(cov)

#modelo gerado pelo r para usar de comparação com os valores que virão a seguir
summary(lm(y ~ x1))

#encontrar o dp de cada parametro (2a coluna do lm)
var <- diag(cov)
sd <- sqrt(var)
sd <- sqrt(var) #ou ainda

summary(lm(y ~ x1)) #para ver se os valores acima estão corretos

#teste de hipótese (3a coluna do lm)

tcalc=bhat/sd

tcrit=qt(0.975,df=(n-2)) #0,025 de um lado e 0,025 do outro

# ou em separado
tcalc1=bhat[1]/sqrt(cov[1,1])

tcalc2=bhat[2]/sqrt(cov[2,2])

#verificar se a hipótese nula é rejeitada ou não (4a coluna do lm)
abs(tcalc)>tcrit

2*pt(-abs(tcalc),df=(n-2))

#teorema do limite central
#a medida que vc vai aumentando 
#a amostra a distribuição vai se 
#aproximando da normal

#se vc retirar várias amostrar de pequeno
#tamanho a média das amostrar vai convergir
#cada vez mais para a normal

#novo modelo
#erros com distr gama

tcl <- function(n){
  x1 <- runif(n, 0, 10)
  X <- cbind(1,x1)
  b <- c(2,0.5)
  N <- 10000
  y <- NULL
  model <- function(x){
    X%*%b + rgamma(n, 0.5)
  }
  y <- sapply(1:N, model)
  y <- colMeans(y)
  y <- (y - mean(y))/sd(y)
  hist(y, prob=TRUE,
       breaks='scott')
  lines(density(y), col='blue')
}

tcl(5)
tcl(10)
tcl(100)
tcl(1000) #a amostra vai se aproximando da normal

#poder do teste
#quando o teste tem baixo poder ele tende 
#a rejeitar a hipótese nula quando não deveria

#modelo para resultar na qtd de que???

n <- 30
x1 <- runif(n, 0, 10)
X <- cbind(1,x1)
A <- solve(t(X)%*%X)%*%t(X)
tbs <- NULL
tcrit = qt(0.975, df=(n-2))
r <- 1000
for(i in 1:r){
  u <- rnorm(n)
  y <- X%*%b + u
  bhat <- A%*%y
  uhat <- y - X%*%bhat
  shat <- as.numeric(crossprod(uhat)/(n-2))
  covhat <- shat*solve(t(X)%*%X)
  tbs <- rbind(((bhat[2]-0.5)/sqrt(covhat[2,2])), tbs)
}
(sum(abs(tbs) > tcrit)/r)*100

#resultado roda em torno de 5

n <- 30
x1 <- runif(n, 0, 10)
X <- cbind(1,x1)
A <- solve(t(X)%*%X)%*%t(X)
tbs <- NULL
tcrit = qt(0.975, df=(n-2))
r <- 1000
for(i in 1:r){
  u <- runif(n) #siubstituindo por runif
  y <- X%*%b + u
  bhat <- A%*%y
  uhat <- y - X%*%bhat
  shat <- as.numeric(crossprod(uhat)/(n-2))
  covhat <- shat*solve(t(X)%*%X)
  tbs <- rbind(((bhat[2]-0.5)/sqrt(covhat[2,2])), tbs)
}
(sum(abs(tbs) > tcrit)/r)*100

#o resultado continua rodando em torno de 5

n <- 30
x1 <- runif(n, 0, 10)
X <- cbind(1,x1)
A <- solve(t(X)%*%X)%*%t(X)
tbs <- NULL
tcrit = qt(0.975, df=(n-2))
r <- 10000 #aumentando a qtd de experimentos
for(i in 1:r){
  u <- runif(n) #siubstituindo por runif
  y <- X%*%b + u
  bhat <- A%*%y
  uhat <- y - X%*%bhat
  shat <- as.numeric(crossprod(uhat)/(n-2))
  covhat <- shat*solve(t(X)%*%X)
  tbs <- rbind(((bhat[2]-0.5)/sqrt(covhat[2,2])), tbs)
}
(sum(abs(tbs) > tcrit)/r)*100

#não se aumenta para nada e o resultado 
#se aproxima mais ainda de 5
#isso considerando que o erro não é normal


# coeficiente de gini

#ler base de dados
data = read.csv('Poverty_Ineq_Data.csv', sep = ',', header = T, stringsAsFactors = FALSE)
attach(data)

head(data)

summary(data)

#plotar gráfico para a visualização da relação
#entre a variação na pobreza entre 1960 e 2010
#e a desigualdade na década de 1960

plot(dpov ~ gini1960)
abline(lm(dpov ~ gini1960))

reg1 = lm(dpov ~ gini1960, data)
summary(reg1)

reg2=lm(k1960 ~ h1960, data)
summary(reg2)

plot(reg2)

.libPaths() # get library location
library()   # see all packages installed
search()    # see packages currently loaded

#usando outra base de dados

data = read.csv('PIB-Theil-2000-2010.csv', sep = ',', header = T, stringsAsFactors = FALSE)
attach(data)

head(data)

summary(data)

#criando pib per capita a partir de pib e população

pibpc2000 = pib2000/pop2000
pibpc2010 = pib2010/pop2010

dppib=log(pibpc2010/pibpc2000)

reg3=lm(dppib ~ theil2000)
plot(reg3)

plot(dppib ~ theil2000)

summary(reg3)

#desigualdade negativa
#significa q quanto menor é a desigualdade em 2000
#cresceu mais entre 2000 e 2010

# se ta tudo de acordo
# se violou presuspostos
# problemas com a interpretação

#rquadrado é quase 0, o ajustamento é horrível
#por aí vai

# AULA 4

data = read.csv('PIB-Theil-2000-2010.csv', sep = ',', header = T, stringsAsFactors = FALSE)
attach(data)

head(data) #observar dados

summary(data) #análise dos dados

pibcap00=pib2000/pop2000 #pib per capita de 2000
pibcap10=pib2010/pop2010 #pib per capita de 2010

varpibcap=pibcap10/pibcap00 #variação do pib per capita em 10 anos
plot(varpibcap)
hist(varpibcap)

dpib=log(varpibcap) #logarítmo da variação acima
plot(dpib)
hist(dpib)

reg1=lm(dpib ~ theil2000) #explicação do log da var do pib per capita em 10 anos pelo índice de desigualdade de theil em no começo do período (2000)
plot(reg1)
summary(reg1)

#p valor indica que o parâmetro é significativo
#r-quad: qto da variação de y está sendo explicada por x

data$dpib=log((pib2010/pop2010)/(pib2000/pop2000)) #inserir uma coluna dpib na matriz data
data[,c(2,8)] #para pegar a coluna para encontrar qual o
data[order(-dpib),]

#Cairu  4.505125918
#Itagibá  3.501334691

u=residuals(reg1) #pegar um vetor dos resíduos da regressao
hist(u)

#tirando as 3 primeiras cidades
data=data[order(-dpib),3:nrow(data)]

#Estimador de Máxima Verossimilhança

#vc tem uma distrib
#o q a caracteriza é média e dp (0, 1)
#vc retira uma amostra
#o q vc vai fazer é maximizar a probabilidade
#d q a amostra tenha saido dessa pop com
#essas características
#de média=0 e dp=1

#encontrar a probabilidade de que a amostra tenha
#vindo de uma determinada população

#EMV tem aplicação mais geral

hist(rexp(1000,rate=1)) #distr exponencial

#estimar o teta
#pega a densidade, lineariza e faz o estimador

log(rexp(1000,rate=1))
hist(log(rexp(1000,rate=1)))

#encontrar o EMV de uma distrib normal

set.seed(1234)
n=100
data=rexp(n,rate=1) #dsitrib exponencial com 100 observaçoes e lâmbida = 1
hist(data)

ll=function(x,lambda) {
  -sum(log(lambda) - lambda*x)
}

p=optim(par=0.5, ll), x=data) #onde está o x?
