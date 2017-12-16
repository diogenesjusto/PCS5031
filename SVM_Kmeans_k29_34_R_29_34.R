# Loading libraries 
library(sqldf)
library(flexclust)
library(dummies)
library(xgboost)
library(scatterD3)
library(randomForest)
library(e1071)

sum <- mean <- sd <- as.numeric()
tipo <- formacao <- rodada <- k_centroids <- as.character()
dfescalacaof <- data.frame(sum, mean, sd, tipo, formacao, rodada, k_centroids)

for (iRod in 29:34) {
  # Start parameters
  # iRod = round to predict
  # dfall = d, dataframe with matches and players characteristics
  iRod <- iRod
  dfall <- d
  rodada_a_prever <- iRod
  rodada_start <- 1

  # Data preparation: extract hour from field date+hour
  dfall$hora_c <- (substr(dfall$partidas.partida_data, 12,13))
  dfall$hora <- as.factor(dfall$hora_c)

  # Data preparation: just renaming variables
  dfall$clube_casa_id <- dfall$partidas.clube_casa_id
  dfall$clube_visitante_id <- dfall$partidas.clube_visitante_id
  dfall$local <- dfall$partidas.local

  # Data preparation: creating a new variable - club against home club is playing
  dfall$clube_id_against <- ifelse(dfall$clube_id==dfall$clube_casa_id, dfall$clube_visitante_id, dfall$clube_casa_id)

  # Preparing data split:
  # Train: all information, starting in round 1 (or parameter setting rodada_start) until round that is being to predict
  # Test: round that will be predict
  train_o <- dfall[(dfall$rodada_id<=(rodada_a_prever-1))&(dfall$rodada_id>=(rodada_start))&dfall$jogos_num>2,]
  test_o <- dfall[dfall$rodada_id == rodada_a_prever,]

  # Preparation on test dataset
  # Eliminating real scouts and real average from player (this contains data that must be predicted)
  myvars <- names(test_o) %in% c("GS","FC","FF","FS","I","PE","SG","CA","RB","DD","G","FD","A","FT","CV","GC","DP","PP") 
  test_o <- test_o[!myvars]
  test_o$media_num <- NULL
  test_o$jogos_num <- NULL # ALTERAÇAO 2 EM RELACAO AO PREDICTION

  # Capturing information from last round before round is being predict
  dfMediaRodAnterior <- sqldf(paste("select atleta_id, media_num
                                    from train_o where rodada_id=",rodada_a_prever-1))
  test_o <- merge(test_o, dfMediaRodAnterior, by = "atleta_id")
  
  dfScoutsFuturo <- sqldf("select atleta_id, max(FS) FS, max(A) A, max(FT) FT, max(FD) FD, 
                          max(FF) FF, max(G) G,max(RB) RB,max(SG) SG,max(DD) DD,max(DP) DP,
                          max(PE) PE, max(I) I,max(PP) PP,max(FC) FC,max(GC) GC,max(CA) CA,
                          max(CV) CV, max(GS) GS, max(jogos_num) jogos_num
                          from train_o group by atleta_id")

  test_o <- merge(test_o, dfScoutsFuturo, by = "atleta_id")
  
  # Data preparation: creating a new dataset that will be used to unsupervisioned learning
  # K-Means to create clusters of footballers that is suposed to have same behavior
  dfk <- rbind(
    train_o[,c( "media_num", "mandante", "posicao_id",  "rodada_id", "clube_id_against", 
                "FS","A","FT","FD","FF","G","RB","SG","DD","DP","PE","I","PP","FC","GC","CA","CV","GS")],
    test_o[,c( "media_num", "mandante", "posicao_id", "rodada_id", "clube_id_against",
               "FS","A","FT","FD","FF","G","RB","SG","DD","DP","PE","I","PP","FC","GC","CA","CV","GS")]
  )
  dfk <- as.data.frame(sapply(dfk, as.numeric))

  #for (i in 22:28) {
    # Kmeans scan
    # We are going to test a range of predictions with diferente number of clusters to define best prediction
    # Using variation of Kmeans called kmeans++ (plus plus)
    # Reference: Arthur, D.; Vassilvitskii, S. (2007). "k-means++: the advantages of careful seeding" (PDF). Proceedings of the eighteenth annual ACM-SIAM symposium on Discrete algorithms. Society for Industrial and Applied Mathematics Philadelphia, PA, USA. pp. 1027–1035.
    kcca <- kcca(dfk, k=i, family=kccaFamily("kmeans"),
                 control=list(initcent="kmeanspp"))

    dfn <- rbind(train_o, test_o)

    # Preparing data types
    dfn$cluster <- as.factor(kcca@cluster)
    dfn$clube_id_against <- as.factor(dfn$clube_id_against)
    dfn$posicao_id <- as.factor(dfn$posicao_id)
    dfn$local_factor <- (as.numeric(as.factor(dfn$local)))

    # Using numeric variables    
    dfn2 <- dfn[,c("pontos_num", "jogos_num", "media_num", "classificacao", "posicao_id")]

    # Creating dummy variables
    #dfn2 <- cbind(dfn2, dummy.data.frame(dfn[,c("cluster", "mandante", "posicao_id", "clube_id", "clube_casa_id", "clube_visitante_id", "clube_id_against", "atleta_id", "local_factor", "hora")]))
    dfn2 <- cbind(dfn2, dummy.data.frame(dfn[,c("mandante", "posicao_id", "clube_id", "clube_casa_id", "clube_visitante_id", "clube_id_against", "atleta_id", "local_factor", "hora")]))
    
    # Splitting train and test, after data preparation 
    train <- dfn2[(dfn$rodada_id <= (rodada_a_prever-1)),]
    test <- dfn2[dfn$rodada_id == rodada_a_prever,]

    # Removing va
    test_jogos_num <- test$jogos_num+1
    train$jogos_num <- NULL
    train$posicao_id <- NULL
    
    test$jogos_num <- NULL
    test_pontos_num <- test$pontos_num
    test$pontos_num <- NULL
    test_posicao_id <- test_o$posicao_id
    test$posicao_id <- NULL
    
    # Preparing data for using specificaly for XGBoost    
    set.seed(1)
    y<-as.numeric(train$pontos_num)
    x<-train[,-1]
    x<-as.data.frame(sapply(x, as.numeric))
    t<-test
    t<-as.data.frame(sapply(t, as.numeric))
    mdy <- as.matrix(y); mdx <- as.matrix(x); mdtest <- as.matrix(t)
    mdy_l <- as.matrix(y)

    # dataInput for use in randomForest
    dataInput <- as.data.frame(cbind(mdy_l, mdx))

    ##########
    #m <- xgboost(data=mdx, label=mdy_l, lambda=.25, alpha=0.45,max.depth = 500, eta = 1, nthread = 8, nrounds = 300, objective = "reg:linear", missing=NaN, verbose=0)
    #m <- randomForest(V1~., ntree=200, data=dataInput )
    m <- svm(V1~., data=dataInput )
    print(i)
    ##########
    p <- predict(m, newdata=mdtest)
    
    test$pontos_num <- test_pontos_num
    test$pontos_num_p <- p
    test$jogos_num <- test_jogos_num
    test$posicao_id <- test_posicao_id

    # Choose best set of players      
    dfescalacao <- fctEscalacoes(test, paste("k=", i), paste("Rod=", iRod))
    dfescalacaof <- rbind(dfescalacaof, dfescalacao)
  #}
}
# Preparing data Visualization
df <- dfescalacaof
dfr <- (df[df$tipo=="pontos_num",])
dfp <- (df[df$tipo=="pontos_num_p",])
colnames(dfp) <- paste(colnames(dfp), "p", sep="")
dfc <- cbind(dfr, dfp)

# Data visualization
# We try to find out one graphic that maximize dfc$sump (prediction for ponctuation for a set of 12 players) 
# and dfc$sum, that was the real ponctuation
scatterD3(x = dfc$sump, y = dfc$sum, col_var=dfc$formacao, lab=dfc$k_centroids, symbol_var=dfc$rodada)
