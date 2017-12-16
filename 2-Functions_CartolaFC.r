# Formations
# 343 3zag    4mei3ata
# 352 3zag    5mei2ata
# 433 2zag2lat3mei3ata
# 442 2zag2lat4mei2ata
# 451 2zag2lat5mei1ata
# 532 3zag2lat3mei2ata
# 541 3zag2lat4mei1ata

# This function made a list of recommended players to use in a round, base
# Input parameters:
#  1. df_fctClube: data frame with football players (footballers)
#  2. strFormacao: a string length 3 with formation 
#  3. strPosicaoId: a string with field name (variable name) that contains the identifier of position
#                   1-goalkeeps, 2-lateral, 3-defense, 4-midfield, 5-forward, 6-coach 
#  4. extra: a number of adittional players recommended
# recebe dataframe, formação e campo pontuação prevista
# devolve dataframe com a escalação: 11 jogadores e técnico com maior pontuação

fctFormation <- function(df_fctClube, strFormacao, strPosicaoId, extra=0) {
  # Just use players with average ponctuation greater than 1.5
  df_fctClube2 <- df_fctClube[df_fctClube$jogos_num>=0&df_fctClube$media_num>1.5,]

  # Golkepper - 1 player for all formations
  dfgol <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==1,],1+extra)
  td1 <- as.numeric(dfgol$clube_id)

  # Lateral - if formation starts with 3, there are no use of laterals. If starts with 4 or 5, 2 laterals
  lat <- ifelse(as.integer(substr(strFormacao, 1,1))>=4,2,0 )
  dflat <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==2,],lat+extra)

  # Defense - if formation starts with 3 or 5, uses 3 defensers. If 4, 2 defensers
  zag <- ifelse(as.integer(substr(strFormacao, 1,1))==4,2,3 )
  dfzag <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==3,],zag+extra)
  
  # Midfield
  mei <- as.integer(substr(strFormacao, 2,2))
  dfmei <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==4,],mei+extra)

  # Forward
  ata <- as.integer(substr(strFormacao, 3,3))
  dfata <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==5,],ata+extra)

  # Coach - 1 player for all formations
  dftec <- head(df_fctClube2[df_fctClube2[,strPosicaoId]==6,],1+extra)
  
  # Returns a dataframe with players from all positions
  return(rbind(dftec, dfgol, dfzag, dflat, dfmei, dfata))
}

multi.fun <- function(x) {
  c(sum = sum(x), mean = mean(x), sd = sd(x))
}  

fctEscalacoes <- function(dftest, strCentroids, strRound) {
  # Formations "343","352", "433", "442", "451", "532", "541"
  # This functions prepare a dataframe with best prediction (max(pontos_num_p)) for each formation
  # it is used to analyze best formation (formation with higher predicted pontuation)
  formacao<-"343"
  esc <- fctFormation(dftest[order(-dftest$pontos_num_p),], formacao, "posicao_id")
  
  dfescala <- t(sapply(esc[,c("pontos_num","pontos_num_p")], multi.fun))
  dfescala <- as.data.frame(dfescala)
  dfescala$tipo <- row.names(dfescala)  
  dfescala$formacao <- formacao
  dfescala$rodada <- strRound
  
  dfescalaf <- dfescala
  
  for (formacao in c("352", "433", "442", "451", "532", "541")){
    esc <- fctFormation(dftest[order(-dftest$pontos_num_p),], formacao, "posicao_id")
    dfescala <- t(sapply(esc[,c("pontos_num","pontos_num_p")], multi.fun))
    dfescala <- as.data.frame(dfescala)
    dfescala$tipo <- row.names(dfescala)  
    dfescala$formacao <- formacao
    dfescala$rodada <- strRound
    
    dfescalaf <- rbind(dfescalaf, dfescala)
  }
  
  dfescalaf$k_centroids <- strCentroids  
  
  return(dfescalaf)
}