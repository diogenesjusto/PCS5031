library(dplyr)

d <- read.csv("0-Data.csv", sep=",", dec=".")

# 1st - Just using players that have points in matches
d <- d[d$pontos_num!=0,]

# 2nd - Create cumulative scouts
# This will be used to understand players behavior in k-means use
# We are going to use cumsum from dplyr package
library(dplyr)
d$CA_o <- d$CA
d <- as.data.frame(mutate(group_by(d,atleta_id), CA=cumsum(CA_o)))
d$CV_o <- d$CV
d <- as.data.frame(mutate(group_by(d,atleta_id), CV=cumsum(CV_o)))
d$FC_o <- d$FC
d <- as.data.frame(mutate(group_by(d,atleta_id), FC=cumsum(FC_o)))
d$FF_o <- d$FF
d <- as.data.frame(mutate(group_by(d,atleta_id), FF=cumsum(FF_o)))
d$FS_o <- d$FS
d <- as.data.frame(mutate(group_by(d,atleta_id), FS=cumsum(FS_o)))
d$PE_o <- d$PE
d <- as.data.frame(mutate(group_by(d,atleta_id), PE=cumsum(PE_o)))
d$RB_o <- d$RB
d <- as.data.frame(mutate(group_by(d,atleta_id), RB=cumsum(RB_o)))
d$FD_o <- d$FD
d <- as.data.frame(mutate(group_by(d,atleta_id), FD=cumsum(FD_o)))
d$I_o <- d$I
d <- as.data.frame(mutate(group_by(d,atleta_id), I=cumsum(I_o)))
d$SG_o <- d$SG
d <- as.data.frame(mutate(group_by(d,atleta_id), SG=cumsum(SG_o)))
d$A_o <- d$A
d <- as.data.frame(mutate(group_by(d,atleta_id), A=cumsum(A_o)))
d$DD_o <- d$DD
d <- as.data.frame(mutate(group_by(d,atleta_id), DD=cumsum(DD_o)))
d$GS_o <- d$GS
d <- as.data.frame(mutate(group_by(d,atleta_id), GS=cumsum(GS_o)))
d$PP_o <- d$PP
d <- as.data.frame(mutate(group_by(d,atleta_id), PP=cumsum(PP_o)))
d$FT_o <- d$FT
d <- as.data.frame(mutate(group_by(d,atleta_id), FT=cumsum(FT_o)))
d$G_o <- d$G
d <- as.data.frame(mutate(group_by(d,atleta_id), G=cumsum(G_o)))
d$GC_o <- d$GC
d <- as.data.frame(mutate(group_by(d,atleta_id), GC=cumsum(GC_o)))
d$DP_o <- d$DP
d <- as.data.frame(mutate(group_by(d,atleta_id), DP=cumsum(DP_o)))

detach("package:dplyr", unload=TRUE)

