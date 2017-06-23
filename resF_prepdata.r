require(ggplot2)
require(plyr)
require(reshape2)
require(sjPlot)
require(Hmisc)
require(GGally)
require(stats)
require(xlsx)
require(e1071)
require(forecast)

# clean data
# clnData1: remove stop words, remove items with zero f_coca values, remove sentence initial and last words
clnData1 <- function(FileNam){
  D <- read.csv(FileNam, na.strings = c("NA",""))
  D <- D[!(D$stopw), ]
  D <- D[D$s_mid==1,]
  return(D)
}
# clnData2: keep stopwords, remove items with zero f_coca values, remove sentence initial and last words
clnData2 <- function(FileNam){
  D <- read.csv(FileNam, na.strings = c("NA",""))
  D <- D[D$s_mid==1,]
  return(D)
}
# prepare frequency values
calLogFreq <- function(X, newColName, oldCol){
  X[!is.na(X[,oldCol]), newColName] <- log(X[!is.na(X[,oldCol]),oldCol])
  X[is.na(X[,oldCol]), newColName] <- NA
  return(X)
}
prepFreq <- function(X){
  # replace spilover = 0 with NA
  X$spilover_log[X$spilover == 0] <- NA
  X$spilover_log[X$spilover != 0] <- log(X$spilover[X$spilover != 0])
  X$spilover[X$spilover == 0] <- NA
  # add log for f_kf, f_hal, f_coca, f_bnc, f_soap
  X$f_kf_log <- 0.0; X <- calLogFreq(X, "f_kf_log", "f_kf")
  X$f_hal_log <- 0.0; X <- calLogFreq(X, "f_hal_log", "f_hal")
  X$f_coca_log <- 0.0; X <- calLogFreq(X, "f_coca_log", "f_coca")
  X$prevf_coca_log <- 0.0; X <- calLogFreq(X, "prevf_coca_log", "prevf_coca")
  X$nextf_coca_log <- 0.0; X <- calLogFreq(X, "nextf_coca_log", "nextf_coca")
  X$f_bnc_log <- 0.0; X <- calLogFreq(X, "f_bnc_log", "f_bnc")
  X$f_soap_log <- 0.0; X <- calLogFreq(X, "f_soap_log", "f_soap")
  X$f_coca_lem_log <- 0.0; X <- calLogFreq(X, "f_coca_lem_log", "f_coca_lem")
  X$f_bnc_lem_log <- 0.0; X <- calLogFreq(X, "f_bnc_lem_log", "f_bnc_lem")
  X$f_soap_lem_log <- 0.0; X <- calLogFreq(X, "f_soap_lem_log", "f_soap_lem")
  X$prevf_coca_lem_log <- 0.0; X <- calLogFreq(X, "prevf_coca_lem_log", "prevf_coca_lem")
  X$nextf_coca_lem_log <- 0.0; X <- calLogFreq(X, "nextf_coca_lem_log", "nextf_coca_lem")
  return(X)
}

# D_ET_F: total: 28563 lines;
D_ET_F1 <- clnData1("./data/res_F/ET_F_raw.csv")
D_ET_F1 <- prepFreq(D_ET_F1)
D_ET_F1 <- D_ET_F1[,c(1:13,155,14,156,15,157,16,162,17,160,18,163,19,161,20,164,
                      21:23,158,24,165,25:27,159,28,166,29:39,154,40:153)]
write.csv(D_ET_F1, "./data/res_F/ET_F_data1.csv", row.names=FALSE)
# 15733 lines
D_ET_F2 <- clnData2("./data/res_F/ET_F_raw.csv")
D_ET_F2 <- prepFreq(D_ET_F2)
D_ET_F2 <- D_ET_F2[,c(1:13,155,14,156,15,157,16,162,17,160,18,163,19,161,20,164,
                      21:23,158,24,165,25:27,159,28,166,29:39,154,40:153)]
write.csv(D_ET_F2, "./data/res_F/ET_F_data2.csv", row.names=FALSE)
# 23752 lines

# get clean data
D_ET_F1 <- read.csv("./data/res_F/ET_F_data1.csv", na.strings = c("NA", ""))
D_ET_F1 <- D_ET_F1[,-c(2:4,6,8,11,13:16,21:28,59,60)]
D_ET_F2 <- read.csv("./data/res_F/ET_F_data2.csv", na.strings = c("NA", ""))
D_ET_F2 <- D_ET_F2[,-c(2:4,6,8,11,13:16,21:28,59,60)]
str(D_ET_F1)
str(D_ET_F2)

# check correlation between f_coca and reglen, prevf_coca and prevwlen, nextf_coca and nextwlen
sub_f1 <- D_ET_F1[,c("reglen", "f_coca", "f_coca_log")]
sub_prevf1 <- D_ET_F1[,c("prevwlen", "prevf_coca", "prevf_coca_log")]
sub_nextf1 <- D_ET_F1[,c("nextwlen", "nextf_coca", "nextf_coca_log")]
sub_f2 <- D_ET_F2[,c("reglen", "f_coca", "f_coca_log")]
sub_prevf2 <- D_ET_F2[,c("prevwlen", "prevf_coca", "prevf_coca_log")]
sub_nextf2 <- D_ET_F2[,c("nextwlen", "nextf_coca", "nextf_coca_log")]

rcorr(as.matrix(sub_f1), type = c("pearson"))
# reglen f_coca f_coca_log
# reglen       1.00  -0.43      -0.50
# f_coca      -0.43   1.00       0.69
# f_coca_log  -0.50   0.69       1.00
ggpairs(sub_f1, title = "f corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_len_f1.png
rcorr(as.matrix(sub_f2), type = c("pearson"))
# reglen f_coca f_coca_log
# reglen       1.00  -0.42      -0.73
# f_coca      -0.42   1.00       0.66
# f_coca_log  -0.73   0.66       1.00
ggpairs(sub_f2, title = "f corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_len_f2.png

rcorr(as.matrix(sub_prevf1), type = c("pearson"))
# prevwlen prevf_coca prevf_coca_log
# prevwlen           1.00      -0.47          -0.77
# prevf_coca        -0.47       1.00           0.76
# prevf_coca_log    -0.77       0.76           1.00
ggpairs(sub_prevf1, title = "prevf corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevwlen_prevf1.png
rcorr(as.matrix(sub_prevf2), type = c("pearson"))
# prevwlen prevf_coca prevf_coca_log
# prevwlen           1.00      -0.45          -0.75
# prevf_coca        -0.45       1.00           0.72
# prevf_coca_log    -0.75       0.72           1.00
ggpairs(sub_prevf2, title = "prevf corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevwlen_prevf2.png

rcorr(as.matrix(sub_nextf1), type = c("pearson"))
# nextwlen nextf_coca nextf_coca_log
# nextwlen           1.00      -0.50          -0.75
# nextf_coca        -0.50       1.00           0.7
# nextf_coca_log    -0.75       0.7            1.00
ggpairs(sub_nextf1, title = "nextf corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextwlen_nextf1.png
rcorr(as.matrix(sub_nextf2), type = c("pearson"))
# nextwlen nextf_coca nextf_coca_log
# nextwlen           1.00      -0.46          -0.75
# nextf_coca        -0.46       1.00           0.70
# nextf_coca_log    -0.75       0.70           1.00
ggpairs(sub_nextf2, title = "nextf corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextwlen_nextf2.png

# linear regression between f_coca and reglen
fit_word1 <- lm(f_coca_log ~ reglen, data = D_ET_F1)
fit_prevw1 <- lm(prevf_coca_log ~ prevwlen, data = D_ET_F1)
fit_nextw1 <- lm(nextf_coca_log ~ nextwlen, data = D_ET_F1)

D_ET_F1$res_f_coca_log <- NA; D_ET_F1$res_f_coca_log[!is.na(D_ET_F1$f_coca_log)] <- residuals(fit_word1)
D_ET_F1$res_prevf_coca_log <- NA; D_ET_F1$res_prevf_coca_log[!is.na(D_ET_F1$prevf_coca_log)] <- residuals(fit_prevw1)
D_ET_F1$res_nextf_coca_log <- NA; D_ET_F1$res_nextf_coca_log[!is.na(D_ET_F1$nextf_coca_log)] <- residuals(fit_nextw1)

fit_word2 <- lm(f_coca_log ~ reglen, data = D_ET_F2)
fit_prevw2 <- lm(prevf_coca_log ~ prevwlen, data = D_ET_F2)
fit_nextw2 <- lm(nextf_coca_log ~ nextwlen, data = D_ET_F2)

D_ET_F2$res_f_coca_log <- NA; D_ET_F2$res_f_coca_log[!is.na(D_ET_F2$f_coca_log)] <- residuals(fit_word2)
D_ET_F2$res_prevf_coca_log <- NA; D_ET_F2$res_prevf_coca_log[!is.na(D_ET_F2$prevf_coca_log)] <- residuals(fit_prevw2)
D_ET_F2$res_nextf_coca_log <- NA; D_ET_F2$res_nextf_coca_log[!is.na(D_ET_F2$nextf_coca_log)] <- residuals(fit_nextw2)

# check correlations between residual freq and original freq
f_sub1 <- D_ET_F1[,c("f_coca_log", "res_f_coca_log")]
rcorr(as.matrix(f_sub1), type = c("pearson"))
# f_coca_log res_f_coca_log
# f_coca_log           1.0           0.86
# res_f_coca_log       0.86          1.0
# n= 15520 
ggpairs(f_sub1, title = "f corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_f_resf1.png
f_sub2 <- D_ET_F2[,c("f_coca_log", "res_f_coca_log")]
rcorr(as.matrix(f_sub2), type = c("pearson"))
# f_coca_log res_f_coca_log
# f_coca_log           1.0           0.69
# res_f_coca_log       0.69          1.0
# n= 23499 
ggpairs(f_sub2, title = "f corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_f_resf2.png

prevf_sub1 <- D_ET_F1[,c("prevf_coca_log", "res_prevf_coca_log")]
rcorr(as.matrix(prevf_sub1), type = c("pearson"))
# prevf_coca_log res_prevf_coca_log
# prevf_coca_log               1.00               0.64
# res_prevf_coca_log           0.64               1.00
# n= 15134
ggpairs(prevf_sub1, title = "prevf corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevf_resprevf1.png
prevf_sub2 <- D_ET_F2[,c("prevf_coca_log", "res_prevf_coca_log")]
rcorr(as.matrix(prevf_sub2), type = c("pearson"))
# prevf_coca_log res_prevf_coca_log
# prevf_coca_log               1.00               0.66
# res_prevf_coca_log           0.66               1.00
# n= 22982
ggpairs(prevf_sub2, title = "prevf corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevf_resprevf2.png

nextf_sub1 <- D_ET_F1[,c("nextf_coca_log", "res_nextf_coca_log")]
rcorr(as.matrix(nextf_sub1), type = c("pearson"))
# nextf_coca_log res_nextf_coca_log
# nextf_coca_log               1.00               0.66
# res_nextf_coca_log           0.66               1.00
# n= 15654 
ggpairs(nextf_sub1, title = "nextf corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextf_resnextf1.png
nextf_sub2 <- D_ET_F2[,c("nextf_coca_log", "res_nextf_coca_log")]
rcorr(as.matrix(nextf_sub2), type = c("pearson"))
# nextf_coca_log res_nextf_coca_log
# nextf_coca_log               1.00               0.66
# res_nextf_coca_log           0.66               1.00
# n= 23563 
ggpairs(nextf_sub2, title = "nextf corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextf_resnextf2.png

# check correlation between f_coca_lem and reglen, prevf_coca_lem and prevwlen, nextf_coca_lem and nextwlen
sub_f_lem1 <- D_ET_F1[,c("reglen", "f_coca_lem", "f_coca_lem_log")]
sub_prevf_lem1 <- D_ET_F1[,c("prevwlen", "prevf_coca_lem", "prevf_coca_lem_log")]
sub_nextf_lem1 <- D_ET_F1[,c("nextwlen", "nextf_coca_lem", "nextf_coca_lem_log")]
sub_f_lem2 <- D_ET_F2[,c("reglen", "f_coca_lem", "f_coca_lem_log")]
sub_prevf_lem2 <- D_ET_F2[,c("prevwlen", "prevf_coca_lem", "prevf_coca_lem_log")]
sub_nextf_lem2 <- D_ET_F2[,c("nextwlen", "nextf_coca_lem", "nextf_coca_lem_log")]

rcorr(as.matrix(sub_f_lem1), type = c("pearson"))
# reglen f_coca_lem f_coca_lem_log
# reglen           1.00      -0.45          -0.51
# f_coca_lem      -0.45       1.00           0.70
# f_coca_lem_log  -0.51       0.70           1.00
ggpairs(sub_f_lem1, title = "f_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_len_f_lem1.png
rcorr(as.matrix(sub_f_lem2), type = c("pearson"))
# reglen f_coca_lem f_coca_lem_log
# reglen           1.00      -0.48          -0.73
# f_coca_lem      -0.48       1.00           0.71
# f_coca_lem_log  -0.73       0.71           1.00
ggpairs(sub_f_lem2, title = "f_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_len_f_lem2.png

rcorr(as.matrix(sub_prevf_lem1), type = c("pearson"))
# prevwlen prevf_coca_lem prevf_coca_lem_log
# prevwlen               1.00          -0.53              -0.77
# prevf_coca_lem        -0.53           1.00               0.80
# prevf_coca_lem_log    -0.77           0.80               1.00
ggpairs(sub_prevf_lem1, title = "prevf_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevwlen_prevf_lem1.png
rcorr(as.matrix(sub_prevf_lem2), type = c("pearson"))
# prevwlen prevf_coca_lem prevf_coca_lem_log
# prevwlen               1.00          -0.51              -0.76
# prevf_coca_lem        -0.51           1.00               0.77
# prevf_coca_lem_log    -0.76           0.77               1.00
ggpairs(sub_prevf_lem2, title = "prevf_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevwlen_prevf_lem2.png

rcorr(as.matrix(sub_nextf_lem1), type = c("pearson"))
# nextwlen nextf_coca_lem nextf_coca_lem_log
# nextwlen               1.00          -0.54              -0.76
# nextf_coca_lem        -0.54           1.00               0.75
# nextf_coca_lem_log    -0.76           0.75               1.00
ggpairs(sub_nextf_lem1, title = "nextf_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextwlen_nextf_lem1.png
rcorr(as.matrix(sub_nextf_lem2), type = c("pearson"))
# nextwlen nextf_coca_lem nextf_coca_lem_log
# nextwlen               1.00          -0.51              -0.75
# nextf_coca_lem        -0.51           1.00               0.75
# nextf_coca_lem_log    -0.75           0.75               1.00
ggpairs(sub_nextf_lem2, title = "nextf_lem corr (before residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextwlen_nextf_lem2.png

# linear regression between f_coca_lem and reglen
fit_word1 <- lm(f_coca_lem_log ~ reglen, data = D_ET_F1)
fit_prevw1 <- lm(prevf_coca_lem_log ~ prevwlen, data = D_ET_F1)
fit_nextw1 <- lm(nextf_coca_lem_log ~ nextwlen, data = D_ET_F1)

D_ET_F1$res_f_coca_lem_log <- NA; D_ET_F1$res_f_coca_lem_log[!is.na(D_ET_F1$f_coca_lem_log)] <- residuals(fit_word1)
D_ET_F1$res_prevf_coca_lem_log <- NA; D_ET_F1$res_prevf_coca_lem_log[!is.na(D_ET_F1$prevf_coca_lem_log)] <- residuals(fit_prevw1)
D_ET_F1$res_nextf_coca_lem_log <- NA; D_ET_F1$res_nextf_coca_lem_log[!is.na(D_ET_F1$nextf_coca_lem_log)] <- residuals(fit_nextw1)

fit_word2 <- lm(f_coca_lem_log ~ reglen, data = D_ET_F2)
fit_prevw2 <- lm(prevf_coca_lem_log ~ prevwlen, data = D_ET_F2)
fit_nextw2 <- lm(nextf_coca_lem_log ~ nextwlen, data = D_ET_F2)

D_ET_F2$res_f_coca_lem_log <- NA; D_ET_F2$res_f_coca_lem_log[!is.na(D_ET_F2$f_coca_lem_log)] <- residuals(fit_word2)
D_ET_F2$res_prevf_coca_lem_log <- NA; D_ET_F2$res_prevf_coca_lem_log[!is.na(D_ET_F2$prevf_coca_lem_log)] <- residuals(fit_prevw2)
D_ET_F2$res_nextf_coca_lem_log <- NA; D_ET_F2$res_nextf_coca_lem_log[!is.na(D_ET_F2$nextf_coca_lem_log)] <- residuals(fit_nextw2)

# check correlations between residual f_coca_lem and original freq
f_lem_sub1 <- D_ET_F1[,c("f_coca_lem_log", "res_f_coca_lem_log")]
rcorr(as.matrix(f_lem_sub1), type = c("pearson"))
# f_coca_lem_log res_f_coca_lem_log
# f_coca_lem_log               1.0               0.86
# res_f_coca_lem_log           0.86              1.0
# n= 15520
ggpairs(f_lem_sub1, title = "f_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_f_resf_lem1.png
f_lem_sub2 <- D_ET_F2[,c("f_coca_lem_log", "res_f_coca_lem_log")]
rcorr(as.matrix(f_lem_sub2), type = c("pearson"))
# f_coca_lem_log res_f_coca_lem_log
# f_coca_lem_log               1.0               0.69
# res_f_coca_lem_log           0.69              1.0
# n= 23499
ggpairs(f_lem_sub2, title = "f_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_f_resf_lem2.png

prevf_lem_sub1 <- D_ET_F1[,c("prevf_coca_lem_log", "res_prevf_coca_lem_log")]
rcorr(as.matrix(prevf_lem_sub1), type = c("pearson"))
# prevf_coca_lem_log res_prevf_coca_lem_log
# prevf_coca_lem_log                   1.00                   0.63
# res_prevf_coca_lem_log               0.63                   1.00
# n= 15134 
ggpairs(prevf_lem_sub1, title = "prevf_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevf_resprevf_lem1.png
prevf_lem_sub2 <- D_ET_F2[,c("prevf_coca_lem_log", "res_prevf_coca_lem_log")]
rcorr(as.matrix(prevf_lem_sub2), type = c("pearson"))
# prevf_coca_lem_log res_prevf_coca_lem_log
# prevf_coca_lem_log                   1.00                   0.65
# res_prevf_coca_lem_log               0.65                   1.00
# n= 22982 
ggpairs(prevf_lem_sub2, title = "prevf_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_prevf_resprevf_lem2.png

nextf_lem_sub1 <- D_ET_F1[,c("nextf_coca_lem_log", "res_nextf_coca_lem_log")]
rcorr(as.matrix(nextf_lem_sub1), type = c("pearson"))
# nextf_coca_lem_log res_nextf_coca_lem_log
# nextf_coca_lem_log                   1.00                   0.66
# res_nextf_coca_lem_log               0.66                   1.00
# n= 15654 
ggpairs(nextf_lem_sub1, title = "nextf_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextf_resnextf_lem1.png
nextf_lem_sub2 <- D_ET_F2[,c("nextf_coca_lem_log", "res_nextf_coca_lem_log")]
rcorr(as.matrix(nextf_lem_sub2), type = c("pearson"))
# nextf_coca_lem_log res_nextf_coca_lem_log
# nextf_coca_lem_log                   1.00                   0.66
# res_nextf_coca_lem_log               0.66                   1.00
# n= 23563 
ggpairs(nextf_lem_sub2, title = "nextf_lem corr (after residualization)", upper = list(params = c(size = 10)), lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue")), diag = list(continuous="density"), axisLabels='show')
# Cor_nextf_resnextf_lem2.png

# standarize res_f_coca_log, res_prevf_coca_log, res_nextf_coca_log, reglen, nextwlen, prevwlen, region, item, and ffos
D_ET_F1$std_res_f_coca_log <- scale(D_ET_F1$res_f_coca_log)
D_ET_F1$std_res_prevf_coca_log <- scale(D_ET_F1$res_prevf_coca_log)
D_ET_F1$std_res_nextf_coca_log <- scale(D_ET_F1$res_nextf_coca_log)
D_ET_F1$std_res_f_coca_lem_log <- scale(D_ET_F1$res_f_coca_lem_log)
D_ET_F1$std_res_prevf_coca_lem_log <- scale(D_ET_F1$res_prevf_coca_lem_log)
D_ET_F1$std_res_nextf_coca_lem_log <- scale(D_ET_F1$res_nextf_coca_lem_log)

D_ET_F2$std_res_f_coca_log <- scale(D_ET_F2$res_f_coca_log)
D_ET_F2$std_res_prevf_coca_log <- scale(D_ET_F2$res_prevf_coca_log)
D_ET_F2$std_res_nextf_coca_log <- scale(D_ET_F2$res_nextf_coca_log)
D_ET_F2$std_res_f_coca_lem_log <- scale(D_ET_F2$res_f_coca_lem_log)
D_ET_F2$std_res_prevf_coca_lem_log <- scale(D_ET_F2$res_prevf_coca_lem_log)
D_ET_F2$std_res_nextf_coca_lem_log <- scale(D_ET_F2$res_nextf_coca_lem_log)

# for region, reglen, prevwlen, nextwlen, add centered and standarized values
D_ET_F1$region_C <- scale(D_ET_F1$region, scale=F); D_ET_F1$region_S <- scale(D_ET_F1$region)
D_ET_F1$reglen_C <- scale(D_ET_F1$reglen, scale=F); D_ET_F1$reglen_S <- scale(D_ET_F1$reglen)
D_ET_F1$prevwlen_C <- scale(D_ET_F1$prevwlen, scale=F); D_ET_F1$prevwlen_S <- scale(D_ET_F1$prevwlen)
D_ET_F1$nextwlen_C <- scale(D_ET_F1$nextwlen, scale=F); D_ET_F1$nextwlen_S <- scale(D_ET_F1$nextwlen)

D_ET_F2$region_C <- scale(D_ET_F2$region, scale=F); D_ET_F2$region_S <- scale(D_ET_F2$region)
D_ET_F2$reglen_C <- scale(D_ET_F2$reglen, scale=F); D_ET_F2$reglen_S <- scale(D_ET_F2$reglen)
D_ET_F2$prevwlen_C <- scale(D_ET_F2$prevwlen, scale=F); D_ET_F2$prevwlen_S <- scale(D_ET_F2$prevwlen)
D_ET_F2$nextwlen_C <- scale(D_ET_F2$nextwlen, scale=F); D_ET_F2$nextwlen_S <- scale(D_ET_F2$nextwlen)

# add ssfix column: it equals to ffixurt when fpcount = 1, or NA otherwise
D_ET_F1$sfix[D_ET_F1$fpcount == 1] <- D_ET_F1$ffixurt[D_ET_F1$fpcount == 1]
D_ET_F1$sfix[D_ET_F1$fpcount != 1] <- NA

D_ET_F2$sfix[D_ET_F2$fpcount == 1] <- D_ET_F2$ffixurt[D_ET_F2$fpcount == 1]
D_ET_F2$sfix[D_ET_F2$fpcount != 1] <- NA

# add new eye-movement measures: fpregres_in
D_ET_F1$fpregres_in[D_ET_F1$spurt!=0] <- 1
D_ET_F1$fpregres_in[D_ET_F1$spurt==0] <- 0

D_ET_F2$fpregres_in[D_ET_F2$spurt!=0] <- 1
D_ET_F2$fpregres_in[D_ET_F2$spurt==0] <- 0

D_ET_F1 <- D_ET_F1[,c(1:4,159,160,5,6,161,162,7,8,147,153,9,10,150,156,11,12,163,164,
                      13,14,148,154,15,16,151,157,17,18,165,166,19,20,149,155,21,22,152,158,
                      23:28,168,29:32,167,33:51,86,55,52,53,130,54,56:59,132,60:64,135,65:69,136,70,71,144,
                      72:75,117,118,122,138,76:78,119,120,133,
                      79,81,82,80,136,83:85,87:89,123,139,145,90:94,127,131,95,96,146,97:116,
                      121,124:126,129,134,137,140,143)]
write.csv(D_ET_F1, "./data/res_F/ET_F_ana1.csv", row.names=FALSE)
D_ET_F2 <- D_ET_F2[,c(1:4,159,160,5,6,161,162,7,8,147,153,9,10,150,156,11,12,163,164,
                      13,14,148,154,15,16,151,157,17,18,165,166,19,20,149,155,21,22,152,158,
                      23:28,168,29:32,167,33:51,86,55,52,53,130,54,56:59,132,60:64,135,65:69,136,70,71,144,
                      72:75,117,118,122,138,76:78,119,120,133,
                      79,81,82,80,136,83:85,87:89,123,139,145,90:94,127,131,95,96,146,97:116,
                      121,124:126,129,134,137,140,143)]
write.csv(D_ET_F2, "./data/res_F/ET_F_ana2.csv", row.names=FALSE)


# add new eye-movement measures: total reading time
D_ET_F1$turt[D_ET_F1$spurt==0] <- D_ET_F1$fpurt[D_ET_F1$spurt==0]
D_ET_F1$turt[D_ET_F1$spurt!=0] <- D_ET_F1$fpurt[D_ET_F1$spurt!=0] + D_ET_F1$spurt[D_ET_F1$spurt!=0]

write.csv(D_ET_F1, "./data/res_F/ET_F_ana1.csv", row.names=FALSE)

D_ET_F2$turt[D_ET_F2$spurt==0] <- D_ET_F2$fpurt[D_ET_F2$spurt==0]
D_ET_F2$turt[D_ET_F2$spurt!=0] <- D_ET_F2$fpurt[D_ET_F2$spurt!=0] + D_ET_F2$spurt[D_ET_F2$spurt!=0]

write.csv(D_ET_F2, "./data/res_F/ET_F_ana2.csv", row.names=FALSE)

# get the information about the subject age and individual reading skills
calIndvalues <- function(X, fileNam){
  labelnames <- c("Label", "Mean", "SD", "Min.", "25%", "50%", "75%", "Max.", "Skewness", "Kurtosis")
  Dres <- data.frame(matrix(ncol = length(labelnames), nrow = ncol(X)))
  names(Dres) <- labelnames
  for(i in 1:ncol(X)){
    Dres$Label[i] <- names(X)[i]
    Dres$Mean[i] <- mean(X[,i], na.rm = TRUE)
    Dres$SD[i] <- sd(X[,i], na.rm = TRUE)
    fivevalue <- fivenum(X[,i])
    for(j in 1:5){
      Dres[i,j+3] <- fivevalue[j]
    }
    Dres$Skewness[i] <- skewness(X[,i], na.rm = TRUE, type = 2)
    Dres$Kurtosis[i] <- kurtosis(X[,i], na.rm = TRUE, type = 2)
  }
  write.csv(Dres, fileNam, row.names=FALSE)
  return(Dres)
}

D_ET_F1 <- read.csv("./data/res_F/ET_F_ana1.csv", na.strings = c("NA", ""))
D_ET_F2 <- read.csv("./data/res_F/ET_F_ana2.csv", na.strings = c("NA", ""))

# extract unique word information
D_word1 <- D_ET_F1[,c(7,4:6,8:12,14:16,18,20:24,26:28,30,32:36,38:40,42)]
D_wordinfo1 <- data.frame(matrix(ncol = ncol(D_word1), nrow = length(unique(D_word1$word))))
names(D_wordinfo1) <- names(D_word1)
D_wordinfo1$word <- unique(D_word1$word)
for(i in 1:nrow(D_wordinfo1)){
  D_wordinfo1[i,2:ncol(D_word1)] <- D_word1[D_word1$word %in% D_wordinfo1$word[i],][1,2:ncol(D_word1)]
}
# extract EM data
D_EM1 <- D_ET_F1[,c(44,45,46,48,49,52,53,57,61,167)]

D_word_res1 <- calIndvalues(D_wordinfo1[,-c(1)], './data/res_F/WordInfo1.csv')
D_EM_res1 <- calIndvalues(D_EM1, './data/res_F/EM1.csv')

# extract unique word information
D_word2 <- D_ET_F2[,c(7,4:6,8:12,14:16,18,20:24,26:28,30,32:36,38:40,42)]
D_wordinfo2 <- data.frame(matrix(ncol = ncol(D_word2), nrow = length(unique(D_word2$word))))
names(D_wordinfo2) <- names(D_word2)
D_wordinfo2$word <- unique(D_word2$word)
for(i in 1:nrow(D_wordinfo2)){
  D_wordinfo2[i,2:ncol(D_word2)] <- D_word2[D_word2$word %in% D_wordinfo2$word[i],][1,2:ncol(D_word2)]
}
# extract EM data
D_EM2 <- D_ET_F2[,c(44,45,46,48,49,52,53,57,61,167)]

D_word_res2 <- calIndvalues(D_wordinfo2[,-c(1)], './data/res_F/WordInfo2.csv')
D_EM_res2 <- calIndvalues(D_EM2, './data/res_F/EM2.csv')
