require(ggplot2)
require(plyr)
require(reshape2)
require(sjPlot)
require(Hmisc)
require(GGally)
require(stats)
require(xlsx)
require(lme4)
require(coefplot)
require(broom)
require(gam)
require(MuMIn)
require(influence.ME)

# load data
D2 <- read.csv("./data/res_F/ET_F_ana2.csv", na.strings = "NA")
D2$itemF <- factor(D2$item) # probably won't matter if item is treated as numeric or categorical, but

D2temp <- D2; levels(D2temp$subj) <- c(1:44) # for testing individual influence

# get subset based on subj
struct <- unique(subset(D2temp, select=c(subj)))
struct$ffixurtAvg <- with(D2temp, tapply(ffixurt, subj, mean)) # ffixurt
struct$fpurtAvg <- with(D2temp, tapply(fpurt, subj, mean)) # fpurt
struct$fpregresAvg <- with(D2temp, tapply(fpregres, subj, mean)) # fpregres
struct$fpregres_inAvg <- with(D2temp, tapply(fpregres_in, subj, mean)) # fpregres_in
struct$turtAvg <- with(D2temp, tapply(turt, subj, mean)) # turt

ggplot(struct, aes(x=factor(subj), y=ffixurtAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean ffixurt') + ggtitle('Mean ffixurt across Subjects')
ggsave('ffixurtAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=fpurtAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean fpurt') + ggtitle('Mean fpurt across Subjects')
ggsave('fpurtAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=fpregresAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean fpregres') + ggtitle('Mean fpregres across Subjects')
ggsave('fpregresAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=fpregres_inAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean fpregres_in') + ggtitle('Mean fpregres_in across Subjects')
ggsave('fpregres_inAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=turtAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean turt') + ggtitle('Mean turt across Subjects')
ggsave('turtAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')

### ffixurt
lmer_ffixurt_c <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D2temp)
summary(lmer_ffixurt_c)
# REML criterion at convergence: 267944.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.0345 -0.6023 -0.1787  0.3707  7.8409 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)  274.0   16.553  
# itemF      (Intercept)   12.3    3.508  
# subj       (Intercept)  706.7   26.585  
# Residual               8118.6   90.103  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          236.7320     4.1250   57.39
# oral.comp_bct                        -14.8804     8.4810   -1.75
# decod.comp_bct                        -8.0963     5.9746   -1.36
# readcomp.comp_bct                     11.1887     8.2492    1.36
# printexp.comp_bct                     -5.7275     7.7553   -0.74
# gort.wpm.st                           -6.0284     5.6708   -1.06
# sspan.corr.st                          7.2797     5.7320    1.27
# region_C                               1.9223     0.2750    6.99  9.796573e-12
# std_res_f_coca_log                    -6.2296     0.8996   -6.92  1.594081e-11
# reglen_C                               0.1424     0.4048    0.35
# std_res_prevf_coca_log                -3.7361     0.8883   -4.21  5.65159e-05
# prevwlen_C                             2.3696     0.4196    5.65  4.666887e-08
# std_res_nextf_coca_log                -1.8459     0.8773   -2.10  0.0439836
# nextwlen_C                            -0.2410     0.3972   -0.61
# oral.comp_bct:region_C                 0.2401     0.3979    0.60
# decod.comp_bct:region_C                0.6701     0.2760    2.43  0.02082943
# readcomp.comp_bct:region_C             0.9572     0.3784    2.53  0.01625445
# printexp.comp_bct:region_C            -1.1892     0.3597   -3.31  0.001666569
# gort.wpm.st:region_C                   0.8650     0.2595    3.33  0.001559502
# sspan.corr.st:region_C                -0.7059     0.2690   -2.62  0.01289213
# oral.comp_bct:std_res_f_coca_log      -0.6167     1.2505   -0.49
# decod.comp_bct:std_res_f_coca_log      1.6215     0.8646    1.88
# readcomp.comp_bct:std_res_f_coca_log  -2.7812     1.1938   -2.33  0.02642649
# printexp.comp_bct:std_res_f_coca_log   2.0594     1.1320    1.82
# gort.wpm.st:std_res_f_coca_log        -0.3366     0.8216   -0.41
# sspan.corr.st:std_res_f_coca_log       1.2076     0.8484    1.42
# oral.comp_bct:reglen_C                 1.2506     0.5731    2.18  0.03706291
# decod.comp_bct:reglen_C               -1.1304     0.3931   -2.88  0.006306726
# readcomp.comp_bct:reglen_C            -0.2155     0.5404   -0.40
# printexp.comp_bct:reglen_C             0.4039     0.5119    0.79
# gort.wpm.st:reglen_C                  -0.6657     0.3699   -1.80
# sspan.corr.st:reglen_C                -0.1196     0.3806   -0.31

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: region_C, std_res_f_coca_log, std_res_prevf_coca_log, prevwlen_C
# significant interactions: printexp.comp_bct:region_C*, gort.wpm.st:region_C, decod.comp_bct:reglen_C*

# get dfbetas figure
estex.lmer_ffixurt_c <- influence(lmer_ffixurt_c, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('printexp.comp_bct:region_C', 'gort.wpm.st:region_C', 'decod.comp_bct:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_ffixurt_c, parameters=0)
png("./data/Ind/ffixurtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()
png("./data/Ind/ffixurtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_ffixurt_c)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_ffixurt_c, parameter=0, sort=TRUE)
png("./data/Ind/ffixurtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 12, 16, 25, 15, 4, 18, 13, 22

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/31, df=Inf)
main1 <- sigtest(estex.lmer_ffixurt_c, test=t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_ffixurt_c, test=-t_crit)$'std_res_f_coca_log'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_ffixurt_c, test=-t_crit)$'std_res_prevf_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_ffixurt_c, test=t_crit)$'prevwlen_C'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_ffixurt_c, test=-t_crit)$'printexp.comp_bct:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 4 12 32 40 *
int2 <- sigtest(estex.lmer_ffixurt_c, test=t_crit)$'gort.wpm.st:region_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 6  9 12 18
int3 <- sigtest(estex.lmer_ffixurt_c, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
# *

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('printexp.comp_bct:region_C', 'gort.wpm.st:region_C', 'decod.comp_bct:reglen_C')
resName <- c(paraMain, paraInt) 

ffixurt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(ffixurt_t_DF) <- c('subj', resName)
ffixurt_t_DF$subj[1] <- 0; ffixurt_t_DF[1,resName] <- summary(lmer_ffixurt_c)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_ffixurt_temp <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
  ffixurt_t_DF$subj[i+1] <- subjSet[i]; ffixurt_t_DF[i+1, resName] <- summary(lmer_ffixurt_temp)$coefficients[resName, 3]
}
ffixurt_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(ffixurt_p_DF) <- c('subj', resName)
for(i in 1:nrow(ffixurt_p_DF)){
  ffixurt_p_DF$subj[i] <- ffixurt_t_DF$subj[i]
  for(j in resName){
    ffixurt_p_DF[i,j] <- dt(ffixurt_t_DF[i,j], df=Inf)
  }  
}
write.csv(ffixurt_t_DF, "./data/Ind/ffixurt_t_DF.csv", row.names = FALSE)
write.csv(ffixurt_p_DF, "./data/Ind/ffixurt_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/31
ffixurt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(ffixurt_per_DF) <- resName
for(i in 1:length(resName)){
  ffixurt_per_DF[1, resName[i]] <- sum(ffixurt_p_DF[2:nrow(ffixurt_p_DF),resName[i]]<=p_thres)/(nrow(ffixurt_p_DF)-1)
}
ffixurt_per_DF
# region_C std_res_f_coca_log std_res_prevf_coca_log prevwlen_C printexp.comp_bct:region_C* gort.wpm.st:region_C
#        1                  1                      1          1                   0.4318182            0.4545455
# decod.comp_bct:reglen_C*
#               0.06818182



### fpurt
lmer_fpurt_c <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D2temp)
summary(lmer_fpurt_c)
# REML criterion at convergence: 282148.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2064 -0.6192 -0.1914  0.4182  6.0100 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)   843.02  29.035 
# itemF      (Intercept)    25.16   5.016 
# subj       (Intercept)  1098.11  33.138 
# Residual               15115.12 122.944 
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          279.50416    5.22131   53.53
# oral.comp_bct                        -17.92396   10.59538   -1.69
# decod.comp_bct                       -11.79314    7.46345   -1.58
# readcomp.comp_bct                     11.49311   10.30465    1.12
# printexp.comp_bct                     -8.20364    9.68817   -0.85
# gort.wpm.st                          -17.29959    7.08373   -2.44 0.02032836
# sspan.corr.st                         10.37486    7.16102    1.45
# region_C                               1.22259    0.42877    2.85 0.006872767
# std_res_f_coca_log                   -11.16724    1.42183   -7.85 1.658648e-14
# reglen_C                              12.83364    0.63820   20.11 6.080487e-89
# std_res_prevf_coca_log                -5.02136    1.39468   -3.60 0.0006119019
# prevwlen_C                             1.62979    0.65623    2.48 0.01842331
# std_res_nextf_coca_log                -6.01844    1.36840   -4.40 2.494247e-05
# nextwlen_C                             0.73333    0.61951    1.18
# oral.comp_bct:region_C                 0.14682    0.54321    0.27
# decod.comp_bct:region_C                0.73390    0.37676    1.95
# readcomp.comp_bct:region_C             0.56781    0.51662    1.10
# printexp.comp_bct:region_C            -1.27668    0.49116   -2.60 0.01358297
# gort.wpm.st:region_C                   1.47528    0.35433    4.16 6.967015e-05
# sspan.corr.st:region_C                -0.65504    0.36731   -1.78
# oral.comp_bct:std_res_f_coca_log      -2.54824    1.70713   -1.49
# decod.comp_bct:std_res_f_coca_log      2.07582    1.18019    1.76
# readcomp.comp_bct:std_res_f_coca_log  -2.07309    1.62967   -1.27
# printexp.comp_bct:std_res_f_coca_log   3.03235    1.54550    1.96
# gort.wpm.st:std_res_f_coca_log         2.32775    1.12170    2.08 0.04586108
# sspan.corr.st:std_res_f_coca_log       1.06142    1.15821    0.92
# oral.comp_bct:reglen_C                -0.06153    0.78245   -0.08
# decod.comp_bct:reglen_C               -2.58638    0.53675   -4.82 3.597978e-06
# readcomp.comp_bct:reglen_C            -0.09932    0.73774   -0.13
# printexp.comp_bct:reglen_C            -0.80229    0.69898   -1.15
# gort.wpm.st:reglen_C                  -3.18230    0.50512   -6.30 9.601433e-10
# sspan.corr.st:reglen_C                 2.01973    0.51960    3.89 0.0002065458

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: region_C*, std_res_f_coca_log, reglen_C, std_res_prevf_coca_log, std_res_nextf_coca_log
# significant interactions: gort.wpm.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# get dfbetas figure
estex.lmer_fpurt_c <- influence(lmer_fpurt_c, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_fpurt_c, parameters=0)
png("./data/Ind/fpurtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()
png("./data/Ind/fpurtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_fpurt_c)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpurt_c, parameter=0, sort=TRUE)
png("./data/Ind/fpurtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpurt_c, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 38 44 16 12 4 15 25 41 31 3 2 22 34

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/31, df=Inf)
main1 <- sigtest(estex.lmer_fpurt_c, test=t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_fpurt_c, test=-t_crit)$'std_res_f_coca_log'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_fpurt_c, test=t_crit)$'reglen_C'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_fpurt_c, test=-t_crit)$'std_res_prevf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
main5 <- sigtest(estex.lmer_fpurt_c, test=-t_crit)$'std_res_nextf_coca_log'; which(main5$Altered.Sig == FALSE & main5$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_fpurt_c, test=t_crit)$'gort.wpm.st:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 12
int2 <- sigtest(estex.lmer_fpurt_c, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 38
int3 <- sigtest(estex.lmer_fpurt_c, test=-t_crit)$'gort.wpm.st:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
int4 <- sigtest(estex.lmer_fpurt_c, test=t_crit)$'sspan.corr.st:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
# 2 4 38

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

fpurt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpurt_t_DF) <- c('subj', resName)
fpurt_t_DF$subj[1] <- 0; fpurt_t_DF[1,resName] <- summary(lmer_fpurt_c)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpurt_temp <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
  fpurt_t_DF$subj[i+1] <- subjSet[i]; fpurt_t_DF[i+1, resName] <- summary(lmer_fpurt_temp)$coefficients[resName, 3]
}
fpurt_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpurt_p_DF) <- c('subj', resName)
for(i in 1:nrow(fpurt_p_DF)){
  fpurt_p_DF$subj[i] <- fpurt_t_DF$subj[i]
  for(j in resName){
    fpurt_p_DF[i,j] <- dt(fpurt_t_DF[i,j], df=Inf)
  }  
}
write.csv(fpurt_t_DF, "./data/Ind/fpurt_t_DF.csv", row.names = FALSE)
write.csv(fpurt_p_DF, "./data/Ind/fpurt_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/31
fpurt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpurt_per_DF) <- resName
for(i in 1:length(resName)){
  fpurt_per_DF[1, resName[i]] <- sum(fpurt_p_DF[2:nrow(fpurt_p_DF),resName[i]]<=p_thres)/(nrow(fpurt_p_DF)-1)
}
fpurt_per_DF
# region_C* std_res_f_coca_log reglen_C std_res_prevf_coca_log std_res_nextf_coca_log gort.wpm.st:region_C
# 0.04545455                 1        1                      1                      1            0.9545455
# decod.comp_bct:reglen_C gort.wpm.st:reglen_C sspan.corr.st:reglen_C
#               0.9772727                    1              0.9090909



## fpregres
lmer_fpregres_c <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2temp, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(lmer_fpregres_c)
# AIC      BIC   logLik deviance df.resid 
# 18562.8  18843.7  -9246.4  18492.8    22544 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.9200 -0.4540 -0.3368 -0.2362  5.4142 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.4093   0.6398  
# itemF      (Intercept) 0.0000   0.0000  
# subj       (Intercept) 0.2145   0.4632  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.906053   0.077429 -24.617  < 2e-16 ***
#   oral.comp_bct                        -0.025296   0.152238  -0.166 0.868031    
# decod.comp_bct                        0.011531   0.107017   0.108 0.914192    
# readcomp.comp_bct                     0.110959   0.148205   0.749 0.454046    
# printexp.comp_bct                     0.088366   0.139030   0.636 0.525043    
# gort.wpm.st                          -0.156775   0.101319  -1.547 0.121781    
# sspan.corr.st                        -0.248932   0.102141  -2.437 0.014804 *  
#   region_C                              0.013720   0.009659   1.420 0.155483    
# std_res_f_coca_log                   -0.038224   0.032031  -1.193 0.232744    
# reglen_C                              0.008061   0.014499   0.556 0.578247    
# std_res_prevf_coca_log               -0.099334   0.031740  -3.130 0.001751 ** 
#   prevwlen_C                           -0.127719   0.015500  -8.240  < 2e-16 ***
#   std_res_nextf_coca_log               -0.044077   0.030956  -1.424 0.154496    
# nextwlen_C                            0.016581   0.014073   1.178 0.238707    
# oral.comp_bct:region_C                0.005143   0.012510   0.411 0.680961    
# decod.comp_bct:region_C               0.025530   0.008602   2.968 0.002998 ** 
#   readcomp.comp_bct:region_C            0.012959   0.012203   1.062 0.288245    
# printexp.comp_bct:region_C           -0.002164   0.011247  -0.192 0.847424    
# gort.wpm.st:region_C                  0.012947   0.007834   1.653 0.098393 .  
# sspan.corr.st:region_C               -0.019879   0.007816  -2.543 0.010976 *  
#   oral.comp_bct:std_res_f_coca_log     -0.012833   0.039719  -0.323 0.746632    
# decod.comp_bct:std_res_f_coca_log     0.006105   0.027136   0.225 0.822009    
# readcomp.comp_bct:std_res_f_coca_log  0.052415   0.039047   1.342 0.179480    
# printexp.comp_bct:std_res_f_coca_log -0.048023   0.035911  -1.337 0.181135    
# gort.wpm.st:std_res_f_coca_log       -0.005248   0.025164  -0.209 0.834784    
# sspan.corr.st:std_res_f_coca_log     -0.052165   0.024814  -2.102 0.035531 *  
#   oral.comp_bct:reglen_C               -0.047976   0.018561  -2.585 0.009745 ** 
#   decod.comp_bct:reglen_C               0.008513   0.012639   0.674 0.500606    
# readcomp.comp_bct:reglen_C            0.003703   0.018123   0.204 0.838116    
# printexp.comp_bct:reglen_C            0.051712   0.016741   3.089 0.002008 ** 
#   gort.wpm.st:reglen_C                 -0.042713   0.011771  -3.629 0.000285 ***
#   sspan.corr.st:reglen_C                0.028574   0.011352   2.517 0.011835 *  
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: std_res_prevf_coca_log*, prevwlen_C
# significant interactions: decod.comp_bct:region_C*, sspan.corr.st:region_C*, 
# oral.comp_bct:reglen_C*, printexp.comp_bct:reglen_C*, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C*

# get dfbetas figure
estex.lmer_fpregres_c <- influence(lmer_fpregres_c, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('decod.comp_bct:region_C', 'sspan.corr.st:region_C', 
             'oral.comp_bct:reglen_C', 'printexp.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_fpregres_c, parameters=0)
png("./data/Ind/fpregresAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()
png("./data/Ind/fpregresAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_fpregres_c)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpregres_c, parameter=0, sort=TRUE)
png("./data/Ind/fpregresAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 2, 15, 16, 18, 31, 38

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/31, df=Inf)
main1 <- sigtest(estex.lmer_fpregres_c, test=-t_crit)$'std_res_prevf_coca_log'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
# 7 15 31
main2 <- sigtest(estex.lmer_fpregres_c, test=-t_crit)$'prevwlen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_fpregres_c, test=t_crit)$'decod.comp_bct:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 2  3  4  7  8 13 15 16 18 19 21 25 27 28 30 32 34 35 38 40 42
int2 <- sigtest(estex.lmer_fpregres_c, test=-t_crit)$'sspan.corr.st:region_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
int3 <- sigtest(estex.lmer_fpregres_c, test=-t_crit)$'oral.comp_bct:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
int4 <- sigtest(estex.lmer_fpregres_c, test=t_crit)$'printexp.comp_bct:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
# 6 10 12 16 29 38 40
int5 <- sigtest(estex.lmer_fpregres_c, test=-t_crit)$'gort.wpm_S:reglen_C'; which(int5$Altered.Sig == FALSE & int5$Changed.Sig == TRUE)
int6 <- sigtest(estex.lmer_fpregres_c, test=t_crit)$'sspan.corr.st:reglen_C'; which(int6$Altered.Sig == FALSE & int6$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('decod.comp_bct:region_C', 'sspan.corr.st:region_C', 
             'oral.comp_bct:reglen_C', 'printexp.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

fpregres_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpregres_p_DF) <- c('subj', resName)
fpregres_p_DF$subj[1] <- 0; fpregres_p_DF[1,resName] <- summary(lmer_fpregres_c)$coefficients[resName, 4]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpregres_temp <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],], family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)), nAGQ = 1)
  fpregres_p_DF$subj[i+1] <- subjSet[i]; fpregres_p_DF[i+1, resName] <- summary(lmer_fpregres_temp)$coefficients[resName, 4]
}
write.csv(fpregres_p_DF, "./data/Ind/fpregres_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/31
fpregres_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpregres_per_DF) <- resName
for(i in 1:length(resName)){
  fpregres_per_DF[1, resName[i]] <- sum(fpregres_p_DF[2:nrow(fpregres_p_DF),resName[i]]<=p_thres)/(nrow(fpregres_p_DF)-1)
}
fpregres_per_DF
# std_res_prevf_coca_log* prevwlen_C decod.comp_bct:region_C* sspan.corr.st:region_C* oral.comp_bct:reglen_C*
#               0.4090909          1                0.1818182              0.02272727                       0
# printexp.comp_bct:reglen_C* gort.wpm.st:reglen_C sspan.corr.st:reglen_C*
#                   0.1590909            0.8863636                       0



## fpregres_in
lmer_fpregres_in_c <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1|subj), data = D2temp, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(lmer_fpregres_in_c)
# AIC      BIC   logLik deviance df.resid 
# 22922.8  23203.7 -11426.4  22852.8    22544 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8266 -0.5672 -0.4184 -0.2381  5.0198 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.18342  0.4283  
# itemF      (Intercept) 0.03991  0.1998  
# subj       (Intercept) 0.32099  0.5666  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.3715035  0.0920558 -14.899  < 2e-16 ***
#   oral.comp_bct                        -0.0325622  0.1825851  -0.178  0.85846    
# decod.comp_bct                        0.0123659  0.1284997   0.096  0.92334    
# readcomp.comp_bct                     0.0843924  0.1774346   0.476  0.63434    
# printexp.comp_bct                    -0.0726625  0.1668409  -0.436  0.66319    
# gort.wpm.st                          -0.0827487  0.1217533  -0.680  0.49673    
# sspan.corr.st                        -0.0279157  0.1229515  -0.227  0.82039    
# region_C                             -0.0467582  0.0074756  -6.255 3.98e-10 ***
#   std_res_f_coca_log                   -0.0777159  0.0247428  -3.141  0.00168 ** 
#   reglen_C                              0.1029857  0.0110755   9.299  < 2e-16 ***
#   std_res_prevf_coca_log               -0.0990621  0.0244238  -4.056 4.99e-05 ***
#   prevwlen_C                           -0.0526540  0.0117235  -4.491 7.08e-06 ***
#   std_res_nextf_coca_log               -0.1119410  0.0241396  -4.637 3.53e-06 ***
#   nextwlen_C                            0.0262190  0.0109585   2.393  0.01673 *  
#   oral.comp_bct:region_C                0.0074611  0.0110403   0.676  0.49916    
# decod.comp_bct:region_C               0.0121784  0.0074954   1.625  0.10421    
# readcomp.comp_bct:region_C           -0.0091370  0.0104615  -0.873  0.38245    
# printexp.comp_bct:region_C            0.0179152  0.0098683   1.815  0.06946 .  
# gort.wpm.st:region_C                 -0.0083514  0.0068360  -1.222  0.22183    
# sspan.corr.st:region_C               -0.0184640  0.0069555  -2.655  0.00794 ** 
#   oral.comp_bct:std_res_f_coca_log      0.0490234  0.0349327   1.403  0.16051    
# decod.comp_bct:std_res_f_coca_log    -0.0053962  0.0236704  -0.228  0.81967    
# readcomp.comp_bct:std_res_f_coca_log -0.0306571  0.0333325  -0.920  0.35771    
# printexp.comp_bct:std_res_f_coca_log -0.0277978  0.0312344  -0.890  0.37348    
# gort.wpm.st:std_res_f_coca_log        0.0003269  0.0217997   0.015  0.98804    
# sspan.corr.st:std_res_f_coca_log     -0.0176489  0.0220627  -0.800  0.42374    
# oral.comp_bct:reglen_C               -0.0113461  0.0158890  -0.714  0.47518    
# decod.comp_bct:reglen_C              -0.0022373  0.0106649  -0.210  0.83384    
# readcomp.comp_bct:reglen_C            0.0197742  0.0149889   1.319  0.18708    
# printexp.comp_bct:reglen_C            0.0056372  0.0140056   0.402  0.68732    
# gort.wpm.st:reglen_C                  0.0163309  0.0097185   1.680  0.09288 .  
# sspan.corr.st:reglen_C                0.0243391  0.0098168   2.479  0.01316 *   
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: region_C, reglen_C, std_res_f_coca_log*, std_res_prevf_coca_log, prevwlen_C, std_res_nextf_coca_log

# get dfbetas figure
estex.lmer_fpregres_in_c <- influence(lmer_fpregres_in_c, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'reglen_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C', 'std_res_nextf_coca_log')
dfbetasAll <- dfbetas(estex.lmer_spurt_c, parameters=0)
png("./data/Ind/fpregres_inAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_in_c, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()

pchange(estex.lmer_fpregres_in_c)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpregres_in_c, parameter=0, sort=TRUE)
png("./data/Ind/fpregres_inAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_in_c, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 14, 18, 35, 37

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/31, df=Inf)
main1 <- sigtest(estex.lmer_fpregres_in_c, test=-t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main1 <- sigtest(estex.lmer_fpregres_in_c, test=t_crit)$'reglen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_fpregres_in_c, test=-t_crit)$'std_res_f_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_fpregres_in_c, test=-t_crit)$'std_res_prevf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
main5 <- sigtest(estex.lmer_fpregres_in_c, test=-t_crit)$'prevwlen_C'; which(main5$Altered.Sig == FALSE & main5$Changed.Sig == TRUE)
main6 <- sigtest(estex.lmer_fpregres_in_c, test=-t_crit)$'std_res_nextf_coca_log'; which(main6$Altered.Sig == FALSE & main6$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'reglen_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C', 'std_res_nextf_coca_log')
resName <- c(paraMain) 

fpregres_in_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpregres_in_p_DF) <- c('subj', resName)
fpregres_in_p_DF$subj[1] <- 0; fpregres_in_p_DF[1,resName] <- summary(lmer_fpregres_in_c)$coefficients[resName, 4]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpregres_in_temp <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],], family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)), nAGQ = 1)
  fpregres_in_p_DF$subj[i+1] <- subjSet[i]; fpregres_in_p_DF[i+1, resName] <- summary(lmer_fpregres_in_temp)$coefficients[resName, 4]
}
write.csv(fpregres_in_p_DF, "./data/Ind/fpregres_in_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/31
fpregres_in_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpregres_in_per_DF) <- resName
for(i in 1:length(resName)){
  fpregres_in_per_DF[1, resName[i]] <- sum(fpregres_in_p_DF[2:nrow(fpregres_in_p_DF),resName[i]]<=p_thres)/(nrow(fpregres_in_p_DF)-1)
}
fpregres_in_per_DF
# region_C reglen_C std_res_f_coca_log* std_res_prevf_coca_log prevwlen_C std_res_nextf_coca_log
#        1        1           0.3863636                      1          1                      1



# turt
lmer_turt_c <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1|subj), data = D2temp)
summary(lmer_turt_c)
# REML criterion at convergence: 303683.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7782 -0.5926 -0.1957  0.3512 12.2049 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)  2602.4   51.01  
# itemF      (Intercept)   305.2   17.47  
# subj       (Intercept)  3878.6   62.28  
# Residual               39075.9  197.68  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          354.2996     9.9050   35.77
# oral.comp_bct                        -22.6937    19.8408   -1.14
# decod.comp_bct                       -16.6603    13.9781   -1.19
# readcomp.comp_bct                     20.6958    19.2997    1.07
# printexp.comp_bct                    -10.2418    18.1438   -0.56
# gort.wpm.st                          -27.4273    13.2674   -2.07  0.04682264
# sspan.corr.st                          9.4142    13.4096    0.70
# region_C                              -1.8519     0.7321   -2.53  0.01625445
# std_res_f_coca_log                   -19.5815     2.4647   -7.94  8.150162e-15
# reglen_C                              21.0011     1.1032   19.04  7.591801e-80
# std_res_prevf_coca_log               -13.1368     2.4097   -5.45  1.416101e-07
# prevwlen_C                            -1.7768     1.1275   -1.58
# std_res_nextf_coca_log               -13.2177     2.3541   -5.61  5.845617e-08
# nextwlen_C                             2.6389     1.0657    2.48  0.01842331
# oral.comp_bct:region_C                 1.4430     0.8736    1.65
# decod.comp_bct:region_C                1.2265     0.6059    2.02
# readcomp.comp_bct:region_C             0.0280     0.8308    0.03
# printexp.comp_bct:region_C             0.2707     0.7899    0.34
# gort.wpm.st:region_C                   1.6871     0.5698    2.96  0.004992899
# sspan.corr.st:region_C                -2.3095     0.5907   -3.91  0.0001910475
# oral.comp_bct:std_res_f_coca_log       0.6322     2.7454    0.23
# decod.comp_bct:std_res_f_coca_log      2.3271     1.8980    1.23
# readcomp.comp_bct:std_res_f_coca_log  -3.9712     2.6208   -1.52
# printexp.comp_bct:std_res_f_coca_log   3.2925     2.4856    1.32
# gort.wpm.st:std_res_f_coca_log         3.7630     1.8040    2.09  0.04491477
# sspan.corr.st:std_res_f_coca_log      -1.4295     1.8626   -0.77
# oral.comp_bct:reglen_C                -1.7742     1.2584   -1.41
# decod.comp_bct:reglen_C               -3.7397     0.8633   -4.33  3.38562e-05
# readcomp.comp_bct:reglen_C             1.1319     1.1865    0.95
# printexp.comp_bct:reglen_C             0.7756     1.1242    0.69
# gort.wpm.st:reglen_C                  -4.1114     0.8125   -5.06  1.099408e-06
# sspan.corr.st:reglen_C                 4.2935     0.8357    5.14  7.310832e-07

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: std_res_f_coca_log, reglen_C, std_res_prevf_coca_log, std_res_nextf_coca_log
# significant interactions: gort.wpm.st:region_C*, sspan.corr.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# get dfbetas figure
estex.lmer_turt_c <- influence(lmer_turt_c, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'sspan.corr.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_turt_c, parameters=0)
png("./data/Ind/turtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()
png("./data/Ind/turtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_turt_c)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_turt_c, parameter=0, sort=TRUE)
png("./data/Ind/turtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 37 38 44 15 14 18 25 12 19 4 3 31 40 5 34

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/31, df=Inf)
main1 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'std_res_f_coca_log'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_turt_c, test=t_crit)$'reglen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'std_res_prevf_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'std_res_nextf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
# significant interactions: gort.wpm.st:region_C*, sspan.corr.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C
int1 <- sigtest(estex.lmer_turt_c, test=t_crit)$'gort.wpm.st:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 1  4  6  9 10 12 14 15 16 17 20 21 23 24 25 26 27 28 32 33 36 37 38 39 40 43 44
int2 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'sspan.corr.st:region_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 4  5  6  7 10 11 13 14 15 16 17 24 26 29 32 36 40 41 44
int3 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
# 38
int4 <- sigtest(estex.lmer_turt_c, test=-t_crit)$'gort.wpm.st:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
int5 <- sigtest(estex.lmer_turt_c, test=t_crit)$'sspan.corr.st:reglen_C'; which(int5$Altered.Sig == FALSE & int5$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'sspan.corr.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

turt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(turt_t_DF) <- c('subj', resName)
turt_t_DF$subj[1] <- 0; turt_t_DF[1,resName] <- summary(lmer_turt_c)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_turt_temp <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
  turt_t_DF$subj[i+1] <- subjSet[i]; turt_t_DF[i+1, resName] <- summary(lmer_turt_temp)$coefficients[resName, 3]
}
turt_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(turt_p_DF) <- c('subj', resName)
for(i in 1:nrow(turt_p_DF)){
  turt_p_DF$subj[i] <- turt_t_DF$subj[i]
  for(j in resName){
    turt_p_DF[i,j] <- dt(turt_t_DF[i,j], df=Inf)
  }  
}
write.csv(turt_t_DF, "./data/Ind/turt_t_DF.csv", row.names = FALSE)
write.csv(turt_p_DF, "./data/Ind/turt_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/31
turt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(turt_per_DF) <- resName
for(i in 1:length(resName)){
  turt_per_DF[1, resName[i]] <- sum(turt_p_DF[2:nrow(turt_p_DF),resName[i]]<=p_thres)/(nrow(turt_p_DF)-1)
}
turt_per_DF
# std_res_f_coca_log reglen_C std_res_prevf_coca_log std_res_nextf_coca_log 
#                  1        1                      1                      1           
# gort.wpm.st:region_C* sspan.corr.st:region_C decod.comp_bct:reglen_C gort.wpm.st:reglen_C sspan.corr.st:reglen_C
#            0.09090909              0.9318182               0.9772727                    1                      1
