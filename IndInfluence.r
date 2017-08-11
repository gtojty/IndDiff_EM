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
ggplot(struct, aes(x=factor(subj), y=turtAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean turt') + ggtitle('Mean turt across Subjects')
ggsave('turtAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=fpregresAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean fpregres') + ggtitle('Mean fpregres across Subjects')
ggsave('fpregresAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(struct, aes(x=factor(subj), y=fpregres_inAvg), color = 'black') + geom_line(aes(group=1)) + geom_point(shape=1, size=3) + 
  xlab('Subject ID') + ylab('mean fpregres_in') + ggtitle('Mean fpregres_in across Subjects')
ggsave('fpregres_inAvg_sub.png', dpi = 300, height = 6, width = 12, units = 'in')

### ffixurt
lmer_ffixurt_c_noRC_slope <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp)

summary(lmer_ffixurt_c_noRC_slope)
# REML criterion at convergence: 267942.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8835 -0.6039 -0.1778  0.3712  7.8264 
# 
# Random effects:
#   Groups     Name               Variance Std.Dev. Corr 
# word:itemF (Intercept)         274.43  16.566        
# itemF      (Intercept)          12.34   3.512        
# subj       (Intercept)         723.43  26.897        
# std_res_f_coca_log   24.50   4.950   -0.21
# Residual                      8099.93  90.000        
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          236.7428     4.1708   56.76
# oral.comp_bct                         -8.0879     6.8993   -1.17
# decod.comp_bct                        -6.6764     5.9460   -1.12
# printexp.comp_bct                     -3.6163     7.6975   -0.47
# gort.wpm.st                           -7.0951     5.6834   -1.25
# sspan.corr.st                          8.5527     5.7188    1.50
# region_C                               1.9132     0.2749    6.96  1.207676e-11
# std_res_f_coca_log                    -6.1867     1.1696   -5.29  3.343797e-07
# reglen_C                               0.1398     0.4048    0.35
# std_res_prevf_coca_log                -3.7318     0.8881   -4.20  5.894307e-05
# prevwlen_C                             2.3754     0.4196    5.66  4.410298e-08
# std_res_nextf_coca_log                -1.8680     0.8772   -2.13
# nextwlen_C                            -0.2478     0.3971   -0.62
# oral.comp_bct:region_C                 0.8389     0.3190    2.63  
# decod.comp_bct:region_C                0.7865     0.2718    2.89  0.006127377*
# printexp.comp_bct:region_C            -1.0276     0.3533   -2.91  0.005782099*
# gort.wpm.st:region_C                   0.7809     0.2567    3.04  0.003927554*
# sspan.corr.st:region_C                -0.5816     0.2640   -2.20
# oral.comp_bct:std_res_f_coca_log      -2.5153     1.6092   -1.56
# decod.comp_bct:std_res_f_coca_log      1.1650     1.3781    0.85
# printexp.comp_bct:std_res_f_coca_log   1.7565     1.7898    0.98
# gort.wpm.st:std_res_f_coca_log        -0.1400     1.3168   -0.11
# sspan.corr.st:std_res_f_coca_log       0.9654     1.3342    0.72
# oral.comp_bct:reglen_C                 1.1327     0.4564    2.48
# decod.comp_bct:reglen_C               -1.1479     0.3871   -2.97  0.004847033*
# printexp.comp_bct:reglen_C             0.3420     0.5037    0.68
# gort.wpm.st:reglen_C                  -0.6361     0.3663   -1.74
# sspan.corr.st:reglen_C                -0.1484     0.3745   -0.40

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/27=0.00185)
# significant main effects: region_C, std_res_f_coca_log, std_res_prevf_coca_log, prevwlen_C
# significant interactions: printexp.comp_bct:region_C*, gort.wpm.st:region_C*, decod.comp_bct:region_C*, decod.comp_bct:reglen_C*

# get dfbetas figure
estex.lmer_ffixurt_c_noRC_slope <- influence(lmer_ffixurt_c_noRC_slope, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('printexp.comp_bct:region_C', 'gort.wpm.st:region_C', 'decod.comp_bct:region_C', 'decod.comp_bct:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_ffixurt_c_noRC_slope, parameters=0)
png("./data/Ind/ffixurtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c_noRC_slope, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()
png("./data/Ind/ffixurtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c_noRC_slope, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_ffixurt_c_noRC_slope)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_ffixurt_c_noRC_slope, parameter=0, sort=TRUE)
png("./data/Ind/ffixurtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c_noRC_slope, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 12, 16, 15, 4, 18, 13, 22

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/27, df=Inf)
main1 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=-t_crit)$'std_res_f_coca_log'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=-t_crit)$'std_res_prevf_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=t_crit)$'prevwlen_C'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=-t_crit)$'printexp.comp_bct:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 4  6  9 11 12 14 15 18 20 21 23 24 25 26 27 29 30 32 33 35 38 39 40 41 44
int2 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=t_crit)$'gort.wpm.st:region_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 6  9 10 12 15 18 23 24 25 32 35
int3 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=t_crit)$'decod.comp_bct:region_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
int4 <- sigtest(estex.lmer_ffixurt_c_noRC_slope, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
# 1  3  8  9 12 16 17 21 23 24 25 28 29 37 38

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('printexp.comp_bct:region_C', 'gort.wpm.st:region_C', 'decod.comp_bct:region_C', 'decod.comp_bct:reglen_C')
resName <- c(paraMain, paraInt) 

ffixurt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(ffixurt_t_DF) <- c('subj', resName)
ffixurt_t_DF$subj[1] <- 0; ffixurt_t_DF[1,resName] <- summary(lmer_ffixurt_c_noRC_slope)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_ffixurt_temp <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
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

p_thres <- 0.05/27
ffixurt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(ffixurt_per_DF) <- resName
for(i in 1:length(resName)){
  ffixurt_per_DF[1, resName[i]] <- sum(ffixurt_p_DF[2:nrow(ffixurt_p_DF),resName[i]]<=p_thres)/(nrow(ffixurt_p_DF)-1)
}
ffixurt_per_DF
# region_C std_res_f_coca_log std_res_prevf_coca_log prevwlen_C printexp.comp_bct:region_C gort.wpm.st:region_C
#        1                  1                      1          1                 0.06818182           0.09090909
# decod.comp_bct:region_C decod.comp_bct:reglen_C
#              0.06818182              0.06818182



### fpurt
lmer_fpurt_c_noRC_slope <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp)

summary(lmer_fpurt_c_noRC_slope)
# REML criterion at convergence: 282152.7
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2235 -0.6176 -0.1907  0.4164  6.0179 
# 
# Random effects:
#   Groups     Name               Variance Std.Dev. Corr 
# word:itemF (Intercept)          843.77  29.048       
# itemF      (Intercept)           25.29   5.029       
# subj       (Intercept)         1102.83  33.209       
# std_res_f_coca_log    26.76   5.173  -0.26
# Residual                      15091.23 122.846       
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          279.5049     5.2318   53.42
# oral.comp_bct                        -10.9398     8.5392   -1.28
# decod.comp_bct                       -10.3100     7.3588   -1.40
# printexp.comp_bct                     -6.0541     9.5267   -0.64
# gort.wpm.st                          -18.3619     7.0335   -2.61  
# sspan.corr.st                         11.6843     7.0781    1.65
# region_C                               1.2122     0.4288    2.83  0.007274439*
# std_res_f_coca_log                   -11.1350     1.6226   -6.86  2.41017e-11
# reglen_C                              12.8265     0.6382   20.10  7.434525e-89
# std_res_prevf_coca_log                -5.0221     1.3947   -3.60  0.0006119019
# prevwlen_C                             1.6387     0.6563    2.50
# std_res_nextf_coca_log                -6.0230     1.3684   -4.40  2.494247e-05
# nextwlen_C                             0.7327     0.6195    1.18
# oral.comp_bct:region_C                 0.4975     0.4357    1.14
# decod.comp_bct:region_C                0.8041     0.3712    2.17
# printexp.comp_bct:region_C            -1.1715     0.4825   -2.43
# gort.wpm.st:region_C                   1.4256     0.3506    4.07  0.0001008992
# sspan.corr.st:region_C                -0.5817     0.3605   -1.61
# oral.comp_bct:std_res_f_coca_log      -3.9310     1.8996   -2.07
# decod.comp_bct:std_res_f_coca_log      1.7653     1.6233    1.09
# printexp.comp_bct:std_res_f_coca_log   2.7767     2.1107    1.32
# gort.wpm.st:std_res_f_coca_log         2.5046     1.5509    1.61
# sspan.corr.st:std_res_f_coca_log       0.8957     1.5751    0.57
# oral.comp_bct:reglen_C                -0.1028     0.6233   -0.16
# decod.comp_bct:reglen_C               -2.5764     0.5286   -4.87  2.823909e-06
# printexp.comp_bct:reglen_C            -0.8548     0.6879   -1.24
# gort.wpm.st:reglen_C                  -3.1788     0.5004   -6.35  6.998266e-10
# sspan.corr.st:reglen_C                 1.9939     0.5115    3.90  0.0001986555

# summary: (*: p value is smaller than 0.05/5=0.01 but bigger than 0.05/27=0.00185)
# significant main effects: region_C*, std_res_f_coca_log, reglen_C, std_res_prevf_coca_log, std_res_nextf_coca_log
# significant interactions: gort.wpm.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# get dfbetas figure
estex.lmer_fpurt_c_noRC_slope <- influence(lmer_fpurt_c_noRC_slope, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_fpurt_c_noRC_slope, parameters=0)
png("./data/Ind/fpurtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c_noRC_slope, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()
png("./data/Ind/fpurtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_ffixurt_c_noRC_slope, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_fpurt_c_noRC_slope)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpurt_c_noRC_slope, parameter=0, sort=TRUE)
png("./data/Ind/fpurtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpurt_c_noRC_slope, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 38 16 44 12 4 31 15 3 2 22 34

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/27, df=Inf)
main1 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=-t_crit)$'std_res_f_coca_log'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=t_crit)$'reglen_C'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=-t_crit)$'std_res_prevf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
main5 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=-t_crit)$'std_res_nextf_coca_log'; which(main5$Altered.Sig == FALSE & main5$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=t_crit)$'gort.wpm.st:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
int2 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 38
int3 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=-t_crit)$'gort.wpm.st:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
int4 <- sigtest(estex.lmer_fpurt_c_noRC_slope, test=t_crit)$'sspan.corr.st:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
# 2 4 38

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

fpurt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpurt_t_DF) <- c('subj', resName)
fpurt_t_DF$subj[1] <- 0; fpurt_t_DF[1,resName] <- summary(lmer_fpurt_c_noRC_slope)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpurt_temp <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
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

p_thres <- 0.05/27
fpurt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpurt_per_DF) <- resName
for(i in 1:length(resName)){
  fpurt_per_DF[1, resName[i]] <- sum(fpurt_p_DF[2:nrow(fpurt_p_DF),resName[i]]<=p_thres)/(nrow(fpurt_p_DF)-1)
}
fpurt_per_DF
# region_C std_res_f_coca_log reglen_C std_res_prevf_coca_log std_res_nextf_coca_log gort.wpm.st:region_C
# 0.04545455                1        1                      1                      1            0.9545455
# decod.comp_bct:reglen_C gort.wpm.st:reglen_C sspan.corr.st:reglen_C
#               0.9772727                    1              0.9318182



# turt
lmer_turt_c_noRC_slope <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp)

summary(lmer_turt_c_noRC_slope)
# REML criterion at convergence: 303680.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8110 -0.5921 -0.1947  0.3516 12.1972 
# 
# Random effects:
#   Groups     Name               Variance Std.Dev. Corr 
# word:itemF (Intercept)         2603.7   51.03        
# itemF      (Intercept)          307.9   17.55        
# subj       (Intercept)         3875.9   62.26        
# std_res_f_coca_log   104.8   10.24   -0.46
# Residual                      38985.6  197.45        
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          354.3081     9.9038   35.77
# oral.comp_bct                        -10.1911    15.9519   -0.64
# decod.comp_bct                       -14.0007    13.7484   -1.02
# printexp.comp_bct                     -6.3545    17.7979   -0.36
# gort.wpm.st                          -29.2645    13.1414   -2.23
# sspan.corr.st                         11.7321    13.2227    0.89
# region_C                              -1.8676     0.7319   -2.55  
# std_res_f_coca_log                   -19.6623     2.9096   -6.76  4.762142e-11
# reglen_C                              20.9898     1.1031   19.03  9.183597e-80
# std_res_prevf_coca_log               -13.1164     2.4095   -5.44  1.495345e-07
# prevwlen_C                            -1.7426     1.1274   -1.55
# std_res_nextf_coca_log               -13.2066     2.3538   -5.61  5.845617e-08
# nextwlen_C                             2.6358     1.0656    2.47
# oral.comp_bct:region_C                 1.4485     0.7004    2.07
# decod.comp_bct:region_C                1.2312     0.5967    2.06
# printexp.comp_bct:region_C             0.2871     0.7757    0.37
# gort.wpm.st:region_C                   1.6980     0.5637    3.01  0.004300652*
# sspan.corr.st:region_C                -2.3004     0.5795   -3.97  0.0001508253
# oral.comp_bct:std_res_f_coca_log      -2.1206     3.4095   -0.62
# decod.comp_bct:std_res_f_coca_log      1.6869     2.9187    0.58
# printexp.comp_bct:std_res_f_coca_log   2.8270     3.7917    0.75
# gort.wpm.st:std_res_f_coca_log         4.1414     2.7890    1.48
# sspan.corr.st:std_res_f_coca_log      -1.6200     2.8269   -0.57
# oral.comp_bct:reglen_C                -0.9979     1.0022   -1.00
# decod.comp_bct:reglen_C               -3.5619     0.8499   -4.19  6.146833e-05
# printexp.comp_bct:reglen_C             0.8995     1.1061    0.81
# gort.wpm.st:reglen_C                  -4.2459     0.8045   -5.28  3.52527e-07
# sspan.corr.st:reglen_C                 4.4038     0.8223    5.36  2.303333e-07  

# summary: (*: p value is smaller than 0.05/5=0.01 but bigger than 0.05/27=0.00185)
# significant main effects: std_res_f_coca_log, reglen_C, std_res_prevf_coca_log, std_res_nextf_coca_log
# significant interactions: gort.wpm.st:region_C*, sspan.corr.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# get dfbetas figure
estex.lmer_turt_c_noRC_slope <- influence(lmer_turt_c_noRC_slope, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'sspan.corr.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_turt_c_noRC_slope, parameters=0)
png("./data/Ind/turtAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c_noRC_slope, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()
png("./data/Ind/turtAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c_noRC_slope, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_turt_c_noRC_slope)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_turt_c_noRC_slope, parameter=0, sort=TRUE)
png("./data/Ind/turtAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_turt_c_noRC_slope, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 37 38 44 14 18 15 12 4 3 31 40

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/27, df=Inf)
main1 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'std_res_f_coca_log'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_turt_c_noRC_slope, test=t_crit)$'reglen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'std_res_prevf_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
main4 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'std_res_nextf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
# significant interactions: gort.wpm.st:region_C*, sspan.corr.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C
int1 <- sigtest(estex.lmer_turt_c_noRC_slope, test=t_crit)$'gort.wpm.st:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 1  6  9 10 12 16 24 25 26 33 37 38 40
int2 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'sspan.corr.st:region_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 4  14
int3 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'decod.comp_bct:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
# 38
int4 <- sigtest(estex.lmer_turt_c_noRC_slope, test=-t_crit)$'gort.wpm.st:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
int5 <- sigtest(estex.lmer_turt_c_noRC_slope, test=t_crit)$'sspan.corr.st:reglen_C'; which(int5$Altered.Sig == FALSE & int5$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('std_res_f_coca_log', 'reglen_C', 'std_res_prevf_coca_log', 'std_res_nextf_coca_log')
paraInt <- c('gort.wpm.st:region_C', 'sspan.corr.st:region_C', 'decod.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

turt_t_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(turt_t_DF) <- c('subj', resName)
turt_t_DF$subj[1] <- 0; turt_t_DF[1,resName] <- summary(lmer_turt_c_noRC_slope)$coefficients[resName, 3]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_turt_temp <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],])
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

p_thres <- 0.05/27
turt_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(turt_per_DF) <- resName
for(i in 1:length(resName)){
  turt_per_DF[1, resName[i]] <- sum(turt_p_DF[2:nrow(turt_p_DF),resName[i]]<=p_thres)/(nrow(turt_p_DF)-1)
}
turt_per_DF
# std_res_f_coca_log reglen_C std_res_prevf_coca_log std_res_nextf_coca_log gort.wpm.st:region_C sspan.corr.st:region_C
#                  1        1                      1                      1            0.1363636              0.9318182
# decod.comp_bct:reglen_C gort.wpm.st:reglen_C sspan.corr.st:reglen_C
#               0.9772727                    1                      1


## fpregres
lmer_fpregres_c_noRC_slope <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

summary(lmer_fpregres_c_noRC_slope)
# AIC      BIC   logLik deviance df.resid 
# 18561.4  18826.3  -9247.7  18495.4    22546 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8398 -0.4531 -0.3371 -0.2357  5.4881 
# 
# Random effects:
#   Groups     Name               Variance  Std.Dev. Corr
# word:itemF (Intercept)        0.4094704 0.63990      
# itemF      (Intercept)        0.0000000 0.00000      
# subj       (Intercept)        0.2187999 0.46776      
# std_res_f_coca_log 0.0003179 0.01783  1.00
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.9061659  0.0780631 -24.418  < 2e-16 ***
#   oral.comp_bct                         0.0435248  0.1232451   0.353 0.723971    
# decod.comp_bct                        0.0266332  0.1061273   0.251 0.801849    
# printexp.comp_bct                     0.1067550  0.1378828   0.774 0.438786    
# gort.wpm.st                          -0.1667823  0.1013680  -1.645 0.099905 .  
# sspan.corr.st                        -0.2365311  0.1017212  -2.325 0.020057 *  
#   region_C                              0.0140199  0.0096592   1.451 0.146651    
# std_res_f_coca_log                   -0.0417919  0.0324654  -1.287 0.197999    
# reglen_C                              0.0080774  0.0144971   0.557 0.577411    
# std_res_prevf_coca_log               -0.0993776  0.0317445  -3.131 0.001745 ** 
#   prevwlen_C                           -0.1276949  0.0155028  -8.237  < 2e-16 ***
#   std_res_nextf_coca_log               -0.0442482  0.0309592  -1.429 0.152935    
# nextwlen_C                            0.0166146  0.0140744   1.180 0.237810    
# oral.comp_bct:region_C                0.0135652  0.0097302   1.394 0.163278    
# decod.comp_bct:region_C               0.0279714  0.0082747   3.380 0.000724 ***
#   printexp.comp_bct:region_C           -0.0008031  0.0111666  -0.072 0.942666    
# gort.wpm.st:region_C                  0.0120473  0.0077836   1.548 0.121677    
# sspan.corr.st:region_C               -0.0185491  0.0077091  -2.406 0.016122 *  
#   oral.comp_bct:std_res_f_coca_log      0.0222112  0.0313185   0.709 0.478197    
# decod.comp_bct:std_res_f_coca_log     0.0160266  0.0264007   0.607 0.543817    
# printexp.comp_bct:std_res_f_coca_log -0.0432423  0.0359773  -1.202 0.229390    
# gort.wpm.st:std_res_f_coca_log       -0.0081201  0.0253255  -0.321 0.748492    
# sspan.corr.st:std_res_f_coca_log     -0.0469346  0.0247703  -1.895 0.058119 .  
# oral.comp_bct:reglen_C               -0.0458811  0.0144134  -3.183 0.001456 ** 
#   decod.comp_bct:reglen_C               0.0093207  0.0121225   0.769 0.441963    
# printexp.comp_bct:reglen_C            0.0522706  0.0166508   3.139 0.001694 ** 
#   gort.wpm.st:reglen_C                 -0.0426821  0.0117013  -3.648 0.000265 ***
#   sspan.corr.st:reglen_C                0.0289538  0.0112294   2.578 0.009926 ** 

# summary: (*: p value is smaller than 0.05/5=0.01 but bigger than 0.05/27=0.00185)
# significant main effects: std_res_prevf_coca_log, prevwlen_C
# significant interactions: decod.comp_bct:region_C, oral.comp_bct:reglen_C, printexp.comp_bct:reglen_C, gort.wpm_S:reglen_C, sspan.corr.st:reglen_C*

# get dfbetas figure
estex.lmer_fpregres_c_noRC_slope <- influence(lmer_fpregres_c_noRC_slope, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('decod.comp_bct:region_C', 'oral.comp_bct:reglen_C', 'printexp.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_fpregres_c_noRC_slope, parameters=0)
png("./data/Ind/fpregresAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c_noRC_slope, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()
png("./data/Ind/fpregresAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c_noRC_slope, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_fpregres_c_noRC_slope)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpregres_c_noRC_slope, parameter=0, sort=TRUE)
png("./data/Ind/fpregresAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_c_noRC_slope, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 12, 7, 39, 18, 29

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/27, df=Inf)
main1 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=-t_crit)$'std_res_prevf_coca_log'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
# 15 31
main2 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=-t_crit)$'prevwlen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=t_crit)$'decod.comp_bct:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
# 7  13 28 34 35
int2 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=-t_crit)$'oral.comp_bct:reglen_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)
# 2 12 18 20 37 40
int3 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=t_crit)$'printexp.comp_bct:reglen_C'; which(int3$Altered.Sig == FALSE & int3$Changed.Sig == TRUE)
# 6 10 12 38 40
int4 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=-t_crit)$'gort.wpm_S:reglen_C'; which(int4$Altered.Sig == FALSE & int4$Changed.Sig == TRUE)
int5 <- sigtest(estex.lmer_fpregres_c_noRC_slope, test=t_crit)$'sspan.corr.st:reglen_C'; which(int5$Altered.Sig == FALSE & int5$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('std_res_prevf_coca_log', 'prevwlen_C')
paraInt <- c('decod.comp_bct:region_C', 'oral.comp_bct:reglen_C', 'printexp.comp_bct:reglen_C', 'gort.wpm.st:reglen_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

fpregres_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpregres_p_DF) <- c('subj', resName)
fpregres_p_DF$subj[1] <- 0; fpregres_p_DF[1,resName] <- summary(lmer_fpregres_c_noRC_slope)$coefficients[resName, 4]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpregres_temp <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],], family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)), nAGQ = 1)
  fpregres_p_DF$subj[i+1] <- subjSet[i]; fpregres_p_DF[i+1, resName] <- summary(lmer_fpregres_temp)$coefficients[resName, 4]
}
write.csv(fpregres_p_DF, "./data/Ind/fpregres_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/27
fpregres_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpregres_per_DF) <- resName
for(i in 1:length(resName)){
  fpregres_per_DF[1, resName[i]] <- sum(fpregres_p_DF[2:nrow(fpregres_p_DF),resName[i]]<=p_thres)/(nrow(fpregres_p_DF)-1)
}
fpregres_per_DF
# std_res_prevf_coca_log prevwlen_C decod.comp_bct:region_C oral.comp_bct:reglen_C printexp.comp_bct:reglen_C
#              0.4772727          1               0.8181818              0.7045455                  0.4772727
# gort.wpm.st:reglen_C sspan.corr.st:reglen_C*
#            0.9545455                      0



## fpregres_in (no random slope since it does not converge!)
lmer_fpregres_in_c_noRC <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2temp, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

summary(lmer_fpregres_in_c_noRC)
# AIC      BIC   logLik deviance df.resid 
# 22918.6  23167.3 -11428.3  22856.6    22548 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8127 -0.5679 -0.4183 -0.2383  4.8359 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.18308  0.4279  
# itemF      (Intercept) 0.03991  0.1998  
# subj       (Intercept) 0.32212  0.5676  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.371409   0.092201 -14.874  < 2e-16 ***
#   oral.comp_bct                         0.016885   0.146860   0.115  0.90847    
# decod.comp_bct                        0.022854   0.126628   0.180  0.85678    
# printexp.comp_bct                    -0.056195   0.164144  -0.342  0.73209    
# gort.wpm.st                          -0.090231   0.120880  -0.746  0.45540    
# sspan.corr.st                        -0.018604   0.121524  -0.153  0.87833    
# region_C                             -0.046844   0.007472  -6.269 3.63e-10 ***
#   std_res_f_coca_log                   -0.078017   0.024730  -3.155  0.00161 ** 
#   reglen_C                              0.103151   0.011070   9.318  < 2e-16 ***
#   std_res_prevf_coca_log               -0.098878   0.024413  -4.050 5.12e-05 ***
#   prevwlen_C                           -0.052673   0.011719  -4.495 6.97e-06 ***
#   std_res_nextf_coca_log               -0.111998   0.024130  -4.641 3.46e-06 ***
#   nextwlen_C                            0.026228   0.010954   2.394  0.01665 *  
#   oral.comp_bct:region_C                0.001375   0.008608   0.160  0.87313    
# decod.comp_bct:region_C               0.010689   0.007292   1.466  0.14271    
# printexp.comp_bct:region_C            0.016790   0.009781   1.717  0.08606 .  
# gort.wpm.st:region_C                 -0.007515   0.006780  -1.108  0.26765    
# sspan.corr.st:region_C               -0.019443   0.006854  -2.837  0.00456 ** 
#   oral.comp_bct:std_res_f_coca_log      0.028615   0.027275   1.049  0.29411    
# decod.comp_bct:std_res_f_coca_log    -0.010409   0.023006  -0.452  0.65096    
# printexp.comp_bct:std_res_f_coca_log -0.031704   0.030941  -1.025  0.30553    
# gort.wpm.st:std_res_f_coca_log        0.003166   0.021617   0.146  0.88355    
# sspan.corr.st:std_res_f_coca_log     -0.021103   0.021746  -0.970  0.33183    
# oral.comp_bct:reglen_C                0.002024   0.012315   0.164  0.86948    
# decod.comp_bct:reglen_C               0.001050   0.010356   0.101  0.91925    
# printexp.comp_bct:reglen_C            0.007990   0.013880   0.576  0.56485    
# gort.wpm.st:reglen_C                  0.014640   0.009640   1.519  0.12885    
# sspan.corr.st:reglen_C                0.026222   0.009699   2.704  0.00686 ** 
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# summary: (*: p value is smaller than 0.05/5=0.01 but bigger than 0.05/27=0.00185)
# significant main effects: region_C, reglen_C, std_res_f_coca_log, std_res_prevf_coca_log, prevwlen_C, std_res_nextf_coca_log
# significant interactions: sspan.corr.st:region_C*, sspan.corr.st:reglen_C*

# get dfbetas figure
estex.lmer_fpregres_in_c_noRC <- influence(lmer_fpregres_in_c_noRC, "subj")
cut_dfbetas <- 2/sqrt(44)
paraMain <- c('region_C', 'reglen_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C', 'std_res_nextf_coca_log')
paraInt <- c('sspan.corr.st:region_C', 'sspan.corr.st:reglen_C')
dfbetasAll <- dfbetas(estex.lmer_fpregres_in_c_noRC, parameters=0)
png("./data/Ind/fpregres_inAvg_dfbetasMain.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_in_c_noRC, which="dfbetas", parameters=paraMain, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID", nrow=1)
dev.off()
png("./data/Ind/fpregres_inAvg_dfbetasInt.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_in_c_noRC, which="dfbetas", parameters=paraInt, cutoff = cut_dfbetas, xlab="DFbetaS", ylab="Subject ID")
dev.off()

pchange(estex.lmer_fpregres_in_c_noRC)

# get Cook's distance figure
cook <- cooks.distance(estex.lmer_fpregres_in_c_noRC, parameter=0, sort=TRUE)
png("./data/Ind/fpregres_inAvg_cook.png", width=3600, height=1800, res=300)
plot(estex.lmer_fpregres_in_c_noRC, which="cook", cutoff=4/44, sort=TRUE, xlab="Cook큦 Distance", ylab="Subject ID")
dev.off()
# 18, 35, 40

# get altered.sig and changed.sig
# get t value using qt(1-p_value, df=Inf)
t_crit <- qt(1-0.05/27, df=Inf)
main1 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'region_C'; which(main1$Altered.Sig == FALSE & main1$Changed.Sig == TRUE)
main2 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=t_crit)$'reglen_C'; which(main2$Altered.Sig == FALSE & main2$Changed.Sig == TRUE)
main3 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'std_res_f_coca_log'; which(main3$Altered.Sig == FALSE & main3$Changed.Sig == TRUE)
# 7 42
main4 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'std_res_prevf_coca_log'; which(main4$Altered.Sig == FALSE & main4$Changed.Sig == TRUE)
main5 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'prevwlen_C'; which(main5$Altered.Sig == FALSE & main5$Changed.Sig == TRUE)
main6 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'std_res_nextf_coca_log'; which(main6$Altered.Sig == FALSE & main6$Changed.Sig == TRUE)
int1 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=-t_crit)$'sspan.corr.st:region_C'; which(int1$Altered.Sig == FALSE & int1$Changed.Sig == TRUE)
int2 <- sigtest(estex.lmer_fpregres_in_c_noRC, test=t_crit)$'sspan.corr.st:reglen_C'; which(int2$Altered.Sig == FALSE & int2$Changed.Sig == TRUE)

# use different submodels each of which has one subject removed to calculate the stability of the significant main effects and interactions
subjSet <- unique(D2temp$subj)
paraMain <- c('region_C', 'reglen_C', 'std_res_f_coca_log', 'std_res_prevf_coca_log', 'prevwlen_C', 'std_res_nextf_coca_log')
paraInt <- c('sspan.corr.st:region_C', 'sspan.corr.st:reglen_C')
resName <- c(paraMain, paraInt) 

fpregres_in_p_DF <- data.frame(matrix(NA, nrow=length(subjSet)+1, ncol=length(resName)+1)); names(fpregres_in_p_DF) <- c('subj', resName)
fpregres_in_p_DF$subj[1] <- 0; fpregres_in_p_DF[1,resName] <- summary(lmer_fpregres_in_c_noRC)$coefficients[resName, 4]
for(i in 1:length(subjSet)){
  cat("Remove subject ", i, "\n");
  lmer_fpregres_in_temp <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C +  printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2temp[D2temp$subj %in% subjSet[-c(i)],], family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)), nAGQ = 1)
  fpregres_in_p_DF$subj[i+1] <- subjSet[i]; fpregres_in_p_DF[i+1, resName] <- summary(lmer_fpregres_in_temp)$coefficients[resName, 4]
}
write.csv(fpregres_in_p_DF, "./data/Ind/fpregres_in_p_DF.csv", row.names = FALSE)

p_thres <- 0.05/27
fpregres_in_per_DF <- data.frame(matrix(NA,nrow=1, ncol=length(resName))); names(fpregres_in_per_DF) <- resName
for(i in 1:length(resName)){
  fpregres_in_per_DF[1, resName[i]] <- sum(fpregres_in_p_DF[2:nrow(fpregres_in_p_DF),resName[i]]<=p_thres)/(nrow(fpregres_in_p_DF)-1)
}
fpregres_in_per_DF
# region_C reglen_C std_res_f_coca_log std_res_prevf_coca_log prevwlen_C std_res_nextf_coca_log sspan.corr.st:region_C
#        1        1          0.5909091                      1          1                      1             0.09090909
# sspan.corr.st:reglen_C
#             0.02272727



