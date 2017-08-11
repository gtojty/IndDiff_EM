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


source("./R-squared.r")
source("./drawMainEffect_Int.r")
source("./mer.r")

# calculate deta-R
calDeltaR <- function(Rs1, Rs2){
  set1 <- c(rep(Rs1, length(Rs2)))
  return(set1 - Rs2)
}

# load data
D1 <- read.csv("./data/res_F/ET_F_ana1.csv", na.strings = "NA")
D1$itemF <- factor(D1$item) # probably won't matter if item is treated as numeric or categorical, but
D2 <- read.csv("./data/res_F/ET_F_ana2.csv", na.strings = "NA")
D2$itemF <- factor(D2$item) # probably won't matter if item is treated as numeric or categorical, but

### Analyses on ffixurt, fpurt, fpregres and fpregres_out, and optional (tffixurt) 
### simulatenous regression analysis: EM measures ~ Lexical Properties + Individual Skills + Lexical Properties:Individual Skills
# symbols for different columns
numQuartile <- 4
# dependent variables (EM measures)
ffixurt <- "ffixurt"; fpurt <- "fpurt"; fpregres <- "fpregres"; fpregres_in <- "fpregres_in"; turt <- "turt"
# independent variables (lexical properties)
f <- "std_res_f_coca_log"; prevf <- "std_res_prevf_coca_log"; nextf <- "std_res_nextf_coca_log"
region <- "region_C"; reglen <- "reglen_C"; prevwlen <- "prevwlen_C"; nextwlen <- "nextwlen_C"
# indepdent variables (individual skill measures)
# composite scores (note that all these using Box Cox tranformed measures)
oral <- "oral.comp_bct"; decod <- "decod.comp_bct"; readcomp <- "readcomp.comp_bct"; printexp <- "printexp.comp_bct"
# individual scores (note that listening comprehension (piat) uses Box Cox transformed measure)
gort <- "gort.wpm.st"; sspan <- "sspan.corr.st" 



### ffixurt
# D2 (with some stopwords)
lmer_ffixurt_c_noRC_slope <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)

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

Rs_ffixurt_noRC_slope <- r.squaredGLMM(lmer_ffixurt_c_noRC_slope)
# R2m       R2c 
# 0.04137847 0.15003405 

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_ffixurt_c_noRC_slope)
# 5.029211

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_ffixurt_c_noRC_slope))
# 3.534922

# run the party implementation
library(party)
cf_ffixurt_c_noRC <- cforest(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_ffixurt_c_noRC_varimp <- varimp(cf_ffixurt_c_noRC)
cf_ffixurt_c_noRC_varimp
# oral.comp_bct         decod.comp_bct      printexp.comp_bct            gort.wpm.st          sspan.corr.st 
# 621.77024              669.65542              507.21012              606.71533              522.41976 
# region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log             prevwlen_C 
# 185.98666              138.02368              104.05652               91.69961              191.59924 
# std_res_nextf_coca_log             nextwlen_C 
# 22.75005               37.88510 
png("./data/resFigures/1.ffixurt/cf_ffixurt_c_noRC_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_ffixurt_c_noRC_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_ffixurt_c_noRC_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# region_C
lmer_ffixurt_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_region <- r.squaredGLMM(lmer_ffixurt_region)
# std_res_f_coca_log
lmer_ffixurt_f <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_f <- r.squaredGLMM(lmer_ffixurt_f)
# std_res_prevf_coca_log
lmer_ffixurt_prevf <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_prevf <- r.squaredGLMM(lmer_ffixurt_prevf)
# prevwlen_C
lmer_ffixurt_prevwlen <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_prevwlen <- r.squaredGLMM(lmer_ffixurt_prevwlen)

# interactions
# printexp.comp_bct:region_C*
lmer_ffixurt_printexp_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                     + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                     + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_printexp_region <- r.squaredGLMM(lmer_ffixurt_printexp_region)
# gort.wpm.st:region_C*
lmer_ffixurt_gort_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_gort_region <- r.squaredGLMM(lmer_ffixurt_gort_region)
# decod.comp_bct:region_C*
lmer_ffixurt_decod_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_decod_region <- r.squaredGLMM(lmer_ffixurt_decod_region)
# decod.comp_bct:reglen_C*
lmer_ffixurt_decod_reglen <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_ffixurt_decod_reglen <- r.squaredGLMM(lmer_ffixurt_decod_reglen)

Rs1 <- Rs_ffixurt_noRC_slope["R2m"] 
Rs2 <- c(Rs_ffixurt_region["R2m"], Rs_ffixurt_f["R2m"], Rs_ffixurt_prevf["R2m"], Rs_ffixurt_prevwlen["R2m"], 
         Rs_ffixurt_printexp_region["R2m"], Rs_ffixurt_gort_region["R2m"], Rs_ffixurt_decod_region["R2m"], Rs_ffixurt_decod_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0038424609 0.0036150791 0.0023806945 0.0024796382
# 0.0002998297* 0.0003428752* 0.0003127827* 0.0002962935*

# draw significant main effects:
# region_C
Draw_MainEffect("linear", D2, region, ffixurt, c(220,265), 0.75, "Word Position", "First Fixation Duration (ms)", 
                "./data/resFigures/1.ffixurt/Main_ffixurt_region2.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, ffixurt, c(220,265), 0.75, "Word Frequency", "First Fixation Duration (ms)", 
                "./data/resFigures/1.ffixurt/Main_ffixurt_f2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, ffixurt, c(220,265), 0.75, "Previous Word Frequency", "First Fixation Duration (ms)", 
                "./data/resFigures/1.ffixurt/Main_ffixurt_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("linear", D2, prevwlen, ffixurt, c(220,265), 0.75, "Previous Word Length", "First Fixation Duration (ms)", 
                "./data/resFigures/1.ffixurt/Main_ffixurt_prevwlen2.png", 6, 6)

# draw significant interactions:
# printexp.comp_bct:region_C*
LabQuartile <- c("Low Print Exp.", "", "", "High Print Exp.")
Draw_Int("linear", "number", 0, D2, region, ffixurt, printexp, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Position", "First Fixation Duration (ms)", "Print Experience x Word Position", 
         "./data/resFigures/1.ffixurt/Int_ffixurt_printexp_region2.png", 24, 6)
# gort.wpm_S:region_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, ffixurt, gort, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Position", "First Fixation Duration (ms)", "Oral Reading Fluency x Word Position", 
         "./data/resFigures/1.ffixurt/Int_ffixurt_gort_region2.png", 24, 6)
# decod.comp_bct:region_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, region, ffixurt, decod, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Region", "First Fixation Duration (ms)", "Decoding x Word Length", 
         "./data/resFigures/1.ffixurt/Int_ffixurt_decod_region2.png", 24, 6)
# decod.comp_bct:reglen_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, ffixurt, decod, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Length", "First Fixation Duration (ms)", "Decoding x Word Length", 
         "./data/resFigures/1.ffixurt/Int_ffixurt_decod_reglen2.png", 24, 6)



### fpurt
# D2 (with some stopwords)
lmer_fpurt_c_noRC_slope <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)

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

# calculate delta-R squared
Rs_fpurt_c_noRC_slope <- r.squaredGLMM(lmer_fpurt_c_noRC_slope)
# R2m       R2c 
# 0.1062309 0.2107980 

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpurt_c_noRC_slope)
# 5.029211

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpurt_c_noRC_slope))
# 3.552312

# run the party implementation
library(party)
cf_fpurt_c_noRC <- cforest(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpurt_c_noRC_varimp <- varimp(cf_fpurt_c_noRC)
cf_fpurt_c_noRC_varimp
# oral.comp_bct         decod.comp_bct      printexp.comp_bct            gort.wpm.st          sspan.corr.st 
# 1201.4548              1008.8699               962.6381              1408.0947               918.2173 
# region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log             prevwlen_C 
# 295.4117               516.6545              1955.2270               134.4534               275.0588 
# std_res_nextf_coca_log             nextwlen_C 
# 108.6452               103.8023 
png("./data/resFigures/2.fpurt/cf_fpurt_c_noRC_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpurt_c_noRC_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpurt_c_noRC_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# region_C*
lmer_fpurt_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_region <- r.squaredGLMM(lmer_fpurt_region)
# std_res_f_coca_log
lmer_fpurt_f <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_f <- r.squaredGLMM(lmer_fpurt_f)
# reglen_C
lmer_fpurt_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_reglen <- r.squaredGLMM(lmer_fpurt_reglen)
# std_res_prevf_coca_log
lmer_fpurt_prevf <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_prevf <- r.squaredGLMM(lmer_fpurt_prevf)
# std_res_nextf_coca_log
lmer_fpurt_nextf <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_nextf <- r.squaredGLMM(lmer_fpurt_nextf)

# interactions
# gort.wpm.st:region_C
lmer_fpurt_gort_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_gort_region <- r.squaredGLMM(lmer_fpurt_gort_region)
# decod.comp_bct:reglen_C
lmer_fpurt_decod_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_decod_reglen <- r.squaredGLMM(lmer_fpurt_decod_reglen)
# gort.wpm.st:reglen_C
lmer_fpurt_gort_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_gort_reglen <- r.squaredGLMM(lmer_fpurt_gort_reglen)
# sspan.corr.st:reglen_C
lmer_fpurt_sspan_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C 
                                + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_fpurt_sspan_reglen <- r.squaredGLMM(lmer_fpurt_sspan_reglen)

Rs1 <- Rs_fpurt_c_noRC_slope["R2m"] 
Rs2 <- c(Rs_fpurt_region["R2m"], Rs_fpurt_f["R2m"], Rs_fpurt_reglen["R2m"], Rs_fpurt_prevf["R2m"], Rs_fpurt_nextf["R2m"],
         Rs_fpurt_gort_region["R2m"], Rs_fpurt_decod_reglen["R2m"], Rs_fpurt_gort_reglen["R2m"], Rs_fpurt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0008476511* 0.0055476815 0.0380699276 0.0028032811 0.0022073085
# 0.0005855128 0.0007972857 0.0013973109 0.0004752797

# draw significant main effects:
# region_C*
Draw_MainEffect("linear", D2, region, fpurt, c(200,400), 0.75, 
                "Word Position", "First-pass Reading Time (ms)", 
                "./data/resFigures/2.fpurt/Main_fpurt_region2.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, fpurt, c(200,400), 0.75, 
                "Word Frequency", "First-pass Reading Time (ms)", 
                "./data/resFigures/2.fpurt/Main_fpurt_f2.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D2, reglen, fpurt, c(200,400), 0.75, 
                "Word Length", "First-pass Reading Time (ms)", 
                "./data/resFigures/2.fpurt/Main_fpurt_reglen2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, fpurt, c(200,400), 0.75, 
                "Previous Word Frequency", "First-pass Reading Time (ms)", 
                "./data/resFigures/2.fpurt/Main_fpurt_prevf2.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("linear", D2, nextf, fpurt, c(200,400), 0.75, 
                "Next Word Frequency", "First-pass Reading Time (ms)", 
                "./data/resFigures/2.fpurt/Main_fpurt_nextf2.png", 6, 6)

# draw significant interactions:
# gort.wpm.st:region_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Position", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Position", 
         "./data/resFigures/2.fpurt/Int_fpurt_gort_region2.png", 24, 6)
# decod.comp_bct:reglen_C
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, decod, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/resFigures/2.fpurt/Int_fpurt_decod_reglen2.png", 24, 6)
LabQuartile <- c("Low Decod.", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, decod, 2, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/resFigures/2.fpurt/Int_fpurt_decod_reglen2_2groups.png", 12, 6)
# gort.wpm.st:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/resFigures/2.fpurt/Int_fpurt_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, sspan, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Verbal Working Memory x Word Length", 
         "./data/resFigures/2.fpurt/Int_fpurt_sspan_reglen2.png", 24, 6)



### turt
# D2 (with stopwords)
lmer_turt_c_noRC_slope <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)

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

Rs_turt_c_noRC_slope <- r.squaredGLMM(lmer_turt_c_noRC_slope)
# R2m        R2c 
# 0.09599745 0.23006538 

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_turt_c_noRC_slope)
# 5.029211

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_turt_c_noRC_slope))
# 3.444066

# run the party implementation
library(party)
cf_turt_c_noRC <- cforest(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_turt_c_noRC_varimp <- varimp(cf_turt_c_noRC)
cf_turt_c_noRC_varimp
# oral.comp_bct         decod.comp_bct      printexp.comp_bct            gort.wpm.st          sspan.corr.st 
# 3017.2527              2799.3702              2652.8795              4650.0693              1422.2756 
# region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log             prevwlen_C 
# 2046.9115              1704.9241              5213.1805               579.9069               884.9051 
# std_res_nextf_coca_log             nextwlen_C 
# 776.4203               331.6904 
png("./data/resFigures/3.turt/cf_turt_c_noRC_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_turt_c_noRC_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_turt_c_noRC_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# std_res_f_coca_log
lmer_turt_f <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_f <- r.squaredGLMM(lmer_turt_f)
# reglen
lmer_turt_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_reglen <- r.squaredGLMM(lmer_turt_reglen)
# std_res_prevf_coca_log
lmer_turt_prevf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_prevf <- r.squaredGLMM(lmer_turt_prevf)
# std_res_nextf_coca_log
lmer_turt_nextf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_nextf <- r.squaredGLMM(lmer_turt_nextf)

# Interactions
# gort.wpm.st:region_C*
lmer_turt_gort_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_gort_region <- r.squaredGLMM(lmer_turt_gort_region)
# sspan.corr.st:region_C
lmer_turt_sspan_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C 
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_sspan_region <- r.squaredGLMM(lmer_turt_sspan_region)
# decod.comp_bct:reglen_C
lmer_turt_decod_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_decod_reglen <- r.squaredGLMM(lmer_turt_decod_reglen)
# gort.wpm.st:reglen_C
lmer_turt_gort_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_gort_reglen <- r.squaredGLMM(lmer_turt_gort_reglen)
# sspan.corr.st:reglen_C
lmer_turt_sspan_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C 
                               + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2)
Rs_turt_sspan_reglen <- r.squaredGLMM(lmer_turt_sspan_reglen)

Rs1 <- Rs_turt_c_noRC_slope["R2m"] 
Rs2 <- c(Rs_turt_f["R2m"], Rs_turt_reglen["R2m"], Rs_turt_prevf["R2m"], Rs_turt_nextf["R2m"], 
         Rs_turt_gort_region["R2m"], Rs_turt_sspan_region["R2m"], Rs_turt_decod_reglen["R2m"], Rs_turt_gort_reglen["R2m"], Rs_turt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0071694575 0.0378799089 0.0026764279 0.0036210754
# 0.0003186606* 0.0005376636 0.0005710486 0.0009580305 0.0009207376 

# draw significant main effects:
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, turt, c(250,600), 0.75, "Word Frequency", "Total Reading Time (ms)", 
                "./data/resFigures/3.turt/Main_turt_f2.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D2, reglen, turt, c(250,600), 0.75, "Word Length", "Total Reading Time (ms)", 
                "./data/resFigures/3.turt/Main_turt_reglen2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, turt, c(250,600), 0.75, "Previous Word Frequency", "Total Reading Time (ms)", 
                "./data/resFigures/3.turt/Main_turt_prevf2.png", 6, 6)
# std_Res_nextf_coca_log*
Draw_MainEffect("linear", D2, nextf, turt, c(250,600), 0.75, "Next Word Frequency", "Total Reading Time (ms)", 
                "./data/resFigures/3.turt/Main_turt_nextf2.png", 6, 6)

# draw significant interactions:
# gort.wpm.st:region_C*
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Oral Reading Fluency x Word Position", 
         "./data/resFigures/3.turt/Int_turt_gort_region2.png", 24, 6)
# sspan.corr.st:region_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, region, turt, sspan, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Verbal Working Memory x Word Position", 
         "./data/resFigures/3.turt/Int_turt_sspan_region2.png", 24, 6)
# decod.comp_bct:reglen_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, turt, decod, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Decoding x Word Length", 
         "./data/resFigures/3.turt/Int_turt_decod_reglen2.png", 24, 6)
# gort.wpm.st:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, reglen, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/resFigures/3.turt/Int_turt_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, reglen, turt, sspan, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Verbal Working Memory x Word Length", 
         "./data/resFigures/3.turt/Int_turt_sspan_reglen2.png", 24, 6)



### fpregres
# D2 (with stopwords)
lmer_fpregres_c_noRC_slope <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                    + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                    + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

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

Rs_fpregres_c_noRC_slope <- r.squared(lmer_fpregres_c_noRC_slope)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.04575436   0.1988251 18561.44

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_c_noRC_slope)
# 5.029211

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_c_noRC_slope))
# 3.676731

# run the party implementation
library(party)
cf_fpregres_c_noRC <- cforest(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_c_noRC_varimp <- varimp(cf_fpregres_c_noRC)
cf_fpregres_c_noRC_varimp
# oral.comp_bct         decod.comp_bct      printexp.comp_bct            gort.wpm.st          sspan.corr.st 
# 0.002843294            0.003259993            0.002192145            0.003886989            0.004745084 
# region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log             prevwlen_C 
# 0.003639119            0.001241620            0.001175449            0.002672870            0.004814442 
# std_res_nextf_coca_log             nextwlen_C 
# 0.001019404            0.001144364 
png("./data/resFigures/4.fpregres/cf_fpregres_c_noRC_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_c_noRC_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_c_noRC_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# std_res_prevf_coca_log
lmer_fpregres_prevf <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C +  prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevf <- r.squared(lmer_fpregres_prevf)
# prevwlen_C
lmer_fpregres_prevwlen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevwlen <- r.squared(lmer_fpregres_prevwlen)

# interactions
# significant interactions: decod.comp_bct:region_C, oral.comp_bct:reglen_C, printexp.comp_bct:reglen_C, gort.wpm_S:reglen_C, sspan.corr.st:reglen_C*
# decod.comp_bct:region_C
lmer_fpregres_decod_region <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                    + oral.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                    + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_decod_region <- r.squared(lmer_fpregres_decod_region)
# oral.comp_bct:reglen_C
lmer_fpregres_oral_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_oral_reglen <- r.squared(lmer_fpregres_oral_reglen)
# printexp.comp_bct:reglen_C
lmer_fpregres_printexp_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_printexp_reglen <- r.squared(lmer_fpregres_printexp_reglen)
# gort.wpm.st:reglen_C
lmer_fpregres_gort_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                                   + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_gort_reglen <- r.squared(lmer_fpregres_gort_reglen)
# sspan.corr.st:reglen_C*
lmer_fpregres_sspan_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C
                                   + (1|itemF/word) + (1+std_res_f_coca_log|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_sspan_reglen <- r.squared(lmer_fpregres_sspan_reglen)


fpregres_marg <- c(rep(Rs_fpregres_c_noRC_slope$Marginal, 2)) - c(Rs_fpregres_prevf$Marginal, Rs_fpregres_prevwlen$Marginal)
# 0.002085104 0.017379945
fpregres_marg_int <- c(rep(Rs_fpregres_c_noRC_slope$Marginal, 5)) - c(Rs_fpregres_decod_region$Marginal,  
                                                         Rs_fpregres_oral_reglen$Marginal, Rs_fpregres_printexp_reglen$Marginal, Rs_fpregres_gort_reglen$Marginal, Rs_fpregres_sspan_reglen$Marginal)
# 0.0008229415 0.0008061488 0.0008609565 0.0011412105 0.0003857691*


# draw significant main effects:
# std_res_prevf_coca_log
Draw_MainEffect("logistic", D2, prevf, fpregres, c(0.05,0.3), 0.75, "Previous Word Frequency", "Regression_out", 
                "./data/resFigures/4.fpregres/Main_fpregres_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D2, prevwlen, fpregres, c(0.05,0.3), 0.75, "Previous Word Length", "Regression_out", 
                "./data/resFigures/4.fpregres/Main_fpregres_prevwlen2.png", 6, 6)

# draw significant interactions:
# decod.comp_bct:region_C
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("logistic", "number", 0, D2, region, fpregres, decod, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Position", "Regression_out", "Decoding x Word Position", 
         "./data/resFigures/4.fpregres/Int_fpregres_decod_region2.png", 24, 6)
# oral.comp_bct:reglen_C
LabQuartile <- c("Low Oral Know.", "", "", "High Oral Know.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, oral, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Oral Language Skill x Word Length", 
         "./data/resFigures/4.fpregres/Int_fpregres_oral_reglen2.png", 24, 6)
# printexp.comp_bct_S:reglen_C
LabQuartile <- c("Low Print Exp.", "", "", "High Print Exp.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, printexp, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Print Experience x Word Length", 
         "./data/resFigures/4.fpregres/Int_fpregres_printexp_reglen2.png", 24, 6)
# gort.wpm_S:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, gort, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Oral Reading Fluency x Word Length", 
         "./data/resFigures/4.fpregres/Int_fpregres_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, sspan, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Verbal Working Memory x Word Length", 
         "./data/resFigures/4.fpregres/Int_fpregres_sspan_reglen2.png", 24, 6)



# fpregres_in (due to convergence requirement, here we delete random slope, since the model with it does not converge)
# D2 (with stopwords)
lmer_fpregres_in_c_noRC <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

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

Rs_fpregres_in_c_noRC <- r.squared(lmer_fpregres_in_c_noRC)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.03717746   0.1770754 22909.27

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_in_c_noRC)
# 5.029211

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_in_c_noRC))
# 3.488279

# run the party implementation
library(party)
cf_fpregres_in_c_noRC <- cforest(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_in_c_noRC_varimp <- varimp(cf_fpregres_in_c_noRC)
cf_fpregres_in_c_noRC_varimp
# oral.comp_bct         decod.comp_bct      printexp.comp_bct            gort.wpm.st          sspan.corr.st 
# 0.004392259            0.003880610            0.003884830            0.005121995            0.003381630 
# region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log             prevwlen_C 
# 0.008141601            0.001711220            0.004221348            0.001678383            0.002494155 
# std_res_nextf_coca_log             nextwlen_C 
# 0.002302348            0.001385294
png("./data/resFigures/5.fpregres_in/cf_fpregres_in_c_noRC_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_in_c_noRC_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_in_c_noRC_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# region_C
lmer_fpregres_in_region <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_region <- r.squared(lmer_fpregres_in_region)
# reglen_C
lmer_fpregres_in_reglen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_reglen <- r.squared(lmer_fpregres_in_reglen)
# std_res_f_coca_log
lmer_fpregres_in_f <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_f <- r.squared(lmer_fpregres_in_f)
# std_res_prevf_coca_log
lmer_fpregres_in_prevf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevf <- r.squared(lmer_fpregres_in_prevf)
# prevwlen_C
lmer_fpregres_in_prevwlen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                   + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevwlen <- r.squared(lmer_fpregres_in_prevwlen)
# std_res_nextf_coca_log
lmer_fpregres_in_nextf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_nextf <- r.squared(lmer_fpregres_in_nextf)

# interactions
# sspan.corr.st:region_C*, sspan.corr.st:reglen_C*
lmer_fpregres_in_sspan_region <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C 
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_sspan_region <- r.squared(lmer_fpregres_in_sspan_region)
# sspan.corr.st:reglen_C*
lmer_fpregres_in_sspan_reglen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C 
                                       + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_sspan_reglen <- r.squared(lmer_fpregres_in_sspan_reglen)

fpregres_in_marg <- c(rep(Rs_fpregres_in_c_noRC$Marginal, 6)) - c(Rs_fpregres_in_region$Marginal, Rs_fpregres_in_reglen$Marginal, Rs_fpregres_in_f$Marginal,
                                                           Rs_fpregres_in_prevf$Marginal, Rs_fpregres_in_prevwlen$Marginal, Rs_fpregres_in_nextf$Marginal)
# 0.0053155488 0.0125404515 0.0015968831 0.0004529765 0.0021636819 0.0033466554
fpregres_in_int <- c(rep(Rs_fpregres_in_c_noRC$Marginal, 2)) - c(Rs_fpregres_in_sspan_region$Marginal, Rs_fpregres_in_sspan_reglen$Marginal)
# 0.0005845621* 0.0005340647*

# draw significant main effects:
# region_C
Draw_MainEffect("logistic", D2, region, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Position", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_region2.png", 6, 6)
# reglen_C
Draw_MainEffect("logistic", D2, reglen, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Length", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_reglen2.png", 6, 6)
# std_res_f_coca_log*
Draw_MainEffect("logistic", D2, f, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Frequency", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_f2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("logistic", D2, prevf, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Frequency", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D2, prevwlen, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Length", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_prevwlen2.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("logistic", D2, nextf, fpregres_in, c(0.1,0.4), 0.75, 
                "Next Word Frequency", "Regression_in", 
                "./data/resFigures/5.fpregres_in/Main_fpregres_in_nextf2.png", 6, 6)

# draw significant interactions
# sspan.corr.st:reglen_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("logistic", "number", 0, D2, region, fpregres_in, sspan, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Region", "Regression_out", "Verbal Working Memory x Word Length", 
         "./data/resFigures/5.fpregres_in/Int_fpregres_in_sspan_region2.png", 24, 6)
# sspan.corr.st:reglen_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres_in, sspan, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Verbal Working Memory x Word Length", 
         "./data/resFigures/5.fpregres_in/Int_fpregres_in_sspan_reglen2.png", 24, 6)




# contribution of decoding and oral knowledge to reading comprehension
a <- lm(readcomp.comp_bct ~ decod.comp_bct + oral.comp_bct, data=aggregate(D2, by=list(D2$subj), FUN=mean, na.rm=TRUE))
summary(a)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.52683 -0.28718  0.01531  0.31547  0.99300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    4.771e-08  7.901e-02   0.000   1.0000    
# decod.comp_bct 1.950e-01  9.669e-02   2.016   0.0503 .  
# oral.comp_bct  7.342e-01  9.669e-02   7.593 2.43e-09 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.5241 on 41 degrees of freedom
# Multiple R-squared:  0.7381,	Adjusted R-squared:  0.7253 
# F-statistic: 57.77 on 2 and 41 DF,  p-value: 1.182e-12

b <- lm(readcomp.comp_bct ~ decod.comp_bct * oral.comp_bct, data=aggregate(D2, by=list(D2$subj), FUN=mean, na.rm=TRUE))
summary(b)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.58138 -0.32534  0.02758  0.30167  0.96634 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   0.03153    0.09727   0.324   0.7475    
# decod.comp_bct                0.19629    0.09753   2.013   0.0509 .  
# oral.comp_bct                 0.74250    0.09861   7.530 3.47e-09 ***
#   decod.comp_bct:oral.comp_bct -0.05734    0.10146  -0.565   0.5751    
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.5285 on 40 degrees of freedom
# Multiple R-squared:  0.7402,	Adjusted R-squared:  0.7207 
# F-statistic: 37.98 on 3 and 40 DF,  p-value: 8.781e-12

anova(a,b)
# Model 1: readcomp.comp_bct ~ decod.comp_bct + oral.comp_bct
# Model 2: readcomp.comp_bct ~ decod.comp_bct * oral.comp_bct
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     41 11.263                           
# 2     40 11.173  1  0.089225 0.3194 0.5751

a1 <- lm(readcomp.comp_bct ~ oral.comp_bct, data=aggregate(D2, by=list(D2$subj), FUN=mean, na.rm=TRUE))
summary(a1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.30054 -0.34687  0.01795  0.34478  0.92797 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.073e-08  8.185e-02    0.00        1    
# oral.comp_bct 8.439e-01  8.279e-02   10.19 6.33e-13 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.5429 on 42 degrees of freedom
# Multiple R-squared:  0.7121,	Adjusted R-squared:  0.7053 
# F-statistic: 103.9 on 1 and 42 DF,  p-value: 6.331e-13

a2 <- lm(readcomp.comp_bct ~ decod.comp_bct, data=aggregate(D2, by=list(D2$subj), FUN=mean, na.rm=TRUE))
summary(a2)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6324 -0.4048  0.1899  0.4751  1.3917 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    6.874e-08  1.211e-01   0.000        1    
# decod.comp_bct 6.081e-01  1.225e-01   4.964  1.2e-05 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.8033 on 42 degrees of freedom
# Multiple R-squared:  0.3697,	Adjusted R-squared:  0.3547 
# F-statistic: 24.64 on 1 and 42 DF,  p-value: 1.197e-05