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
# D1 (without stopwords)
lmer_ffixurt_i <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D1)

# model selection
lmer_ffixurt0 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|subj), data = D1)

lmer_ffixurt1 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|word) + (1|subj), data = D1)

lmer_ffixurt2 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|itemF) + (1|word) + (1|subj), data = D1)

anova(lmer_ffixurt_i, lmer_ffixurt0)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_ffixurt0  34 176497 176756 -88214   176429                             
# lmer_ffixurt_i 36 176301 176575 -88115   176229 199.85      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_ffixurt_i, lmer_ffixurt1)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# lmer_ffixurt1  35 176307 176573 -88118   176237                            
# lmer_ffixurt_i 36 176301 176575 -88115   176229 7.6208      1    0.00577 **
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_ffixurt_i, lmer_ffixurt2)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_ffixurt_i 36 176301 176575 -88115   176229                        
# lmer_ffixurt2  36 176308 176582 -88118   176236     0      0          1

# conclusion: lmer_ffixurt_i is the best!

summary(lmer_ffixurt_i)
# REML criterion at convergence: 176173.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9890 -0.6078 -0.1602  0.3837  8.0562 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)  300.553 17.336  
# itemF      (Intercept)    6.854  2.618  
# subj       (Intercept)  722.468 26.879  
# Residual               7885.134 88.798  
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          236.56559    4.22116   56.04
# oral.comp_bct                        -12.65044    8.61384   -1.47
# decod.comp_bct                        -9.42405    6.06942   -1.55
# readcomp.comp_bct                     11.61560    8.38078    1.39
# printexp.comp_bct                     -4.62436    7.87817   -0.59
# gort.wpm.st                           -6.89291    5.76225   -1.20
# sspan.corr.st                          6.37081    5.82379    1.09
# region_C                               2.51714    0.35601    7.07 5.582477e-12
# std_res_f_coca_log                    -7.17419    1.14532   -6.26 1.234329e-09
# reglen_C                              -0.51634    0.56978   -0.91
# std_res_prevf_coca_log                -3.41993    1.14842   -2.98 0.004704958
# prevwlen_C                             2.97984    0.55824    5.34 4.350789e-07
# std_res_nextf_coca_log                -2.01133    1.15638   -1.74
# nextwlen_C                             0.47750    0.52297    0.91
# oral.comp_bct:region_C                 0.10693    0.47130    0.23
# decod.comp_bct:region_C                0.88879    0.32923    2.70 0.01042093
# readcomp.comp_bct:region_C             0.82877    0.45396    1.83
# printexp.comp_bct:region_C            -1.02461    0.43070   -2.38 0.02349099
# gort.wpm.st:region_C                   0.61700    0.31297    1.97
# sspan.corr.st:region_C                -0.56956    0.32134   -1.77
# oral.comp_bct:std_res_f_coca_log      -0.17244    1.52246   -0.11
# decod.comp_bct:std_res_f_coca_log      2.88623    1.06093    2.72 0.009871154
# readcomp.comp_bct:std_res_f_coca_log  -1.58046    1.46391   -1.08
# printexp.comp_bct:std_res_f_coca_log   3.11832    1.38613    2.25 0.03173965
# gort.wpm.st:std_res_f_coca_log        -0.65905    1.01169   -0.65
# sspan.corr.st:std_res_f_coca_log       0.04136    1.03241    0.04
# oral.comp_bct:reglen_C                 0.71471    0.76176    0.94
# decod.comp_bct:reglen_C               -1.09676    0.52894   -2.07 0.04682264
# readcomp.comp_bct:reglen_C            -1.16023    0.73035   -1.59
# printexp.comp_bct:reglen_C             0.21190    0.68517    0.31
# gort.wpm.st:reglen_C                  -0.21591    0.50086   -0.43
# sspan.corr.st:reglen_C                 0.73204    0.51083    1.43

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: region_C, std_res_f_coca_log, std_res_prevf_coca_log*, prevwlen_C
# significant interactions: decod.comp_bct:region_C*, decod.comp_bct:std_res_f_coca_log*

# calculate delta-R squared
Rs_ffixurt <- r.squaredGLMM(lmer_ffixurt_i)
# R2m        R2c 
# 0.05104584 0.16067037 

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_ffixurt_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_ffixurt_i))
# 4.3425

# run the party implementation
library(party)
cf_ffixurt_i <- cforest(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
               , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_ffixurt_i_varimp <- varimp(cf_ffixurt_i)
cf_ffixurt_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 614.25791              668.81696              629.47862              438.87980              476.35298 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 548.50098              254.28628              158.50485               77.64363               82.40088 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 129.59800               25.06092               40.49728 
png("./data/res_F/resFigures/1.ffixurt/cf_ffixurt_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_ffixurt_i_varimp), panel=function (x,y){
     panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
     panel.abline(v=abs(min(cf_ffixurt_i_varimp)), col = 'red', lty='longdash', lwd=4)
     panel.abline(v=0, col='blue')
})
print(g)
dev.off()


# remove models
# main effects
# region_C
lmer_ffixurt_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_region <- r.squaredGLMM(lmer_ffixurt_region)
# std_res_f_coca_log
lmer_ffixurt_f <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_f <- r.squaredGLMM(lmer_ffixurt_f)
# std_res_prevf_coca_log*
lmer_ffixurt_prevf <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_prevf <- r.squaredGLMM(lmer_ffixurt_prevf)
# prevwlen
lmer_ffixurt_prevwlen <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_prevwlen <- r.squaredGLMM(lmer_ffixurt_prevwlen)

# Interactions
# decod.comp_bct:region_C*
lmer_ffixurt_decod_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_decod_region <- r.squaredGLMM(lmer_ffixurt_decod_region)
# decod.comp_bct:std_res_f_coca_log*
lmer_ffixurt_decod_f <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1|subj), data = D1)
Rs_ffixurt_decod_f <- r.squaredGLMM(lmer_ffixurt_decod_f)

Rs1 <- Rs_ffixurt["R2m"] 
Rs2 <- c(Rs_ffixurt_region["R2m"], Rs_ffixurt_f["R2m"], Rs_ffixurt_prevf["R2m"], Rs_ffixurt_prevwlen["R2m"], 
         Rs_ffixurt_decod_region["R2m"], Rs_ffixurt_decod_f["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0067294167 0.0053705493 0.0027080301* 0.0036334195
# 0.0004039952* 0.0004143044*

# draw significant main effects:
# region_C
Draw_MainEffect("linear", D1, region, ffixurt, c(200,275), 0.75, "Word Position", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_region1.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D1, f, ffixurt, c(200,275), 0.75, "Word Frequency", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_f1.png", 6, 6)
# std_res_prevf_coca_log*
Draw_MainEffect("linear", D1, prevf, ffixurt, c(200,275), 0.75, "Previous Word Frequency", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_prevf1.png", 6, 6)
# prevwlen_C
Draw_MainEffect("linear", D1, prevwlen, ffixurt, c(200,275), 0.75, "Previous Word Length", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_prevwlen1.png", 6, 6)

# draw significant interactions:
# decod.comp_bct:region_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, region, ffixurt, decod, numQuartile, LabQuartile, c(200,320), 0.85, 0,
         "Word Position", "First Fixation Duration (ms)", "Decoding x Word Position", 
         "./data/res_F/resFigures/1.ffixurt/Int_ffixurt_decod_region1.png", 24, 6)
# decod.comp_bct:std_res_f_coca_log*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, f, ffixurt, decod, numQuartile, LabQuartile, c(200,320), 0.85, 0,
         "Word Frequency", "First Fixation Duration (ms)", "Decoding x Word Frequency", 
         "./data/res_F/resFigures/1.ffixurt/Int_ffixurt_decod_f1.png", 24, 6)



# D2 (with some stopwords)
lmer_ffixurt_c <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D2)

# model selection
lmer_ffixurt0 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|subj), data = D2)

lmer_ffixurt1 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|word) + (1|subj), data = D2)

lmer_ffixurt2 <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|itemF) + (1|word) + (1|subj), data = D2)

anova(lmer_ffixurt_c, lmer_ffixurt0)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_ffixurt0  34 268306 268579 -134119   268238                             
# lmer_ffixurt_c 36 268060 268349 -133994   267988 250.09      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_ffixurt_c, lmer_ffixurt1)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_ffixurt1  35 268119 268400 -134025   268049                             
# lmer_ffixurt_c 36 268060 268349 -133994   267988 61.458      1  4.522e-15 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_ffixurt_c, lmer_ffixurt2)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_ffixurt_c 36 268060 268349 -133994   267988                        
# lmer_ffixurt2  36 268113 268402 -134020   268041     0      0          1

# conclusion: lmer_ffixurt_c is the best!

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

# calculate delta-R squared
Rs_ffixurt <- r.squaredGLMM(lmer_ffixurt_c)
# R2m       R2c 
# 0.04483077 0.14893239

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_ffixurt_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_ffixurt_c))
# 4.331512

# run the party implementation
library(party)
cf_ffixurt_c <- cforest(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_ffixurt_c_varimp <- varimp(cf_ffixurt_c)
cf_ffixurt_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 538.15059              607.03267              495.59406              544.84281              542.25775 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 448.37457              194.52726              135.99045              104.37583               64.29837 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 162.07771               23.54237               13.97157 
png("./data/res_F/resFigures/1.ffixurt/cf_ffixurt_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_ffixurt_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_ffixurt_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# region_C
lmer_ffixurt_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                            + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                            + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                            + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                            + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                            + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_region <- r.squaredGLMM(lmer_ffixurt_region)
# std_res_f_coca_log
lmer_ffixurt_f <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_f <- r.squaredGLMM(lmer_ffixurt_f)
# std_res_prevf_coca_log
lmer_ffixurt_prevf <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_prevf <- r.squaredGLMM(lmer_ffixurt_prevf)
# prevwlen_C
lmer_ffixurt_prevwlen <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_prevwlen <- r.squaredGLMM(lmer_ffixurt_prevwlen)

# interactions
# printexp.comp_bct:region_C*
lmer_ffixurt_printexp_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                     + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_printexp_region <- r.squaredGLMM(lmer_ffixurt_printexp_region)
# gort.wpm.st:region_C
lmer_ffixurt_gort_region <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_gort_region <- r.squaredGLMM(lmer_ffixurt_gort_region)
# decod.comp_bct:reglen_C*
lmer_ffixurt_decod_reglen <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1|subj), data = D2)
Rs_ffixurt_decod_reglen <- r.squaredGLMM(lmer_ffixurt_decod_reglen)

Rs1 <- Rs_ffixurt["R2m"] 
Rs2 <- c(Rs_ffixurt_region["R2m"], Rs_ffixurt_f["R2m"], Rs_ffixurt_prevf["R2m"], Rs_ffixurt_prevwlen["R2m"], 
         Rs_ffixurt_printexp_region["R2m"], Rs_ffixurt_gort_region["R2m"], Rs_ffixurt_decod_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0038316212 0.0041460412 0.0024483513 0.0024520253
# 0.0003912878* 0.0004142257 0.0002749870*

# draw significant main effects:
# region_C
Draw_MainEffect("linear", D2, region, ffixurt, c(220,265), 0.75, "Word Position", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_region2.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, ffixurt, c(220,265), 0.75, "Word Frequency", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_f2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, ffixurt, c(220,265), 0.75, "Previous Word Frequency", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("linear", D2, prevwlen, ffixurt, c(220,265), 0.75, "Previous Word Length", "First Fixation Duration (ms)", 
                "./data/res_F/resFigures/1.ffixurt/Main_ffixurt_prevwlen2.png", 6, 6)

# draw significant interactions:
# printexp.comp_bct:region_C*
LabQuartile <- c("Low Print Exp.", "", "", "High Print Exp.")
Draw_Int("linear", "number", 0, D2, region, ffixurt, printexp, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Position", "First Fixation Duration (ms)", "Print Experience x Word Position", 
         "./data/res_F/resFigures/1.ffixurt/Int_ffixurt_printexp_region2.png", 24, 6)
# gort.wpm_S:region_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, ffixurt, gort, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Position", "First Fixation Duration (ms)", "Oral Reading Fluency x Word Position", 
         "./data/res_F/resFigures/1.ffixurt/Int_ffixurt_gort_region2.png", 24, 6)
# decod.comp_bct:reglen_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, ffixurt, decod, numQuartile, LabQuartile, c(200,280), 0.85, 0,
         "Word Length", "First Fixation Duration (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/1.ffixurt/Int_ffixurt_decod_reglen2.png", 24, 6)



### fpurt
# D1 (without stopwords)
lmer_fpurt_i <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D1)

# model selection
lmer_fpurt0 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|subj), data = D1)

lmer_fpurt1 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|word) + (1|subj), data = D1)

lmer_fpurt2 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF) + (1|word) + (1|subj), data = D1)

anova(lmer_fpurt_i, lmer_fpurt0)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpurt0  34 187725 187984 -93829   187657                            
# lmer_fpurt_i 36 187294 187568 -93611   187222   435      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpurt_i, lmer_fpurt1)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpurt1  35 187291 187558 -93611   187221                        
# lmer_fpurt_i 36 187294 187568 -93611   187222     0      1          1
anova(lmer_fpurt_i, lmer_fpurt2)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpurt_i 36 187294 187568 -93611   187222                             
# lmer_fpurt2  36 187293 187566 -93610   187221 1.9073      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# conclusion: lmer_fpurt_i is the best!

summary(lmer_fpurt_i)
# REML criterion at convergence: 187141.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.0169 -0.6270 -0.1798  0.4423  5.7896 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)  1022.29  31.973 
# itemF      (Intercept)    42.03   6.483 
# subj       (Intercept)  1340.74  36.616 
# Residual               16366.11 127.930 
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          291.33516    5.89819   49.39
# oral.comp_bct                        -16.27752   11.75628   -1.38
# decod.comp_bct                       -14.70357    8.28334   -1.78
# readcomp.comp_bct                     11.65211   11.43783    1.02
# printexp.comp_bct                     -8.08723   10.75204   -0.75
# gort.wpm.st                          -20.94694    7.86426   -2.66 0.01160014
# sspan.corr.st                         11.93859    7.94870    1.50
# region_C                               1.95316    0.60298    3.24 0.002095971
# std_res_f_coca_log                   -15.26488    1.95032   -7.83 1.940223e-14
# reglen_C                              13.40139    0.96750   13.85 8.854895e-43
# std_res_prevf_coca_log                -4.61692    1.95179   -2.37 0.02405557
# prevwlen_C                             1.24496    0.94748    1.31
# std_res_nextf_coca_log                -7.48445    1.96738   -3.80 0.0002919469
# nextwlen_C                             1.53362    0.88810    1.73
# oral.comp_bct:region_C                 0.02502    0.67915    0.04
# decod.comp_bct:region_C                0.85835    0.47442    1.81
# readcomp.comp_bct:region_C            -0.09149    0.65416   -0.14
# printexp.comp_bct:region_C            -0.76560    0.62068   -1.23
# gort.wpm.st:region_C                   1.23932    0.45102    2.75 0.009093563
# sspan.corr.st:region_C                -0.48002    0.46308   -1.04
# oral.comp_bct:std_res_f_coca_log      -1.48139    2.19392   -0.68
# decod.comp_bct:std_res_f_coca_log      3.48920    1.52873    2.28 0.02965458
# readcomp.comp_bct:std_res_f_coca_log  -1.76055    2.10947   -0.83
# printexp.comp_bct:std_res_f_coca_log   4.56117    1.99741    2.28 0.02965458
# gort.wpm.st:std_res_f_coca_log         3.79120    1.45790    2.60 0.01358297
# sspan.corr.st:std_res_f_coca_log       0.13513    1.48773    0.09
# oral.comp_bct:reglen_C                -1.24413    1.09775   -1.13
# decod.comp_bct:reglen_C               -2.79998    0.76219   -3.67 0.0004744338
# readcomp.comp_bct:reglen_C            -0.95634    1.05244   -0.91
# printexp.comp_bct:reglen_C            -0.86150    0.98738   -0.87
# gort.wpm.st:reglen_C                  -2.61847    0.72177   -3.63 0.0005490129
# sspan.corr.st:reglen_C                 3.03013    0.73615    4.12 8.221782e-05

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: gort.wpm.st*, region_C*, std_res_f_coca_log, reglen_C, std_res_nextf_coca_log
# significant interactions: gort.wpm.st:region_C*, decod:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# calculate delta-R squared
Rs_fpurt <- r.squaredGLMM(lmer_fpurt_i)
# R2m       R2c 
# 0.1173206 0.2304144

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpurt_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpurt_i))
# 4.341609

# run the party implementation
library(party)
cf_fpurt_i <- cforest(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpurt_i_varimp <- varimp(cf_fpurt_i)
cf_fpurt_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 914.0873              1047.5507               699.5075              1083.1980              1857.4352 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 610.4397               328.0308               715.4462              1561.4826               173.0565 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 127.7316               238.0421               121.2274 
png("./data/res_F/resFigures/2.fpurt/cf_fpurt_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpurt_i_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpurt_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# gort.wpm.st*
lmer_fpurt_gort <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + sspan.corr.st 
                          + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_gort <- r.squaredGLMM(lmer_fpurt_gort)
# region_C*
lmer_fpurt_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_region <- r.squaredGLMM(lmer_fpurt_region)
# std_res_f_coca_log
lmer_fpurt_f <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_f <- r.squaredGLMM(lmer_fpurt_f)
# reglen_C
lmer_fpurt_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_reglen <- r.squaredGLMM(lmer_fpurt_reglen)
# std_res_nextf_coca_log
lmer_fpurt_nextf <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_nextf <- r.squaredGLMM(lmer_fpurt_nextf)

# interactions
# gort.wpm.st:region_C*
lmer_fpurt_gort_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_gort_region <- r.squaredGLMM(lmer_fpurt_gort_region)
# decod:reglen_C 
lmer_fpurt_decod_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_decod_reglen <- r.squaredGLMM(lmer_fpurt_decod_reglen)
# gort.wpm_S:reglen_C
lmer_fpurt_gort_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_gort_reglen <- r.squaredGLMM(lmer_fpurt_gort_reglen)
# sspan.corr_S:reglen_C
lmer_fpurt_sspan_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1)
Rs_fpurt_sspan_reglen <- r.squaredGLMM(lmer_fpurt_sspan_reglen)

Rs1 <- Rs_fpurt["R2m"] 
Rs2 <- c(Rs_fpurt_gort["R2m"], Rs_fpurt_region["R2m"], Rs_fpurt_f["R2m"], Rs_fpurt_reglen["R2m"], Rs_fpurt_nextf["R2m"],
         Rs_fpurt_gort_region["R2m"], Rs_fpurt_decod_reglen["R2m"], Rs_fpurt_gort_reglen["R2m"], Rs_fpurt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0100105589* 0.0018705280* 0.0104066588 0.0317948432 0.0028223500 
# 0.0004019548* 0.0006653299 0.0006444882 0.0008668721 

# draw significant main effects:
# gort.wpm.st*
Draw_MainEffect("linear", D1, gort, fpurt, c(250,400), 0.75, 
                "Oral Reading Fluency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_gort1.png", 6, 6)
# region_C*
Draw_MainEffect("linear", D1, region, fpurt, c(250,400), 0.75, 
                "Word Position", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_region1.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D1, f, fpurt, c(250,400), 0.75, 
                "Word Frequency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_f1.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D1, reglen, fpurt, c(250,400), 0.75, 
                "Word Length", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_reglen1.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("linear", D1, nextf, fpurt, c(250,400), 0.75, 
                "Next Word Frequency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_nextf1.png", 6, 6)

# draw significant interactions:
# gort.wpm.st:region_C*
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D1, region, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Position", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_gort_region1.png", 24, 6)
# decod:reglen_C
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, reglen, fpurt, decod, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_decod_reglen1.png", 24, 6)
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, reglen, fpurt, decod, 2, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_decod_reglen1_2groups.png", 12, 6)
# gort.wpm_S:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D1, reglen, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_gort_reglen1.png", 24, 6)
# sspan.corr_S:reglen_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D1, reglen, fpurt, sspan, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time", "Verbal Working Memory x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_sspan_reglen1.png", 24, 6)



# D2 (with some stopwords)
lmer_fpurt_c <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D2)

# model selection
lmer_fpurt0 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|subj), data = D2)

lmer_fpurt1 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|word) + (1|subj), data = D2)

lmer_fpurt2 <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF) + (1|word) + (1|subj), data = D2)

anova(lmer_fpurt_c, lmer_fpurt0)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpurt0  34 282851 283124 -141392   282783                            
# lmer_fpurt_c 36 282285 282573 -141106   282213 570.6      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpurt_c, lmer_fpurt1)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpurt1  35 282257 282538 -141094   282187                        
# lmer_fpurt_c 36 282285 282573 -141106   282213     0      1          1
anova(lmer_fpurt_c, lmer_fpurt2)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpurt_c 36 282285 282573 -141106   282213                             
# lmer_fpurt2  36 282259 282548 -141093   282187 25.735      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# conclusion: lmer_fpurt_c is the best!

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

# calculate delta-R squared
Rs_fpurt <- r.squaredGLMM(lmer_fpurt_c)
# R2m       R2c 
# 0.1078773 0.2105722

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpurt_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpurt_c))
# 4.330561

# run the party implementation
library(party)
cf_fpurt_c <- cforest(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpurt_c_varimp <- varimp(cf_fpurt_c)
cf_fpurt_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 1020.78116             1008.10510              757.65088              801.28750             1490.15456 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 699.05720              274.79427              513.12660             1852.49080              131.14664 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 229.61603              110.97684               90.28718 
png("./data/res_F/resFigures/2.fpurt/cf_fpurt_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpurt_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpurt_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# main effects
# region_C*
lmer_fpurt_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_region <- r.squaredGLMM(lmer_fpurt_region)
# std_res_f_coca_log
lmer_fpurt_f <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_f <- r.squaredGLMM(lmer_fpurt_f)
# reglen_C
lmer_fpurt_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                          + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_reglen <- r.squaredGLMM(lmer_fpurt_reglen)
# std_res_prevf_coca_log
lmer_fpurt_prevf <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_prevf <- r.squaredGLMM(lmer_fpurt_prevf)
# std_res_nextf_coca_log
lmer_fpurt_nextf <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_nextf <- r.squaredGLMM(lmer_fpurt_nextf)

# interactions
# gort.wpm.st:region_C
lmer_fpurt_gort_region <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_gort_region <- r.squaredGLMM(lmer_fpurt_gort_region)
# decod.comp_bct:reglen_C
lmer_fpurt_decod_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_decod_reglen <- r.squaredGLMM(lmer_fpurt_decod_reglen)
# gort.wpm.st:reglen_C
lmer_fpurt_gort_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_gort_reglen <- r.squaredGLMM(lmer_fpurt_gort_reglen)
# sspan.corr.st:reglen_C
lmer_fpurt_sspan_reglen <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C 
                                + (1|itemF/word) + (1|subj), data = D2)
Rs_fpurt_sspan_reglen <- r.squaredGLMM(lmer_fpurt_sspan_reglen)

Rs1 <- Rs_fpurt["R2m"] 
Rs2 <- c(Rs_fpurt_region["R2m"], Rs_fpurt_f["R2m"], Rs_fpurt_reglen["R2m"], Rs_fpurt_prevf["R2m"], Rs_fpurt_nextf["R2m"],
         Rs_fpurt_gort_region["R2m"], Rs_fpurt_decod_reglen["R2m"], Rs_fpurt_gort_reglen["R2m"], Rs_fpurt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0008485754* 0.0052819525 0.0381078832 0.0027915640 0.0021930783
# 0.0006174584 0.0007768648 0.0013753523 0.0004759151

# draw significant main effects:
# region_C*
Draw_MainEffect("linear", D2, region, fpurt, c(200,400), 0.75, 
                "Word Position", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_region2.png", 6, 6)
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, fpurt, c(200,400), 0.75, 
                "Word Frequency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_f2.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D2, reglen, fpurt, c(200,400), 0.75, 
                "Word Length", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_reglen2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, fpurt, c(200,400), 0.75, 
                "Previous Word Frequency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_prevf2.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("linear", D2, nextf, fpurt, c(200,400), 0.75, 
                "Next Word Frequency", "First-pass Reading Time (ms)", 
                "./data/res_F/resFigures/2.fpurt/Main_fpurt_nextf2.png", 6, 6)

# draw significant interactions:
# gort.wpm.st:region_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Position", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Position", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_gort_region2.png", 24, 6)
# decod.comp_bct:reglen_C
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, decod, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_decod_reglen2.png", 24, 6)
LabQuartile <- c("Low Decod.", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, decod, 2, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_decod_reglen2_2groups.png", 12, 6)
# gort.wpm.st:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, gort, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, reglen, fpurt, sspan, numQuartile, LabQuartile, c(200,500), 0.85, 0, 
         "Word Length", "First-pass Reading Time (ms)", "Verbal Working Memory x Word Length", 
         "./data/res_F/resFigures/2.fpurt/Int_fpurt_sspan_reglen2.png", 24, 6)



### ffos
# D1
lmer_ffos_i <- lmer(ffos ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1|subj), data = D1)
summary(lmer_ffos_i)
# REML criterion at convergence: 58272.1
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9911 -0.7759 -0.0227  0.6864  5.1009 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.074536 0.27301 
# itemF      (Intercept) 0.003639 0.06033 
# subj       (Intercept) 0.080794 0.28424 
# Residual               2.830497 1.68241 
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                           2.4060312  0.0476512   50.49
# oral.comp_bct                        -0.0536474  0.0941937   -0.57
# decod.comp_bct                       -0.0312002  0.0663316   -0.47
# readcomp.comp_bct                     0.0301501  0.0915963    0.33
# printexp.comp_bct                     0.1516397  0.0861236    1.76
# gort.wpm.st                          -0.0536588  0.0629931   -0.85
# sspan.corr.st                        -0.0025277  0.0637271   -0.04
# region_C                              0.0146650  0.0061237    2.39  0.02293735
# std_res_f_coca_log                   -0.0142172  0.0197217   -0.72
# reglen_C                              0.2274009  0.0098073   23.19  6.672886e-118
# std_res_prevf_coca_log                0.0361782  0.0197610    1.83
# prevwlen_C                            0.0889223  0.0096193    9.24  1.15186e-19
# std_res_nextf_coca_log               -0.0296629  0.0199001   -1.49
# nextwlen_C                            0.0105258  0.0090058    1.17
# oral.comp_bct:region_C                0.0062351  0.0089279    0.70
# decod.comp_bct:region_C               0.0067351  0.0062365    1.08
# readcomp.comp_bct:region_C            0.0008222  0.0085993    0.10
# printexp.comp_bct:region_C           -0.0135726  0.0081582   -1.66
# gort.wpm.st:region_C                 -0.0174507  0.0059282   -2.94  0.005296344
# sspan.corr.st:region_C                0.0113493  0.0060870    1.86
# oral.comp_bct:std_res_f_coca_log      0.0204696  0.0288395    0.71
# decod.comp_bct:std_res_f_coca_log     0.0361522  0.0200979    1.80
# readcomp.comp_bct:std_res_f_coca_log  0.0173301  0.0277309    0.62
# printexp.comp_bct:std_res_f_coca_log -0.0582410  0.0262575   -2.22  0.03246027
# gort.wpm.st:std_res_f_coca_log       -0.0100898  0.0191638   -0.53
# sspan.corr.st:std_res_f_coca_log     -0.0165437  0.0195569   -0.85
# oral.comp_bct:reglen_C                0.0222988  0.0144284    1.55
# decod.comp_bct:reglen_C              -0.0043249  0.0100193   -0.43
# readcomp.comp_bct:reglen_C           -0.0025033  0.0138335   -0.18
# printexp.comp_bct:reglen_C            0.0082652  0.0129779    0.64
# gort.wpm.st:reglen_C                 -0.0127021  0.0094869   -1.34
# sspan.corr.st:reglen_C               -0.0028445  0.0096758   -0.29

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: reglen_C, prewlen_C
# significant interactions: gort.wpm.st:region_C*

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_ffos_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_ffos_i))
# 4.343233

# run the party implementation
library(party)
cf_ffos_i <- cforest(ffos ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_ffos_i_varimp <- varimp(cf_ffos_i)
cf_ffos_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.06040597             0.03563282             0.04250795             0.07314500             0.04491411 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.03363880             0.08395873             0.02409885             0.33164646             0.04083796 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.14722050             0.03372928             0.03241254 
png("./data/res_F/resFigures/cf_ffos_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpurt_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_ffos_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()


# D2 (with some stopwords)
lmer_ffos_c <- lmer(ffos ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1|subj), data = D2)
summary(lmer_ffos_c)
# REML criterion at convergence: 84106.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4195 -0.7631 -0.0214  0.6886  5.6616 
# 
# Random effects:
#   Groups     Name        Variance  Std.Dev.
# word:itemF (Intercept) 0.0765027 0.2766  
# itemF      (Intercept) 0.0001988 0.0141  
# subj       (Intercept) 0.0440382 0.2099  
# Residual               2.3440567 1.5310  
# Number of obs: 22579, groups:  word:itemF, 714; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                           2.1374369  0.0349982   61.07
# oral.comp_bct                        -0.0514233  0.0696334   -0.74
# decod.comp_bct                       -0.0167554  0.0489765   -0.34
# readcomp.comp_bct                     0.0130616  0.0676074    0.19
# printexp.comp_bct                     0.1278915  0.0636072    2.01
# gort.wpm.st                          -0.0350788  0.0464681   -0.75
# sspan.corr.st                         0.0004896  0.0470618    0.01
# region_C                              0.0076164  0.0046137    1.65
# std_res_f_coca_log                   -0.0363013  0.0150101   -2.42  0.02134071
# reglen_C                              0.2779505  0.0067623   41.10  0
# std_res_prevf_coca_log                0.0247160  0.0148334    1.67
# prevwlen_C                            0.0770105  0.0070216   10.97  2.945884e-27
# std_res_nextf_coca_log               -0.0194896  0.0146648   -1.33
# nextwlen_C                            0.0051127  0.0066374    0.77
# oral.comp_bct:region_C                0.0047262  0.0067609    0.70
# decod.comp_bct:region_C               0.0031411  0.0046890    0.67
# readcomp.comp_bct:region_C            0.0056856  0.0064300    0.88
# printexp.comp_bct:region_C           -0.0126044  0.0061122   -2.06
# gort.wpm.st:region_C                 -0.0107894  0.0044094   -2.45  0.01983735  
# sspan.corr.st:region_C                0.0052669  0.0045712    1.15
# oral.comp_bct:std_res_f_coca_log      0.0266198  0.0212459    1.25
# decod.comp_bct:std_res_f_coca_log     0.0189635  0.0146891    1.29
# readcomp.comp_bct:std_res_f_coca_log -0.0043783  0.0202834   -0.22
# printexp.comp_bct:std_res_f_coca_log -0.0307122  0.0192327   -1.60
# gort.wpm.st:std_res_f_coca_log        0.0069111  0.0139583    0.50
# sspan.corr.st:std_res_f_coca_log     -0.0107639  0.0144153   -0.75
# oral.comp_bct:reglen_C                0.0033801  0.0097350    0.35
# decod.comp_bct:reglen_C              -0.0075491  0.0066787   -1.13
# readcomp.comp_bct:reglen_C            0.0114811  0.0091797    1.25
# printexp.comp_bct:reglen_C            0.0218522  0.0086954    2.51  0.01709467
# gort.wpm.st:reglen_C                 -0.0185166  0.0062831   -2.95  0.005142641
# sspan.corr.st:reglen_C               -0.0040373  0.0064649   -0.62

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: reglen_C, prewlen_C
# significant interactions: gort.wpm.st:region_C*, gort.wpm.st:reglen_C*

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_ffos_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_ffos_c))
# 4.332144

# run the party implementation
library(party)
cf_ffos_c <- cforest(ffos ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_ffos_c_varimp <- varimp(cf_ffos_c)
cf_ffos_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.04446715             0.03519926             0.03478238             0.07865848             0.04018228 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.03116040             0.06665521             0.05402488             0.58248806             0.05939883 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.11906599             0.02835516             0.02415842 
png("./data/res_F/resFigures/cf_ffos_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_ffos_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_ffos_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()



### fpregres
# D1 (without stopwords)
lmer_fpregres_i <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

# model selection!
lmer_fpregres0 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres1 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres2 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF) + (1|word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

anova(lmer_fpregres_i, lmer_fpregres0)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres0  33 12815 13066 -6374.3    12749                             
# lmer_fpregres_i 35 12452 12718 -6191.0    12382 366.74      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_i, lmer_fpregres1)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres1  34 12496 12754 -6213.9    12428                             
# lmer_fpregres_i 35 12452 12718 -6191.0    12382 45.873      1  1.262e-11 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_i, lmer_fpregres2)
# Df   AIC   BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpregres_i 35 12452 12718 -6191.0    12382                        
# lmer_fpregres2  35 12486 12752 -6208.1    12416     0      0          1

# conclusion: lmer_fpregres_i is the best!

summary(lmer_fpregres_i)
# AIC      BIC   logLik deviance df.resid 
# 12451.9  12718.2  -6191.0  12381.9    14846 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8156 -0.4584 -0.3401 -0.2352  5.1143 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.450263 0.67102 
# itemF      (Intercept) 0.006203 0.07876 
# subj       (Intercept) 0.202003 0.44945 
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.885741   0.081187 -23.227  < 2e-16 ***
#   oral.comp_bct                        -0.055524   0.150946  -0.368  0.71299    
# decod.comp_bct                        0.016694   0.106073   0.157  0.87494    
# readcomp.comp_bct                     0.091655   0.147175   0.623  0.53344    
# printexp.comp_bct                     0.136156   0.138114   0.986  0.32422    
# gort.wpm.st                          -0.200638   0.100435  -1.998  0.04575 *  
#   sspan.corr.st                        -0.218189   0.101123  -2.158  0.03095 *  
#   region_C                              0.012077   0.012947   0.933  0.35095    
# std_res_f_coca_log                   -0.071238   0.041734  -1.707  0.08784 .  
# reglen_C                              0.018741   0.021195   0.884  0.37657    
# std_res_prevf_coca_log               -0.120477   0.042904  -2.808  0.00498 ** 
#   prevwlen_C                           -0.103686   0.021010  -4.935 8.01e-07 ***
#   std_res_nextf_coca_log               -0.004405   0.042334  -0.104  0.91712    
# nextwlen_C                            0.012668   0.019221   0.659  0.50983    
# oral.comp_bct:region_C               -0.004501   0.014930  -0.301  0.76308    
# decod.comp_bct:region_C               0.026951   0.010288   2.620  0.00880 ** 
#   readcomp.comp_bct:region_C            0.011849   0.014654   0.809  0.41877    
# printexp.comp_bct:region_C            0.007054   0.013691   0.515  0.60640    
# gort.wpm.st:region_C                  0.007879   0.009539   0.826  0.40882    
# sspan.corr.st:region_C               -0.020910   0.009505  -2.200  0.02781 *  
#   oral.comp_bct:std_res_f_coca_log      0.005179   0.048905   0.106  0.91566    
# decod.comp_bct:std_res_f_coca_log    -0.012113   0.033422  -0.362  0.71704    
# readcomp.comp_bct:std_res_f_coca_log  0.007510   0.048201   0.156  0.87618    
# printexp.comp_bct:std_res_f_coca_log -0.038726   0.044893  -0.863  0.38835    
# gort.wpm.st:std_res_f_coca_log       -0.019416   0.031155  -0.623  0.53314    
# sspan.corr.st:std_res_f_coca_log     -0.046358   0.030745  -1.508  0.13160    
# oral.comp_bct:reglen_C               -0.057138   0.025263  -2.262  0.02372 *  
#   decod.comp_bct:reglen_C               0.017599   0.017361   1.014  0.31071    
# readcomp.comp_bct:reglen_C            0.030198   0.024920   1.212  0.22559    
# printexp.comp_bct:reglen_C            0.035089   0.023080   1.520  0.12843    
# gort.wpm.st:reglen_C                 -0.038095   0.016250  -2.344  0.01906 *  
#   sspan.corr.st:reglen_C                0.014195   0.015730   0.902  0.36685   
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: std_res_prevf_coca_log*, prevwlen_C
# significant interactions: decod.comp_bct:region_C*

# calculate delta-R squared
Rs_fpregres <- r.squared(lmer_fpregres_i)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.04140271   0.2012692 12451.94

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_i))
# 4.55142

# run the party implementation
library(party)
cf_fpregres_i <- cforest(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_i_varimp <- varimp(cf_fpregres_i)
cf_fpregres_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.003195504            0.002411266            0.002503709            0.002039777            0.004310732 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.002592610            0.003505687            0.001476142            0.001162275            0.002746344 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.004395938            0.001315082            0.001197138
png("./data/res_F/resFigures/3.fpregres/cf_fpregres_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_i_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# std_res_prevf_coca_log*
lmer_fpregres_prevf <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevf <- r.squared(lmer_fpregres_prevf)
# prevwlen_C
lmer_fpregres_prevwlen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevwlen <- r.squared(lmer_fpregres_prevwlen)

# decod.comp_bct:region_C*
lmer_fpregres_decod_region <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                    + oral.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                    + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_decod_region <- r.squared(lmer_fpregres_decod_region)

fpregres_marg <- c(rep(Rs_fpregres$Marginal, 2)) - c(Rs_fpregres_prevf$Marginal, Rs_fpregres_prevwlen$Marginal)
# 0.002827992* 0.011123976
fpregres_marg_int <- c(rep(Rs_fpregres$Marginal, 1)) - c(Rs_fpregres_decod_region$Marginal)
# 0.0007875384*

# draw significant main effects:
# std_res_prevf_coca_log*
Draw_MainEffect("logistic", D1, prevf, fpregres, c(0.05,0.3), 0.75, "Previous Word Frequency", "Regression_out", 
                "./data/res_F/resFigures/3.fpregres/Main_fpregres_prevf1.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D1, prevwlen, fpregres, c(0.05,0.3), 0.75, "Previous Word Length", "Regression_out", 
                "./data/res_F/resFigures/3.fpregres/Main_fpregres_prevwlen1.png", 6, 6)

# draw significant interactions:
# decod.comp_bct:region_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("logistic", "number", 0, D1, region, fpregres, decod, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Position", "Regression_out", "Decoding x Word Position", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_decod_region1.png", 24, 6)



# D2 (with stopwords)
lmer_fpregres_c <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

# model selection!
lmer_fpregres0 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres1 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres2 <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF) + (1|word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

anova(lmer_fpregres_c, lmer_fpregres0)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres0  33 18995 19260 -9464.6    18929                            
# lmer_fpregres_c 35 18563 18844 -9246.4    18493 436.3      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_c, lmer_fpregres1)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres1  34 18647 18920 -9289.4    18579                             
# lmer_fpregres_c 35 18563 18844 -9246.4    18493 85.992      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_c, lmer_fpregres2)
# Df   AIC   BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpregres_c 35 18563 18844 -9246.4    18493                        
# lmer_fpregres2  35 18638 18919 -9284.2    18568     0      0          1

# conclusion: lmer_fpurt is the best!

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
# oral.comp_bct:reglen_C*, printexp.comp_bct:reglen_C*, gort.wpm_S:reglen_C, sspan.corr.st:reglen_C*

# calculate delta-R squared
Rs_fpregres <- r.squared(lmer_fpregres_c)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.04645786   0.1984449 18562.81

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_c))
# 4.511366

# run the party implementation
library(party)
cf_fpregres_c <- cforest(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_c_varimp <- varimp(cf_fpregres_c)
cf_fpregres_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.0027824723           0.0035682312           0.0020443860           0.0018008748           0.0036144290 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.0036858069           0.0036803168           0.0007435291           0.0007245338           0.0018679406 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.0042415495           0.0009072845           0.0006629945 
png("./data/res_F/resFigures/3.fpregres/cf_fpregres_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# std_res_prevf_coca_log*
lmer_fpregres_prevf <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C +  prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevf <- r.squared(lmer_fpregres_prevf)
# prevwlen_C
lmer_fpregres_prevwlen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_prevwlen <- r.squared(lmer_fpregres_prevwlen)
# decod.comp_bct:region_C*
lmer_fpregres_decod_region <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                    + oral.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                    + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_decod_region <- r.squared(lmer_fpregres_decod_region)
# sspan.corr.st:region_C*
lmer_fpregres_sspan_region <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C 
                                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                    + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_sspan_region <- r.squared(lmer_fpregres_sspan_region)
# oral.comp_bct:reglen_C*
lmer_fpregres_oral_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_oral_reglen <- r.squared(lmer_fpregres_oral_reglen)
# printexp.comp_bct:reglen_C*
lmer_fpregres_printexp_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                       + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_printexp_reglen <- r.squared(lmer_fpregres_printexp_reglen)
# gort.wpm.st:reglen_C
lmer_fpregres_gort_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                                   + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_gort_reglen <- r.squared(lmer_fpregres_gort_reglen)
# sspan.corr.st:reglen_C*
lmer_fpregres_sspan_reglen <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C
                                   + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
Rs_fpregres_sspan_reglen <- r.squared(lmer_fpregres_sspan_reglen)


fpregres_marg <- c(rep(Rs_fpregres$Marginal, 2)) - c(Rs_fpregres_prevf$Marginal, Rs_fpregres_prevwlen$Marginal)
# 0.002086409* 0.017403959
fpregres_marg_int <- c(rep(Rs_fpregres$Marginal, 6)) - c(Rs_fpregres_decod_region$Marginal, Rs_fpregres_sspan_region$Margina, 
                                                         Rs_fpregres_oral_reglen$Marginal, Rs_fpregres_printexp_reglen$Marginal, Rs_fpregres_gort_reglen$Marginal, Rs_fpregres_sspan_reglen$Marginal)
# 0.0006377776* 0.0002488991* 0.0004978510* 0.0008483402* 0.0011527500 0.0003650669*

# draw significant main effects:
# std_res_prevf_coca_log*
Draw_MainEffect("logistic", D2, prevf, fpregres, c(0.05,0.3), 0.75, "Previous Word Frequency", "Regression_out", 
                "./data/res_F/resFigures/3.fpregres/Main_fpregres_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D2, prevwlen, fpregres, c(0.05,0.3), 0.75, "Previous Word Length", "Regression_out", 
                "./data/res_F/resFigures/3.fpregres/Main_fpregres_prevwlen2.png", 6, 6)

# draw significant interactions:
# decod.comp_bct:region_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("logistic", "number", 0, D2, region, fpregres, decod, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Position", "Regression_out", "Decoding x Word Position", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_decod_region2.png", 24, 6)
# sspan.corr.st:region_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("logistic", "number", 0, D2, region, fpregres, sspan, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Position", "Regression_out", "Verbal Working Memory x Word Position", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_sspan_region2.png", 24, 6)
# oral.comp_bct:reglen_C*
LabQuartile <- c("Low Oral Know.", "", "", "High Oral Know.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, oral, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Oral Language Skill x Word Length", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_oral_reglen2.png", 24, 6)
# printexp.comp_bct_S:reglen_C*
LabQuartile <- c("Low Print Exp.", "", "", "High Print Exp.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, printexp, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Print Experience x Word Length", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_printexp_reglen2.png", 24, 6)
# gort.wpm_S:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, gort, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Oral Reading Fluency x Word Length", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("logistic", "number", 0, D2, reglen, fpregres, sspan, numQuartile, LabQuartile, c(0.05,0.3), 0.85, 0,
         "Word Length", "Regression_out", "Verbal Working Memory x Word Length", 
         "./data/res_F/resFigures/3.fpregres/Int_fpregres_sspan_reglen2.png", 24, 6)



# fpregres_in
# D1 (without stopwords)
lmer_fpregres_in_i <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

# model selection!
lmer_fpregres_in0 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres_in1 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres_in2 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF) + (1|word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

anova(lmer_fpregres_in_i, lmer_fpregres_in0)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres_in0  33 15965 16216 -7949.6    15899                             
# lmer_fpregres_in_i 35 15759 16026 -7844.7    15689 209.87      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_in_i, lmer_fpregres_in1)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres_in1  34 15771 16030 -7851.6    15703                             
# lmer_fpregres_in_i 35 15759 16026 -7844.7    15689 13.813      1  0.0002019 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_in_i, lmer_fpregres_in2)
# Df   AIC   BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpregres_in_i 35 15759 16026 -7844.7    15689                             
# lmer_fpregres_in2  35 15757 16023 -7843.5    15687 2.3641      0  < 2.2e-16 ***
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# conclusion: lmer_fpregres_out_i is the best!

summary(lmer_fpregres_in_i)
# AIC      BIC   logLik deviance df.resid 
# 15759.4  16025.6  -7844.7  15689.4    14846 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8839 -0.5878 -0.4375 -0.1873  5.1166 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept) 0.16604  0.4075  
# itemF      (Intercept) 0.04973  0.2230  
# subj       (Intercept) 0.29992  0.5476  
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.2414763  0.0917343 -13.533  < 2e-16 ***
#   oral.comp_bct                        -0.0591123  0.1782778  -0.332 0.740211    
# decod.comp_bct                        0.0105818  0.1254356   0.084 0.932770    
# readcomp.comp_bct                     0.1180525  0.1732782   0.681 0.495689    
# printexp.comp_bct                    -0.0702090  0.1629599  -0.431 0.666588    
# gort.wpm.st                          -0.0354942  0.1188589  -0.299 0.765226    
# sspan.corr.st                         0.0003568  0.1199728   0.003 0.997627    
# region_C                             -0.0628873  0.0091886  -6.844 7.70e-12 ***
#   std_res_f_coca_log                   -0.0483278  0.0301408  -1.603 0.108846    
# reglen_C                              0.0771978  0.0149319   5.170 2.34e-07 ***
#   std_res_prevf_coca_log               -0.0933308  0.0302425  -3.086 0.002028 ** 
#   prevwlen_C                           -0.0523086  0.0149468  -3.500 0.000466 ***
#   std_res_nextf_coca_log               -0.1059014  0.0301990  -3.507 0.000454 ***
#   nextwlen_C                            0.0281822  0.0137480   2.050 0.040373 *  
#   oral.comp_bct:region_C                0.0075537  0.0130238   0.580 0.561917    
# decod.comp_bct:region_C               0.0063878  0.0089307   0.715 0.474445    
# readcomp.comp_bct:region_C           -0.0075117  0.0124702  -0.602 0.546927    
# printexp.comp_bct:region_C            0.0122807  0.0117964   1.041 0.297851    
# gort.wpm.st:region_C                 -0.0078365  0.0082584  -0.949 0.342665    
# sspan.corr.st:region_C               -0.0105937  0.0083243  -1.273 0.203152    
# oral.comp_bct:std_res_f_coca_log      0.0659419  0.0415289   1.588 0.112319    
# decod.comp_bct:std_res_f_coca_log     0.0241252  0.0284667   0.847 0.396724    
# readcomp.comp_bct:std_res_f_coca_log -0.0239745  0.0399121  -0.601 0.548051    
# printexp.comp_bct:std_res_f_coca_log -0.0735425  0.0375905  -1.956 0.050417 .  
# gort.wpm.st:std_res_f_coca_log        0.0287779  0.0264656   1.087 0.276874    
# sspan.corr.st:std_res_f_coca_log     -0.0234668  0.0265001  -0.886 0.375868    
# oral.comp_bct:reglen_C                0.0005434  0.0210201   0.026 0.979375    
# decod.comp_bct:reglen_C              -0.0084669  0.0143250  -0.591 0.554484    
# readcomp.comp_bct:reglen_C            0.0128201  0.0201127   0.637 0.523856    
# printexp.comp_bct:reglen_C            0.0073467  0.0187101   0.393 0.694570    
# gort.wpm.st:reglen_C                 -0.0024063  0.0131464  -0.183 0.854769    
# sspan.corr.st:reglen_C                0.0226494  0.0131942   1.717 0.086050 .  
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: region_C, reglen_C, std_res_prevf_coca_log*, prevwlen_C, std_res_nextf_coca_log

# calculate delta-R squared
Rs_fpregres_in <- r.squared(lmer_fpregres_in_i)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.03035346   0.1617488 15759.36

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_in_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_in_i))
# 4.433626

# run the party implementation
library(party)
cf_fpregres_in_i <- cforest(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_in_i_varimp <- varimp(cf_fpregres_in_i)
cf_fpregres_in_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.003464506            0.003527988            0.004115897            0.003168172            0.004200952 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.002809926            0.008017485            0.002016337            0.003771376            0.001536755 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.002484293            0.002252612            0.001127572 
png("./data/res_F/resFigures/4.fpregres_in/cf_fpregres_in_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_in_i_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_in_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# region_C
lmer_fpregres_in_region <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_region <- r.squared(lmer_fpregres_in_region)
# reglen_C
lmer_fpregres_in_reglen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_reglen <- r.squared(lmer_fpregres_in_reglen)
# std_res_prevf_coca_log*
lmer_fpregres_in_prevf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevf <- r.squared(lmer_fpregres_in_prevf)
# prevwlen_C
lmer_fpregres_in_prevwlen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                   + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevwlen <- r.squared(lmer_fpregres_in_prevwlen)
# std_res_nextf_coca_log
lmer_fpregres_in_nextf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_nextf <- r.squared(lmer_fpregres_in_nextf)

fpregres_in_marg <- c(rep(Rs_fpregres_in$Marginal, 5)) - c(Rs_fpregres_in_region$Marginal, Rs_fpregres_in_reglen$Marginal, 
                                                           Rs_fpregres_in_prevf$Marginal, Rs_fpregres_in_prevwlen$Marginal, Rs_fpregres_in_nextf$Marginal)
# 0.0102881678 0.0056317226 0.0004719789* 0.0021215814 0.0029643831

# draw significant main effects:
# region_C
Draw_MainEffect("logistic", D1, region, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Position", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_region1.png", 6, 6)
# reglen_C
Draw_MainEffect("logistic", D1, reglen, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Length", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_reglen1.png", 6, 6)
# std_res_prevf_coca_log*
Draw_MainEffect("logistic", D1, prevf, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Frequency", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_prevf1.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D1, prevwlen, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Length", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_prevwlen1.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("logistic", D1, nextf, fpregres_in, c(0.1,0.4), 0.75, 
                "Next Word Frequency", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_nextf1.png", 6, 6)



# D2 (with stopwords)
lmer_fpregres_in_c <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                             + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                             + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                             + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                             + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                             + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

# model selection!
lmer_fpregres_in0 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres_in1 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

lmer_fpregres_in2 <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF) + (1|word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)

anova(lmer_fpregres_in_c, lmer_fpregres_in0)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres_in0  33 23214 23479 -11574    23148                             
# lmer_fpregres_in_c 35 22923 23204 -11426    22853 295.53      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_in_c, lmer_fpregres_in1)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_fpregres_in1  34 23019 23291 -11475    22951                             
# lmer_fpregres_in_c 35 22923 23204 -11426    22853 97.784      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
anova(lmer_fpregres_in_c, lmer_fpregres_in2)
# Df   AIC   BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_fpregres_in_c 35 22923 23204 -11426    22853                        
# lmer_fpregres_in2  35 22975 23256 -11452    22905     0      0          1

# conclusion: lmer_fpregres_out_i is the best!

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

# calculate delta-R squared
Rs_fpregres_in <- r.squared(lmer_fpregres_in_c)
# Class   Family  Link   Marginal Conditional      AIC
# 1 glmerMod binomial logit 0.03671244    0.173466 22922.79

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_fpregres_in_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_fpregres_in_c))
# 4.417315

# run the party implementation
library(party)
cf_fpregres_in_c <- cforest(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_in_c_varimp <- varimp(cf_fpregres_in_c)
cf_fpregres_in_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 0.003314480            0.003389830            0.003946525            0.003950039            0.004584696 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 0.003427987            0.008153777            0.001442043            0.004171735            0.001837416 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 0.003104052            0.001823219            0.001127651 
png("./data/res_F/resFigures/4.fpregres_in/cf_fpregres_in_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_in_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_in_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

# remove models
# region_C
lmer_fpregres_in_region <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_region <- r.squared(lmer_fpregres_in_region)
# reglen_C
lmer_fpregres_in_reglen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                 + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                 + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                 + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                 + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                 + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_reglen <- r.squared(lmer_fpregres_in_reglen)
# std_res_f_coca_log*
lmer_fpregres_in_f <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_f <- r.squared(lmer_fpregres_in_f)
# std_res_prevf_coca_log
lmer_fpregres_in_prevf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevf <- r.squared(lmer_fpregres_in_prevf)
# prevwlen_C
lmer_fpregres_in_prevwlen <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + std_res_nextf_coca_log + nextwlen_C
                                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                   + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_prevwlen <- r.squared(lmer_fpregres_in_prevwlen)
# std_res_nextf_coca_log
lmer_fpregres_in_nextf <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                                + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                + (1|itemF/word) + (1|subj), data = D2, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e8)), nAGQ = 1)
Rs_fpregres_in_nextf <- r.squared(lmer_fpregres_in_nextf)

fpregres_in_marg <- c(rep(Rs_fpregres_in$Marginal, 6)) - c(Rs_fpregres_in_region$Marginal, Rs_fpregres_in_reglen$Marginal, Rs_fpregres_in_f$Marginal,
                                                           Rs_fpregres_in_prevf$Marginal, Rs_fpregres_in_prevwlen$Marginal, Rs_fpregres_in_nextf$Marginal)
# 0.0052799379 0.0124384883 0.0015670108* 0.0005240159 0.0021669224 0.0033571501

# draw significant main effects:
# region_C
Draw_MainEffect("logistic", D2, region, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Position", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_region2.png", 6, 6)
# reglen_C
Draw_MainEffect("logistic", D2, reglen, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Length", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_reglen2.png", 6, 6)
# std_res_f_coca_log*
Draw_MainEffect("logistic", D2, f, fpregres_in, c(0.1,0.4), 0.75, 
                "Word Frequency", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_f2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("logistic", D2, prevf, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Frequency", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_prevf2.png", 6, 6)
# prevwlen_C
Draw_MainEffect("logistic", D2, prevwlen, fpregres_in, c(0.1,0.4), 0.75, 
                "Previous Word Length", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_prevwlen2.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("logistic", D2, nextf, fpregres_in, c(0.1,0.4), 0.75, 
                "Next Word Frequency", "Regression_in", 
                "./data/res_F/resFigures/4.fpregres_in/Main_fpregres_in_nextf2.png", 6, 6)



### turt
# D1 (without stopwords)
lmer_turt_i <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D1)

# model selection
lmer_turt0 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|subj), data = D1)

lmer_turt1 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|word) + (1|subj), data = D1)

lmer_turt2 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      + (1|itemF) + (1|word) + (1|subj), data = D1)

anova(lmer_turt_i, lmer_turt0)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_turt0  34 202475 202733 -101203   202407                            
# lmer_turt_i 36 201863 202137 -100896   201791 615.3      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

anova(lmer_turt_i, lmer_turt1)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_turt1  35 201869 202136 -100900   201799                            
# lmer_turt_i 36 201863 202137 -100896   201791 7.9552      1   0.004795 **
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

anova(lmer_turt_i, lmer_turt2)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_turt_i 36 201863 202137 -100896   201791                        
# lmer_turt2  36 201867 202141 -100897   201795     0      0          1

summary(lmer_turt_i)
# REML criterion at convergence: 201676.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.6446 -0.5998 -0.1890  0.3671 11.4897 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# word:itemF (Intercept)  3176.5   56.36  
# itemF      (Intercept)   397.8   19.95  
# subj       (Intercept)  4696.3   68.53  
# Residual               43343.0  208.19  
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          374.9179    11.1119   33.74
# oral.comp_bct                        -23.6139    21.9087   -1.08
# decod.comp_bct                       -19.4603    15.4378   -1.26
# readcomp.comp_bct                     22.8589    21.3168    1.07
# printexp.comp_bct                     -9.7786    20.0380   -0.49
# gort.wpm.st                          -30.6480    14.6562   -2.09  0.04491477
# sspan.corr.st                         13.9238    14.8117    0.94
# region_C                              -2.8170     1.0506   -2.68  0.01099694
# std_res_f_coca_log                   -25.7512     3.4364   -7.49  2.623784e-13
# reglen_C                              20.5346     1.6957   12.11  5.699164e-33
# std_res_prevf_coca_log               -11.9441     3.4241   -3.49  0.0009037222
# prevwlen_C                            -2.9223     1.6625   -1.76
# std_res_nextf_coca_log               -14.1614     3.4576   -4.10  8.926166e-05
# nextwlen_C                             3.6981     1.5578    2.37  0.02405557
# oral.comp_bct:region_C                 1.3197     1.1053    1.19
# decod.comp_bct:region_C                1.3091     0.7721    1.70
# readcomp.comp_bct:region_C            -0.7936     1.0646   -0.75
# printexp.comp_bct:region_C             0.8944     1.0102    0.89
# gort.wpm.st:region_C                   1.7293     0.7340    2.36  0.02463127
# sspan.corr.st:region_C                -2.0751     0.7537   -2.75  0.009093563
# oral.comp_bct:std_res_f_coca_log       2.5611     3.5706    0.72
# decod.comp_bct:std_res_f_coca_log      6.8063     2.4880    2.74  0.009346638
# readcomp.comp_bct:std_res_f_coca_log  -4.5749     3.4332   -1.33
# printexp.comp_bct:std_res_f_coca_log   2.9560     3.2508    0.91
# gort.wpm.st:std_res_f_coca_log         7.3326     2.3728    3.09  0.003369508
# sspan.corr.st:std_res_f_coca_log      -2.2899     2.4213   -0.95
# oral.comp_bct:reglen_C                -3.1580     1.7867   -1.77
# decod.comp_bct:reglen_C               -4.7916     1.2405   -3.86  0.000232008
# readcomp.comp_bct:reglen_C             0.5012     1.7129    0.29
# printexp.comp_bct:reglen_C             1.7064     1.6070    1.06
# gort.wpm.st:reglen_C                  -4.4818     1.1747   -3.82  0.000270527
# sspan.corr.st:reglen_C                 4.8024     1.1981    4.01  0.0001285762

# calculate p value from t value: p.value = dt(t.value, df=Inf)

# summary: (*: p value is smaller than 0.05/4=0.0125 but bigger than 0.05/31=0.00161)
# significant main effects: std_res_f_coca_log, reglen_C, std_res_prevf_coca_log, std_res_nextf_coca_log
# significant interactions: sspan.corr.st:region_C*, decod.comp_bct:region_C*, gort.wpm.st:std_res_f_coca_log*, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C

# calculate delta-R squared
Rs_turt <- r.squaredGLMM(lmer_turt_i)
# R2m        R2c 
# 0.09725581 0.24191348 

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_turt_i)
# 5.656352

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_turt_i))
# 4.34128

# run the party implementation
library(party)
cf_turt_i <- cforest(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        , data=D1,control=cforest_unbiased(mtry=2,ntree=50))
cf_turt_i_varimp <- varimp(cf_turt_i)
cf_turt_i_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 3153.3119              2998.8437              2830.1333              2679.2147              4637.5906 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 1755.9733              2609.3445              2116.5183              4279.7873               936.4446 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 876.9921               780.4767               468.7506
png("./data/res_F/resFigures/5.turt/cf_turt_i_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_turt_i_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_turt_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()


# remove models
# main effects
# std_res_f_coca_log
lmer_turt_f <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_f <- r.squaredGLMM(lmer_turt_f)
# reglen_C
lmer_turt_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_reglen <- r.squaredGLMM(lmer_turt_reglen)
# std_res_prevf_coca_log
lmer_turt_prevf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                           + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                           + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                           + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                           + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                           + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_prevf <- r.squaredGLMM(lmer_turt_prevf)
# std_res_nextf_coca_log
lmer_turt_nextf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_nextf <- r.squaredGLMM(lmer_turt_nextf)

# Interactions
# sspan.corr.st:region_C*
lmer_turt_sspan_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_sspan_region <- r.squaredGLMM(lmer_turt_sspan_region)
# decod.comp_bct:region_C*
lmer_turt_decod_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                                  + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                                  + oral.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                                  + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                                  + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                                  + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_decod_region <- r.squaredGLMM(lmer_turt_decod_region)
# gort.wpm.st:std_res_f_coca_log*
lmer_turt_gort_f <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_gort_f <- r.squaredGLMM(lmer_turt_gort_f)
# decod.comp_bct:reglen_C
lmer_turt_decod_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                               + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_decod_reglen <- r.squaredGLMM(lmer_turt_decod_reglen)
# gort.wpm.st:reglen_C
lmer_turt_gort_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_gort_reglen <- r.squaredGLMM(lmer_turt_gort_reglen)
# sspan.corr.st:reglen_C
lmer_turt_sspan_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D1)
Rs_turt_sspan_reglen <- r.squaredGLMM(lmer_turt_sspan_reglen)

Rs1 <- Rs_turt["R2m"] 
Rs2 <- c(Rs_turt_f["R2m"], Rs_turt_reglen["R2m"], Rs_turt_prevf["R2m"], Rs_turt_nextf["R2m"],
         Rs_turt_sspan_region["R2m"], Rs_turt_decod_region["R2m"], Rs_turt_gort_f["R2m"], Rs_turt_decod_reglen["R2m"], Rs_turt_gort_reglen["R2m"], Rs_turt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0111396306 0.0267971157 0.0035694385 0.0035003044
# 0.0003714573* 0.0001388969* 0.0004745268* 0.0007422874 0.0007184776 0.0008135905

# draw significant main effects:
# std_res_f_coca_log
Draw_MainEffect("linear", D1, f, turt, c(250,600), 0.75, "Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_f1.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D1, reglen, turt, c(250,600), 0.75, "Word Length", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_reglen1.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D1, prevf, turt, c(250,600), 0.75, "Previous Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_prevf1.png", 6, 6)
# std_res_nextf_coca_log
Draw_MainEffect("linear", D1, nextf, turt, c(250,600), 0.75, "Next Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_nextf1.png", 6, 6)

# draw significant interactions:
# sspan.corr.st:region_C*
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D1, region, turt, sspan, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Verbal Working Memory x Word Position", 
         "./data/res_F/resFigures/5.turt/Int_turt_sspan_region1.png", 24, 6)
# decod.comp_bct:region_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, region, turt, decod, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Decoding x Word Position", 
         "./data/res_F/resFigures/5.turt/Int_turt_decod_region1.png", 24, 6)
# gort.wpm.st:std_res_f_coca_log*
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D1, f, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Frequency", "Total Reading Time (ms)", "Oral Reading Fluency x Word Frequency", 
         "./data/res_F/resFigures/5.turt/Int_turt_gort_f1.png", 24, 6)
# decod.comp_bct:reglen_C
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D1, reglen, turt, decod, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/5.turt/Int_turt_decod_reglen1.png", 24, 6)
# gort.wpm.st:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D1, reglen, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/res_F/resFigures/5.turt/Int_turt_gort_reglen1.png", 24, 6)



# D2 (with stopwords)
lmer_turt_c <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1|subj), data = D2)

# model selection
lmer_turt0 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                   + (1|subj), data = D2)

lmer_turt1 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                   + (1|word) + (1|subj), data = D2)

lmer_turt2 <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                   + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                   + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                   + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                   + (1|itemF) + (1|word) + (1|subj), data = D2)

anova(lmer_turt_c, lmer_turt0)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_turt0  34 304685 304958 -152308   304617                             
# lmer_turt_c 36 303853 304142 -151891   303781 835.63      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

anova(lmer_turt_c, lmer_turt1)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer_turt1  35 303863 304144 -151896   303793                             
# lmer_turt_c 36 303853 304142 -151891   303781 11.712      1  0.0006209 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

anova(lmer_turt_c, lmer_turt2)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmer_turt_c 36 303853 304142 -151891   303781                             
# lmer_turt2  36 303845 304134 -151887   303773 7.5971      0  < 2.2e-16 ***

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

# calculate delta-R squared
Rs_turt <- r.squaredGLMM(lmer_turt_c)
# R2m        R2c 
# 0.09789088 0.23137486

# calculate condition number kappa and variation inflation
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer(lmer_turt_c)
# 5.601807

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
max(vif.mer(lmer_turt_c))
# 4.330166

# run the party implementation
library(party)
cf_turt_c <- cforest(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     , data=D2,control=cforest_unbiased(mtry=2,ntree=50))
cf_turt_c_varimp <- varimp(cf_turt_c)
cf_turt_c_varimp
# oral.comp_bct         decod.comp_bct      readcomp.comp_bct      printexp.comp_bct            gort.wpm.st 
# 3313.6456              2511.3179              2635.3951              2166.1648              4257.2276 
# sspan.corr.st               region_C     std_res_f_coca_log               reglen_C std_res_prevf_coca_log 
# 1992.0428              1961.0804              1515.3030              4731.9722               464.6599 
# prevwlen_C std_res_nextf_coca_log             nextwlen_C 
# 671.9200               489.0663               250.2747 
png("./data/res_F/resFigures/5.turt/cf_turt_c_varimp.png", width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_turt_c_varimp), panel=function (x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_turt_c_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()


# remove models
# main effects
# std_res_f_coca_log
lmer_turt_f <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_f <- r.squaredGLMM(lmer_turt_f)
# reglen
lmer_turt_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_res_f_coca_log + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_reglen <- r.squaredGLMM(lmer_turt_reglen)
# std_res_prevf_coca_log
lmer_turt_prevf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_prevf <- r.squaredGLMM(lmer_turt_prevf)
# std_res_nextf_coca_log
lmer_turt_nextf <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_nextf <- r.squaredGLMM(lmer_turt_nextf)

# Interactions
# gort.wpm.st:region_C*
lmer_turt_gort_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_gort_region <- r.squaredGLMM(lmer_turt_gort_region)
# sspan.corr.st:region_C
lmer_turt_sspan_region <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C 
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_sspan_region <- r.squaredGLMM(lmer_turt_sspan_region)
# decod.comp_bct:reglen_C
lmer_turt_decod_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_decod_reglen <- r.squaredGLMM(lmer_turt_decod_reglen)
# gort.wpm.st:reglen_C
lmer_turt_gort_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                              + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                              + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                              + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                              + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + sspan.corr.st:reglen_C
                              + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_gort_reglen <- r.squaredGLMM(lmer_turt_gort_reglen)
# sspan.corr.st:reglen_C
lmer_turt_sspan_reglen <- lmer(turt ~ oral.comp_bct + decod.comp_bct + readcomp.comp_bct + printexp.comp_bct + gort.wpm.st + sspan.corr.st 
                               + region_C + std_res_f_coca_log + reglen_C + std_res_prevf_coca_log + prevwlen_C + std_res_nextf_coca_log + nextwlen_C
                               + oral.comp_bct:region_C + decod.comp_bct:region_C + readcomp.comp_bct:region_C + printexp.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                               + oral.comp_bct:std_res_f_coca_log + decod.comp_bct:std_res_f_coca_log + readcomp.comp_bct:std_res_f_coca_log + printexp.comp_bct:std_res_f_coca_log + gort.wpm.st:std_res_f_coca_log + sspan.corr.st:std_res_f_coca_log
                               + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + readcomp.comp_bct:reglen_C + printexp.comp_bct:reglen_C + gort.wpm.st:reglen_C 
                               + (1|itemF/word) + (1|subj), data = D2)
Rs_turt_sspan_reglen <- r.squaredGLMM(lmer_turt_sspan_reglen)

Rs1 <- Rs_turt["R2m"] 
Rs2 <- c(Rs_turt_f["R2m"], Rs_turt_reglen["R2m"], Rs_turt_prevf["R2m"], Rs_turt_nextf["R2m"], 
         Rs_turt_gort_region["R2m"], Rs_turt_sspan_region["R2m"], Rs_turt_decod_reglen["R2m"], Rs_turt_gort_reglen["R2m"], Rs_turt_sspan_reglen["R2m"])
calDeltaR(Rs1, Rs2)
# 0.0061321137 0.0379073685 0.0028022617 0.0036498940
# 0.0003118011* 0.0005246443 0.0006058479 0.0008857115 0.0008540415 

# draw significant main effects:
# std_res_f_coca_log
Draw_MainEffect("linear", D2, f, turt, c(250,600), 0.75, "Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_f2.png", 6, 6)
# reglen_C
Draw_MainEffect("linear", D2, reglen, turt, c(250,600), 0.75, "Word Length", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_reglen2.png", 6, 6)
# std_res_prevf_coca_log
Draw_MainEffect("linear", D2, prevf, turt, c(250,600), 0.75, "Previous Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_prevf2.png", 6, 6)
# std_Res_nextf_coca_log*
Draw_MainEffect("linear", D2, nextf, turt, c(250,600), 0.75, "Next Word Frequency", "Total Reading Time (ms)", 
                "./data/res_F/resFigures/5.turt/Main_turt_nextf2.png", 6, 6)

# draw significant interactions:
# significant interactions: gort.wpm.st:region_C*, sspan.corr.st:region_C, decod.comp_bct:reglen_C, gort.wpm.st:reglen_C, sspan.corr.st:reglen_C
# gort.wpm.st:region_C*
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, region, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Oral Reading Fluency x Word Position", 
         "./data/res_F/resFigures/5.turt/Int_turt_gort_region2.png", 24, 6)
# sspan.corr.st:region_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, region, turt, sspan, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Position", "Total Reading Time (ms)", "Verbal Working Memory x Word Position", 
         "./data/res_F/resFigures/5.turt/Int_turt_sspan_region2.png", 24, 6)
# decod.comp_bct:reglen_C*
LabQuartile <- c("Low Decod.", "", "", "High Decod.")
Draw_Int("linear", "number", 0, D2, reglen, turt, decod, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Decoding x Word Length", 
         "./data/res_F/resFigures/5.turt/Int_turt_decod_reglen2.png", 24, 6)
# gort.wpm.st:reglen_C
LabQuartile <- c("Low Oral Fluen.", "", "", "High Oral Fluen.")
Draw_Int("linear", "number", 0, D2, reglen, turt, gort, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Oral Reading Fluency x Word Length", 
         "./data/res_F/resFigures/5.turt/Int_turt_gort_reglen2.png", 24, 6)
# sspan.corr.st:reglen_C
LabQuartile <- c("Low Verbal Mem.", "", "", "High Verbal Mem.")
Draw_Int("linear", "number", 0, D2, reglen, turt, sspan, numQuartile, LabQuartile, c(250,600), 0.85, 0,
         "Word Length", "Total Reading Time (ms)", "Verbal Working Memory x Word Length", 
         "./data/res_F/resFigures/5.turt/Int_turt_sspan_reglen2.png", 24, 6)


# contribution of decoding and oral knowledge to reading comprehension
a <- lm(readcomp.comp_bct ~ decod.comp_bct + oral.comp_bct, data=D2)
summary(a)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.48649 -0.31548 -0.01405  0.42244  1.00256 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.009340   0.003354  -2.785  0.00536 ** 
#   decod.comp_bct  0.188090   0.004047  46.481  < 2e-16 ***
#   oral.comp_bct   0.756480   0.004116 183.772  < 2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.5165 on 23749 degrees of freedom
# Multiple R-squared:  0.7319,	Adjusted R-squared:  0.7318 
# F-statistic: 3.241e+04 on 2 and 23749 DF,  p-value: < 2.2e-16

a1 <- lm(readcomp.comp_bct ~ oral.comp_bct, data=D2)
summary(a1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.26856 -0.33750  0.02251  0.41974  0.94038 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.010018   0.003503   -2.86  0.00424 ** 
#   oral.comp_bct  0.861463   0.003594  239.66  < 2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.5395 on 23750 degrees of freedom
# Multiple R-squared:  0.7075,	Adjusted R-squared:  0.7075 
# F-statistic: 5.744e+04 on 1 and 23750 DF,  p-value: < 2.2e-16

a2 <- lm(readcomp.comp_bct ~ decod.comp_bct, data=D2)
summary(a2)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5999 -0.4120  0.1779  0.5083  1.4060 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.026995   0.005217  -5.174 2.31e-07 ***
#   decod.comp_bct  0.596127   0.005265 113.225  < 2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.8039 on 23750 degrees of freedom
# Multiple R-squared:  0.3506,	Adjusted R-squared:  0.3505 
# F-statistic: 1.282e+04 on 1 and 23750 DF,  p-value: < 2.2e-16