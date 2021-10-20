require(stats)
require(lme4)
require(lmerTest)
require(EMAtools)
require(party)
require(lattice)
require(MuMIn)
require(boot)

# install.packages("devtools")
# devtools::install_github("neuropsychology/report")
require(report)

source("./drawMainEffect_Int.r")
source("./mer.r")

# load data
D1 <- read.csv("./data/res_F/ET_F_ana1.csv", na.strings = "NA") # without stopwords
D1$itemF <- factor(D1$item) # probably won't matter if item is treated as numeric or categorical, but
# add std_f_coca_log, not residualized word frequency
D1$std_f_coca_log <- scale(D1$f_coca_log)
D1$std_prevf_coca_log <- scale(D1$prevf_coca_log); D1$std_nextf_coca_log <- scale(D1$nextf_coca_log)
write.csv(D1, "./data/res_F/ET_F_ana1.csv", row.names=FALSE) # resave D1

D2 <- read.csv("./data/res_F/ET_F_ana2.csv", na.strings = "NA") # with stopwords
D2$itemF <- factor(D2$item) # probably won't matter if item is treated as numeric or categorical, but
# add std_f_coca_log, not residualized word frequency
D2$std_f_coca_log <- scale(D2$f_coca_log)
D2$std_prevf_coca_log <- scale(D2$prevf_coca_log); D2$std_nextf_coca_log <- scale(D2$nextf_coca_log)
write.csv(D2, "./data/res_F/ET_F_ana2.csv", row.names=FALSE) # resave D2

### Analyses on ffixurt, fpurt, fpregres and fpregres_out, and optional (tffixurt) 
### simultaneous regression analysis: EM measures ~ Lexical Properties + Individual Skills + Lexical Properties:Individual Skills

# symbols for different columns
numQuartile <- 2
# dependent variables (EM measures)
ffixurt <- "ffixurt"; fpurt <- "fpurt"; fpregres <- "fpregres"; fpregres_in <- "fpregres_in"; turt <- "turt"
# independent variables (lexical properties)
f <- "std_f_coca_log"; prevf <- "std_prevf_coca_log"; nextf <- "std_nextf_coca_log"
region <- "region_C"; reglen <- "reglen_C"; prevwlen <- "prevwlen_C"; nextwlen <- "nextwlen_C"
# independent variables (individual skill measures)
# composite scores (note that all these using Box Cox transformed measures)
oral <- "oral.comp_bct"; decod <- "decod.comp_bct"; readcomp <- "readcomp.comp_bct"; printexp <- "printexp.comp_bct"
# individual scores (note that listening comprehension (piat) uses Box Cox transformed measure)
gort <- "gort.wpm.st"; sspan <- "sspan.corr.st" 


dir.create(file.path('.', 'results'), showWarnings = FALSE)

### ffixurt
dir.create(file.path('.', 'results', 'ffixurt'), showWarnings = FALSE)
resdir <- file.path('.', 'results', 'ffixurt')

# D1 (without stopwords)
# 1) with only skill measures
lmer_ffixurt_i_skill <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                             + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_ffixurt_i_skill)
# REML criterion at convergence: 184101.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9378 -0.6074 -0.1643  0.3800  8.0102 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     453.6   21.299        
# itemF      (Intercept)       0.0    0.000        
# subj       (Intercept)     893.1   29.884        
# std_f_coca_log   41.8    6.465   -0.85
# Residual                  7975.5   89.306        
# Number of obs: 15520, groups:  word:itemF, 416; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)     229.443      4.039  54.020  56.811   <2e-16 ***
#   oral.comp_bct    -5.671      4.737  38.327  -1.197   0.2386    
# decod.comp_bct    2.948      4.538  38.495   0.650   0.5197    
# gort.wpm.st      -8.095      4.240  38.293  -1.909   0.0638 .  
# sspan.corr.st     4.569      4.669  38.943   0.978   0.3339    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_ffixurt_i_skill, D1, 'lme4')
# t       df          d
# oral.comp_bct  -1.1971133 38.32659 -0.3867365
# decod.comp_bct  0.6496979 38.49503  0.2094301
# gort.wpm.st    -1.9090896 38.29267 -0.6170189
# sspan.corr.st   0.9783846 38.94293  0.3135633

# get Kappa and VIF
kappa.mer(lmer_ffixurt_i_skill)
# 2.820187
vif.mer(lmer_ffixurt_i_skill)
# oral.comp_bct decod.comp_bct    gort.wpm.st  sspan.corr.st 
# 1.929550       1.779704       1.551818       1.874229 


# 2) mode without interaction, without print experience, using original frequency (not residualized frequency)
lmer_ffixurt_i_skillword <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                                 + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                                 + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_ffixurt_i_skillword)
# REML criterion at convergence: 176217.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9768 -0.6082 -0.1622  0.3845  8.1191 
# 
# Random effects:
#   Groups     Name           Variance  Std.Dev.  Corr 
# word:itemF (Intercept)    3.097e+02 17.599498      
# itemF      (Intercept)    2.267e-05  0.004762      
# subj       (Intercept)    7.990e+02 28.267161      
# std_f_coca_log 3.488e+01  5.905678 -0.90
# Residual                  7.875e+03 88.742640      
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        236.4563     4.4137  37.7339  53.574  < 2e-16 ***
#   oral.comp_bct       -6.2891     4.4400  38.4407  -1.416 0.164692    
# decod.comp_bct       2.3255     4.2525  38.5918   0.547 0.587622    
# gort.wpm.st         -7.7912     3.9746  38.4402  -1.960 0.057244 .  
# sspan.corr.st        5.7773     4.3787  39.1321   1.319 0.194701    
# region_C             2.4644     0.3575 390.8486   6.893 2.20e-11 ***
#   std_f_coca_log      -8.4308     1.5990 131.8004  -5.273 5.36e-07 ***
#   reglen_C            -2.5964     0.6641 380.9623  -3.910 0.000109 ***
#   std_prevf_coca_log  -5.2334     1.7981 383.7521  -2.911 0.003818 ** 
#   prevwlen_C           1.0514     0.8731 389.0611   1.204 0.229200    
# std_nextf_coca_log  -3.0215     1.7572 381.0059  -1.719 0.086337 .  
# nextwlen_C          -0.5198     0.7994 382.7192  -0.650 0.515909    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_ffixurt_i_skillword, D1, 'lme4')
# t        df           d
# oral.comp_bct      -1.4164652  38.44066 -0.45692032
# decod.comp_bct      0.5468616  38.59177  0.17605975
# gort.wpm.st        -1.9602500  38.44021 -0.63233701
# sspan.corr.st       1.3194082  39.13209  0.42183506
# region_C            6.8928620 390.84862  0.69730903
# std_f_coca_log     -5.2725035 131.80044 -0.91851962
# reglen_C           -3.9096956 380.96235 -0.40061934
# std_prevf_coca_log -2.9105483 383.75206 -0.29715254
# prevwlen_C          1.2043140 389.06109  0.12211270
# std_nextf_coca_log -1.7194929 381.00586 -0.17618322
# nextwlen_C         -0.6502678 382.71922 -0.06647863

# get Kappa and VIF
kappa.mer(lmer_ffixurt_i_skillword)
# 3.015076
vif.mer(lmer_ffixurt_i_skillword)
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1.926694           1.777995           1.552319           1.873449           1.046562           1.223121           1.256052 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 2.485754           2.505670           2.357942           2.362345 


# 3) with skill, word, and interaction
lmer_ffixurt_i <- lmer(ffixurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                       + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                       + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                       + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                       + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                       + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_ffixurt_i)
# REML criterion at convergence: 176179.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.0247 -0.6080 -0.1599  0.3812  8.1027 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     302.748 17.400        
# itemF      (Intercept)       6.619  2.573        
# subj       (Intercept)     716.328 26.764        
# std_f_coca_log   21.959  4.686   -0.92
# Residual                  7871.164 88.720        
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                    2.365e+02  4.205e+00  4.285e+01  56.246  < 2e-16 ***
#   oral.comp_bct                 -6.784e+00  5.787e+00  3.889e+01  -1.172 0.248252    
# decod.comp_bct                -8.716e+00  5.537e+00  3.889e+01  -1.574 0.123575    
# gort.wpm.st                   -8.738e+00  5.181e+00  3.891e+01  -1.687 0.099696 .  
# sspan.corr.st                  7.987e+00  5.683e+00  3.895e+01   1.405 0.167825    
# region_C                       2.502e+00  3.565e-01  3.775e+02   7.016 1.06e-11 ***
#   std_f_coca_log                -8.384e+00  1.505e+00  1.427e+02  -5.572 1.21e-07 ***
#   reglen_C                      -2.599e+00  6.642e-01  3.806e+02  -3.913 0.000108 ***
#   std_prevf_coca_log            -5.348e+00  1.796e+00  3.825e+02  -2.978 0.003083 ** 
#   prevwlen_C                     9.985e-01  8.728e-01  3.890e+02   1.144 0.253309    
# std_nextf_coca_log            -3.029e+00  1.756e+00  3.811e+02  -1.725 0.085312 .  
# nextwlen_C                    -5.464e-01  7.984e-01  3.817e+02  -0.684 0.494106    
# oral.comp_bct:region_C         1.858e-01  3.168e-01  1.445e+04   0.587 0.557411    
# decod.comp_bct:region_C        7.359e-01  3.023e-01  1.444e+04   2.435 0.014914 *  
#   gort.wpm.st:region_C           2.689e-01  2.827e-01  1.446e+04   0.951 0.341499    
# sspan.corr.st:region_C        -3.671e-01  3.131e-01  1.445e+04  -1.172 0.241079    
# oral.comp_bct:std_f_coca_log   2.517e-01  1.545e+00  5.241e+01   0.163 0.871204    
# decod.comp_bct:std_f_coca_log  3.995e+00  1.480e+00  5.267e+01   2.699 0.009315 ** 
#   gort.wpm.st:std_f_coca_log     3.530e-01  1.384e+00  5.263e+01   0.255 0.799679    
# sspan.corr.st:std_f_coca_log  -3.363e-01  1.524e+00  5.348e+01  -0.221 0.826237    
# oral.comp_bct:reglen_C         5.485e-02  5.885e-01  1.444e+04   0.093 0.925747    
# decod.comp_bct:reglen_C       -2.270e-01  5.628e-01  1.444e+04  -0.403 0.686737    
# gort.wpm.st:reglen_C          -1.047e-02  5.279e-01  1.447e+04  -0.020 0.984174    
# sspan.corr.st:reglen_C         4.805e-01  5.783e-01  1.445e+04   0.831 0.406130    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# critical p value is 0.05/23 = 0.002173913
# significant main effects: region_C, std_f_coca_log, reglen_C, std_prevf_coca_log (marginal)
# significant interactions: decod.comp_bct:std_f_coca_log (marginal)

# get effect size
lme.dscore(lmer_ffixurt_i, D1, 'lme4')
# t          df             d
# oral.comp_bct                 -1.17218968    38.89074 -0.3759281118
# decod.comp_bct                -1.57405577    38.89163 -0.5048031357
# gort.wpm.st                   -1.68653685    38.90965 -0.5407508086
# sspan.corr.st                  1.40541240    38.95005  0.4503807133
# region_C                       7.01618178   377.46225  0.7222608328
# std_f_coca_log                -5.57218716   142.74568 -0.9327692192
# reglen_C                      -3.91293865   380.62379 -0.4011299323
# std_prevf_coca_log            -2.97837500   382.47232 -0.3045856005
# prevwlen_C                     1.14404346   389.00770  0.1160094638
# std_nextf_coca_log            -1.72514749   381.10141 -0.1767404481
# nextwlen_C                    -0.68444988   381.69378 -0.0700670939
# oral.comp_bct:region_C         0.58670553 14453.89190  0.0097601788
# decod.comp_bct:region_C        2.43474699 14443.17754  0.0405184159
# gort.wpm.st:region_C           0.95123914 14455.66201  0.0158234332
# sspan.corr.st:region_C        -1.17234248 14454.30428 -0.0195023030
# oral.comp_bct:std_f_coca_log   0.16292447    52.41004  0.0450100051
# decod.comp_bct:std_f_coca_log  2.69945185    52.67256  0.7438979481
# gort.wpm.st:std_f_coca_log     0.25504914    52.62712  0.0703151706
# sspan.corr.st:std_f_coca_log  -0.22060873    53.47750 -0.0603346955
# oral.comp_bct:reglen_C         0.09319842 14444.41582  0.0015509170
# decod.comp_bct:reglen_C       -0.40329510 14436.48244 -0.0067130866
# gort.wpm.st:reglen_C          -0.01983721 14467.35071 -0.0003298497
# sspan.corr.st:reglen_C         0.83074673 14449.38395  0.0138220980

kappa.mer(lmer_ffixurt_i)
# 5.742707
vif.mer(lmer_ffixurt_i)
# oral.comp_bct                decod.comp_bct                   gort.wpm.st                 sspan.corr.st 
# 3.212360                      2.957708                      2.588022                      3.096916 
# region_C                std_f_coca_log                      reglen_C            std_prevf_coca_log 
# 1.046848                      1.262675                      1.295094                      2.493733 
# prevwlen_C            std_nextf_coca_log                    nextwlen_C        oral.comp_bct:region_C 
# 2.511125                      2.361501                      2.360780                      1.955097 
# decod.comp_bct:region_C          gort.wpm.st:region_C        sspan.corr.st:region_C  oral.comp_bct:std_f_coca_log 
# 1.800713                      1.576176                      1.901275                      3.749153 
# decod.comp_bct:std_f_coca_log    gort.wpm.st:std_f_coca_log  sspan.corr.st:std_f_coca_log        oral.comp_bct:reglen_C 
# 3.467662                      3.037023                      3.650673                      2.494893 
# decod.comp_bct:reglen_C          gort.wpm.st:reglen_C        sspan.corr.st:reglen_C 
# 2.328616                      2.043349                      2.440570 

# run the party implementation
cf_ffixurt_i <- cforest(ffixurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         , data=D1, control=cforest_unbiased(mtry=2,ntree=50))
cf_ffixurt_i_varimp <- varimp(cf_ffixurt_i)
cf_ffixurt_i_varimp
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 744.73572          786.41731          657.75084          557.40989          252.62941          178.29689           83.88295 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 155.69750          137.59346           73.11209           40.17246  
png(file.path(resdir, "cf_ffixurt_i_varimp.png"), width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_ffixurt_i_varimp), panel=function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_ffixurt_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

r.squaredGLMM(lmer_ffixurt_i_skill)
# R2m      R2c
# 0.007872213 0.154986
r.squaredGLMM(lmer_ffixurt_i_skillword)
# R2m      R2c
# 0.02591285 0.150156
r.squaredGLMM(lmer_ffixurt_i)
# R2m       R2c
# 0.04670803 0.1592385

# anova model selection
anova(lmer_ffixurt_i, lmer_ffixurt_i_skillword)
# npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)   
# lmer_ffixurt_i_skillword   18 176286 176423 -88125   176250                        
# lmer_ffixurt_i             30 176280 176508 -88110   176220 30.352 12   0.002471 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# get bootstrapping results
r3 <- report(lmer_ffixurt_i, bootstrap=TRUE, iterations = 500)
table_long(r3)
# Parameter          | Coefficient |               CI |      p |      Fit
# -----------------------------------------------------------------------
#   (Intercept)        |      236.70 | [229.06, 244.90] | < .001 |         
#   oral.comp_bct      |       -6.59 | [-17.84,   4.39] | 0.224  |         
#   decod.comp_bct     |       -8.22 | [-19.60,   1.81] | 0.112  |         
#   gort.wpm.st        |       -8.73 | [-19.17,   2.56] | 0.128  |         
#   sspan.corr.st      |        7.47 | [ -3.58,  18.89] | 0.160  |         
#   region_C           |        2.52 | [  1.78,   3.14] | < .001 |         
#   std_f_coca_log     |       -8.44 | [-11.23,  -5.13] | < .001 |         
#   reglen_C           |       -2.66 | [ -3.92,  -1.40] | < .001 |         
#   std_prevf_coca_log |       -5.44 | [ -9.29,  -2.06] | < .001 |         
#   prevwlen_C         |        0.93 | [ -0.82,   2.50] | 0.279  |         
#   std_nextf_coca_log |       -2.98 | [ -6.51,   0.46] | 0.120  |         
#   nextwlen_C         |       -0.44 | [ -2.14,   1.21] | 0.571  |         
#   |        0.19 | [ -0.48,   0.85] | 0.575  |         
#   |        0.72 | [  0.10,   1.30] | 0.032  |         
#   |        0.30 | [ -0.25,   0.84] | 0.319  |         
#   |       -0.35 | [ -1.07,   0.25] | 0.228  |         
#   |        0.21 | [ -2.77,   3.35] | 0.866  |         
#   |        4.11 | [  0.79,   6.84] | 0.008  |         
#   |        0.35 | [ -2.17,   3.22] | 0.830  |         
#   |       -0.23 | [ -3.19,   2.52] | 0.874  |         
#   |        0.07 | [ -1.14,   1.04] | 0.902  |         
#   |       -0.21 | [ -1.30,   0.84] | 0.719  |         
#   |       -0.01 | [ -1.09,   0.93] | 0.970  |         
#   |        0.50 | [ -0.58,   1.67] | 0.447  |         
#   |             |                  |        |         
#   AIC                |             |                  |        | 1.76e+05
# BIC                |             |                  |        | 1.76e+05
# R2 (conditional)   |             |                  |        |     0.16
# R2 (marginal)      |             |                  |        |     0.05
# ICC                |             |                  |        |     0.12
# RMSE               |             |                  |        |    87.83

# using boot for bootstrap
confint(lmer_ffixurt_i, method="boot", nsim=500, boot.type="perc")
# 2.5 %      97.5 %
#   .sig01                         15.15979965  19.3514590
# .sig02                          0.00000000   6.6866673
# .sig03                         21.03464228  33.1901983
# .sig04                         -1.00000000  -0.6624267
# .sig05                          2.52974323   6.7072005
# .sigma                         87.76259615  89.7864409
# (Intercept)                   228.35940218 245.0891688
# oral.comp_bct                 -20.38796013   4.2126073
# decod.comp_bct                -20.19608180   2.0015221
# gort.wpm.st                   -19.15840248   1.3988303
# sspan.corr.st                  -4.24193482  19.4758687
# region_C                        1.79943204   3.3219711
# std_f_coca_log                -11.04861998  -5.7576094
# reglen_C                       -3.84683402  -1.1550965
# std_prevf_coca_log             -8.73117190  -1.6376873
# prevwlen_C                     -0.67917670   2.7024435
# std_nextf_coca_log             -6.22402485   0.3857555
# nextwlen_C                     -2.04355651   0.8853431
# oral.comp_bct:region_C         -0.43540997   0.8951605
# decod.comp_bct:region_C         0.04524333   1.3352665
# gort.wpm.st:region_C           -0.28842182   0.9120315
# sspan.corr.st:region_C         -0.99475223   0.1644418
# oral.comp_bct:std_f_coca_log   -3.31795508   3.1026362
# decod.comp_bct:std_f_coca_log   1.04132135   7.0354793
# gort.wpm.st:std_f_coca_log     -2.19196943   3.1088259
# sspan.corr.st:std_f_coca_log   -3.32964664   2.6932695
# oral.comp_bct:reglen_C         -1.17892347   1.1875725
# decod.comp_bct:reglen_C        -1.32591580   0.8696271
# gort.wpm.st:reglen_C           -1.05278162   1.0057025
# sspan.corr.st:reglen_C         -0.65807290   1.7470333

# using lmeresampler package's bootstrap function
mySumm <- function(.) {
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}
boo1 <- bootstrap(model = lmer_ffixurt_i, fn = mySumm, type = "parametric", B = 500)
requireNamespace("boot")
boo1
# Bootstrap Statistics :
#   original       bias    std. error
# t1*  236.49907370  0.049246406   4.1198554
# t2*   -6.78402602 -0.391061665   6.1862486
# t3*   -8.71613749  0.192699586   5.7395929
# t4*   -8.73832565 -0.040215144   5.1326062
# t5*    7.98711317  0.284016832   5.6694456
# t6*    2.50150771  0.005810943   0.3790583
# t7*   -8.38353807  0.002739739   1.5484845
# t8*   -2.59897883 -0.015616944   0.6409522
# t9*   -5.34833531  0.049399946   1.7819865
# t10*   0.99846401  0.043511787   0.8571199
# t11*  -3.02917625  0.035943399   1.6647506
# t12*  -0.54642846  0.049993012   0.7692524
# t13*   0.18585372 -0.025714471   0.3127024
# t14*   0.73592215 -0.006584462   0.3165749
# t15*   0.26890389 -0.010091422   0.2800444
# t16*  -0.36708833  0.024870368   0.3297380
# t17*   0.25167485  0.003416853   1.6478700
# t18*   3.99539593  0.065346994   1.4953944
# t19*   0.35303935 -0.006753800   1.4025860
# t20*  -0.33625800  0.030122264   1.5515038
# t21*   0.05484668  0.010407749   0.5803230
# t22*  -0.22699585 -0.013726040   0.5453216
# t23*  -0.01047242  0.025359402   0.5238110
# t24*   0.48045782  0.014813922   0.5522526
# t25*  88.71957840  0.022170527   0.5177395
# t26*  17.39966464 -0.114843436   1.1082627
# t27*   2.57267824 -0.317328659   2.1885237
# t28*  26.76430665 -0.325610640   3.0584433
# t29*  -4.31482595  0.064477344   0.9697547
# t30*   1.82786325 -0.411807880   1.3339928

plot(boo1, index=1)

# draw significant main effects:
yRange <- c(150, 300)
yrate <- 0.35
# region_C
X <- region; xlab <- 'region'; Y <- ffixurt; ylab <- ffixurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 2)

# std_f_coca_log
X <- f; xlab <- f; Y <- ffixurt; ylab <- ffixurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# reglen_C
X <- reglen; xlab <- reglen; Y <- ffixurt; ylab <- ffixurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_prevf_coca_log (marginal)
X <- prevf; xlab <- prevf; Y <- ffixurt; ylab <- ffixurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)


# draw significant interactions:
# decod.comp_bct:std_f_coca_log (marginal)
yrate <- 0.35
LabQuartile <- c("Low Decod.", "High Decod.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, f, ffixurt, decod, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word frequency", "First Fixation Duration (ms)", "Decoding x Word Frequency", 
         file.path(resdir, "Int_ffixurt_decod_f"))



### fpurt
dir.create(file.path('.', 'results', 'fpurt'), showWarnings = FALSE)
resdir <- file.path('.', 'results', 'fpurt')

# D1 (without stopwords)
# 1) with only skill measures
lmer_fpurt_i_skill <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                           + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_fpurt_i_skill)
# REML criterion at convergence: 195606.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7587 -0.6210 -0.1854  0.4469  5.7645 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     1382.4   37.180       
# itemF      (Intercept)       61.5    7.843       
# subj       (Intercept)     3064.3   55.356       
# std_f_coca_log   753.7   27.454  -0.90
# Residual                  16534.7  128.587       
# Number of obs: 15520, groups:  word:itemF, 416; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)     254.394      5.431  65.571  46.840   <2e-16 ***
#   oral.comp_bct    -9.300      5.851  38.328  -1.590   0.1202    
# decod.comp_bct   -1.857      5.601  38.424  -0.331   0.7421    
# gort.wpm.st     -10.816      5.238  38.363  -2.065   0.0458 *  
#   sspan.corr.st     7.088      5.762  38.821   1.230   0.2260    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_fpurt_i_skill, D1, 'lme4')
# t       df          d
# oral.comp_bct  -1.5895583 38.32762 -0.5135119
# decod.comp_bct -0.3314972 38.42373 -0.1069572
# gort.wpm.st    -2.0647297 38.36310 -0.6667091
# sspan.corr.st   1.2302375 38.82095  0.3948989

# get Kappa and VIF
kappa.mer(lmer_fpurt_i_skill)
# 2.820187
vif.mer(lmer_fpurt_i_skill)
# oral.comp_bct decod.comp_bct    gort.wpm.st  sspan.corr.st 
# 1.932699       1.780087       1.554402       1.873789 


# 2) mode without interaction, without print experience, using original frequency (not residualized frequency)
lmer_fpurt_i_skillword <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                               + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                               + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_fpurt_i_skillword)
# REML criterion at convergence: 187187.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8141 -0.6264 -0.1803  0.4469  5.7921 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     1030.5   32.101       
# itemF      (Intercept)       44.0    6.633       
# subj       (Intercept)     1698.1   41.208       
# std_f_coca_log   273.5   16.539  -0.84
# Residual                  16287.4  127.622       
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        291.2430     6.5538  34.6934  44.439  < 2e-16 ***
#   oral.comp_bct       -8.8582     5.7763  38.3842  -1.534 0.133344    
# decod.comp_bct      -2.2435     5.5293  38.4578  -0.406 0.687183    
# gort.wpm.st        -10.4891     5.1725  38.4395  -2.028 0.049549 *  
#   sspan.corr.st        7.6194     5.6897  38.9044   1.339 0.188291    
# region_C             1.9108     0.6042 375.8506   3.163 0.001691 ** 
#   std_f_coca_log     -17.6090     3.3685  97.5031  -5.228 9.78e-07 ***
#   reglen_C             9.0206     1.1321 387.4712   7.968 1.82e-14 ***
#   std_prevf_coca_log  -7.2162     3.0548 386.3823  -2.362 0.018660 *  
#   prevwlen_C          -1.4386     1.4847 393.0911  -0.969 0.333173    
# std_nextf_coca_log -11.3621     2.9903 386.4886  -3.800 0.000168 ***
#   nextwlen_C          -2.3246     1.3583 385.6835  -1.711 0.087808 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_fpurt_i_skillword, D1, 'lme4')
# t        df           d
# oral.comp_bct      -1.5335429  38.38423 -0.49505053
# decod.comp_bct     -0.4057387  38.45782 -0.13085313
# gort.wpm.st        -2.0278407  38.43949 -0.65414648
# sspan.corr.st       1.3391579  38.90438  0.42940051
# region_C            3.1625133 375.85061  0.32625317
# std_f_coca_log     -5.2276170  97.50307 -1.05882603
# reglen_C            7.9681038 387.47124  0.80959020
# std_prevf_coca_log -2.3622377 386.38231 -0.24035047
# prevwlen_C         -0.9689351 393.09110 -0.09774129
# std_nextf_coca_log -3.7995908 386.48862 -0.38654358
# nextwlen_C         -1.7114176 385.68349 -0.17428919

# get Kappa and VIF
kappa.mer(lmer_fpurt_i_skillword)
# 3.015076
vif.mer(lmer_fpurt_i_skillword)
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1.931574           1.778931           1.555161           1.873303           1.045607           1.139651           1.172474 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 2.501880           2.513341           2.370155           2.363253 


# 3) with skill, word, and interaction
lmer_fpurt_i <- lmer(fpurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                   + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                   + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                   + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                   + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                   + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_fpurt_i)
#REML criterion at convergence: 187090.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9696 -0.6219 -0.1810  0.4461  5.8032 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     1036.1   32.189       
# itemF      (Intercept)       42.1    6.489       
# subj       (Intercept)     1292.3   35.949       
# std_f_coca_log   142.8   11.950  -0.78
# Residual                  16237.5  127.426       
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     291.3628     5.8065    46.9923  50.179  < 2e-16 ***
#   oral.comp_bct                   -12.1007     7.7912    38.8543  -1.553 0.128499    
# decod.comp_bct                  -14.9812     7.4545    38.8556  -2.010 0.051445 .  
# gort.wpm.st                     -23.8227     6.9751    38.8757  -3.415 0.001504 ** 
#   sspan.corr.st                    13.8037     7.6511    38.9220   1.804 0.078950 .  
# region_C                          1.9304     0.6050   376.4126   3.191 0.001539 ** 
#   std_f_coca_log                  -17.6550     2.8955   127.0869  -6.097 1.20e-08 ***
#   reglen_C                          8.9534     1.1332   387.5992   7.901 2.89e-14 ***
#   std_prevf_coca_log               -7.1902     3.0578   386.5247  -2.351 0.019205 *  
#   prevwlen_C                       -1.4321     1.4860   393.1321  -0.964 0.335775    
# std_nextf_coca_log              -11.3091     2.9932   386.5816  -3.778 0.000183 ***
#   nextwlen_C                       -2.3222     1.3597   385.8424  -1.708 0.088452 .  
# oral.comp_bct:region_C           -0.4231     0.4552 14438.0762  -0.929 0.352660    
# decod.comp_bct:region_C           0.6278     0.4343 14430.1437   1.446 0.148297    
# gort.wpm.st:region_C              1.0008     0.4062 14439.9348   2.464 0.013758 *  
#   sspan.corr.st:region_C           -0.4094     0.4499 14438.0806  -0.910 0.362913    
# oral.comp_bct:std_f_coca_log     -0.5835     3.0565    45.5972  -0.191 0.849452    
# decod.comp_bct:std_f_coca_log     5.2176     2.9267    45.7341   1.783 0.081259 .  
# gort.wpm.st:std_f_coca_log        6.0735     2.7375    45.6977   2.219 0.031519 *  
#   sspan.corr.st:std_f_coca_log     -0.4726     3.0086    46.1050  -0.157 0.875879    
# oral.comp_bct:reglen_C           -2.5127     0.8455 14432.3109  -2.972 0.002965 ** 
#   decod.comp_bct:reglen_C          -1.8954     0.8086 14426.0798  -2.344 0.019093 *  
#   gort.wpm.st:reglen_C             -1.3473     0.7586 14451.2079  -1.776 0.075731 .  
# sspan.corr.st:reglen_C            2.8896     0.8310 14436.0978   3.477 0.000508 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# critical p value is 0.05/23 = 0.002173913
# significant main effects: gort.wpm.st, region_C, std_f_coca_log, reglen_C, std_nextf_coca_log
# significant interactions: sspan.corr.st:reglen_C, oral.comp_bct:reglen_C (marginal)

# get effect size
lme.dscore(lmer_fpurt_i, D1, 'lme4')
# t          df           d
# oral.comp_bct                 -1.5531357    38.85430 -0.49833325
# decod.comp_bct                -2.0096933    38.85555 -0.64481218
# gort.wpm.st                   -3.4153812    38.87568 -1.09554485
# sspan.corr.st                  1.8041539    38.92201  0.57837022
# region_C                       3.1905156   376.41259  0.32889616
# std_f_coca_log                -6.0974802   127.08686 -1.08175787
# reglen_C                       7.9009061   387.59923  0.80263010
# std_prevf_coca_log            -2.3513854   386.52467 -0.23920222
# prevwlen_C                    -0.9637265   393.13205 -0.09721081
# std_nextf_coca_log            -3.7782705   386.58162 -0.38432836
# nextwlen_C                    -1.7079400   385.84245 -0.17389920
# oral.comp_bct:region_C        -0.9294719 14438.07625 -0.01547076
# decod.comp_bct:region_C        1.4456516 14430.14375  0.02406901
# gort.wpm.st:region_C           2.4638111 14439.93484  0.04100670
# sspan.corr.st:region_C        -0.9098566 14438.08064 -0.01514427
# oral.comp_bct:std_f_coca_log  -0.1908993    45.59723 -0.05654122
# decod.comp_bct:std_f_coca_log  1.7827869    45.73411  0.52724117
# gort.wpm.st:std_f_coca_log     2.2186434    45.69771  0.65640259
# sspan.corr.st:std_f_coca_log  -0.1570652    46.10502 -0.04626323
# oral.comp_bct:reglen_C        -2.9718267 14432.31091 -0.04947497
# decod.comp_bct:reglen_C       -2.3439672 14426.07977 -0.03903079
# gort.wpm.st:reglen_C          -1.7761376 14451.20792 -0.02954980
# sspan.corr.st:reglen_C         3.4774261 14436.09779  0.05788460

kappa.mer(lmer_fpurt_i)
# 5.742707
vif.mer(lmer_fpurt_i)
# oral.comp_bct                decod.comp_bct                   gort.wpm.st                 sspan.corr.st 
# 3.455741                      3.179074                      2.780586                      3.331253 
# region_C                std_f_coca_log                      reglen_C            std_prevf_coca_log 
# 1.046231                      1.199108                      1.232055                      2.502511 
# prevwlen_C            std_nextf_coca_log                    nextwlen_C        oral.comp_bct:region_C 
# 2.515270                      2.369967                      2.363580                      1.954561 
# decod.comp_bct:region_C          gort.wpm.st:region_C        sspan.corr.st:region_C  oral.comp_bct:std_f_coca_log 
# 1.800163                      1.575623                      1.900832                      3.738893 
# decod.comp_bct:std_f_coca_log    gort.wpm.st:std_f_coca_log  sspan.corr.st:std_f_coca_log        oral.comp_bct:reglen_C 
# 3.447046                      3.016198                      3.622384                      2.238389 
# decod.comp_bct:reglen_C          gort.wpm.st:reglen_C        sspan.corr.st:reglen_C 
# 2.086056                      1.824326                      2.186252 

# run the party implementation
cf_fpurt_i <- cforest(fpurt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                        + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                        + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                        + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                        + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                        , data=D1, control=cforest_unbiased(mtry=2,ntree=50))
cf_fpurt_i_varimp <- varimp(cf_fpurt_i)
cf_fpurt_i_varimp
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1383.2566          1456.0694          2110.1518           911.9212           429.9460          1199.8257          1339.0181 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 300.4122           241.5439           338.6450           230.2112
png(file.path(resdir, "cf_fpurt_i_varimp.png"), width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpurt_i_varimp), panel=function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpurt_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

r.squaredGLMM(lmer_fpurt_i_skill)
# R2m       R2c
# 0.0100696 0.2490482
r.squaredGLMM(lmer_fpurt_i_skillword)
# R2m      R2c
# 0.06026447 0.209593
r.squaredGLMM(lmer_fpurt_i)
# R2m       R2c
# 0.1150517 0.2344069

# anova model selection
anova(lmer_fpurt_i, lmer_fpurt_i_skillword)
# npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)   
# lmer_fpurt_i_skillword   18 187267 187404 -93615   187231                         
# lmer_fpurt_i             30 187213 187441 -93576   187153 77.893 12   1.04e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# get bootstrapping results
r3 <- report(lmer_fpurt_i, bootstrap=TRUE, iterations = 500)
table_long(r3)
# Parameter          | Coefficient |               CI |      p |      Fit
# -----------------------------------------------------------------------
#   (Intercept)        |      291.31 | [280.67, 303.13] | < .001 |         
#   oral.comp_bct      |      -12.47 | [-27.95,   1.68] | 0.084  |         
#   decod.comp_bct     |      -14.66 | [-29.62,   0.62] | 0.072  |         
#   gort.wpm.st        |      -23.07 | [-37.12, -10.62] | 0.004  |         
#   sspan.corr.st      |       14.59 | [ -0.71,  30.09] | 0.068  |         
#   region_C           |        1.93 | [  0.86,   3.10] | 0.004  |         
#   std_f_coca_log     |      -17.66 | [-23.26, -12.10] | < .001 |         
#   reglen_C           |        9.02 | [  7.00,  11.24] | < .001 |         
#   std_prevf_coca_log |       -7.23 | [-12.76,  -1.80] | 0.012  |         
#   prevwlen_C         |       -1.48 | [ -4.16,   1.42] | 0.323  |         
#   std_nextf_coca_log |      -11.13 | [-17.09,  -5.56] | < .001 |         
#   nextwlen_C         |       -2.28 | [ -4.95,   0.13] | 0.068  |         
#   |       -0.37 | [ -1.35,   0.52] | 0.391  |         
#   |        0.62 | [ -0.23,   1.49] | 0.152  |         
#   |        0.97 | [  0.13,   1.77] | 0.024  |         
#   |       -0.40 | [ -1.37,   0.57] | 0.327  |         
#   |       -0.42 | [ -6.26,   5.48] | 0.890  |         
#   |        5.21 | [ -0.80,  10.85] | 0.112  |         
#   |        6.03 | [  0.40,  11.17] | 0.036  |         
#   |       -0.59 | [ -6.67,   5.56] | 0.886  |         
#   |       -2.51 | [ -4.18,  -0.85] | < .001 |         
#   |       -1.89 | [ -3.46,  -0.52] | 0.024  |         
#   |       -1.35 | [ -2.83,   0.10] | 0.068  |         
#   |        2.92 | [  1.23,   4.47] | < .001 |         
#   |             |                  |        |         
#   AIC                |             |                  |        | 1.87e+05
# BIC                |             |                  |        | 1.87e+05
# R2 (conditional)   |             |                  |        |     0.23
# R2 (marginal)      |             |                  |        |     0.12
# ICC                |             |                  |        |     0.13
# RMSE               |             |                  |        |   125.88

# using boot for bootstrap
confint(lmer_fpurt_i, method="boot", nsim=500, boot.type="perc")
# 2.5 %       97.5 %
#   .sig01                         28.2602006  35.19929354
# .sig02                          0.0000000  12.34817540
# .sig03                         28.0244991  44.64659249
# .sig04                         -0.9542535  -0.53928554
# .sig05                          8.2848276  15.33809323
# .sigma                        125.9536286 128.74853577
# (Intercept)                   279.9622318 303.70215390
# oral.comp_bct                 -29.5321645   2.78610492
# decod.comp_bct                -26.8728405  -0.05236562
# gort.wpm.st                   -37.3050948 -11.09955642
# sspan.corr.st                  -0.5795296  28.50918085
# region_C                        0.8561262   3.18786656
# std_f_coca_log                -23.7622657 -12.14672425
# reglen_C                        6.7800260  10.99614893
# std_prevf_coca_log            -13.7349788  -0.56846180
# prevwlen_C                     -4.3896680   1.60302239
# std_nextf_coca_log            -17.1999446  -5.38875452
# nextwlen_C                     -5.0717551   0.39773184
# oral.comp_bct:region_C         -1.3402023   0.49064459
# decod.comp_bct:region_C        -0.1996398   1.40887173
# gort.wpm.st:region_C            0.2241658   1.86211235
# sspan.corr.st:region_C         -1.3594316   0.68729329
# oral.comp_bct:std_f_coca_log   -5.7161125   5.95791636
# decod.comp_bct:std_f_coca_log  -0.2941568  10.70992746
# gort.wpm.st:std_f_coca_log      0.7249620  10.77786393
# sspan.corr.st:std_f_coca_log   -6.4554415   5.43690039
# oral.comp_bct:reglen_C         -4.0972449  -0.97337681
# decod.comp_bct:reglen_C        -3.4367068  -0.22801122
# gort.wpm.st:reglen_C           -2.8583186   0.13922744
# sspan.corr.st:reglen_C          1.2128667   4.41132042
# using lmeresampler package's bootstrap function

mySumm <- function(.) {
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}
boo2 <- bootstrap(model = lmer_fpurt_i, fn = mySumm, type = "parametric", B = 500)
requireNamespace("boot")
boo2
# Bootstrap Statistics :
#   original       bias    std. error
# t1*  291.3628364  0.019572403   6.1587377
# t2*  -12.1007306 -0.080149592   7.8585808
# t3*  -14.9812248 -0.007437108   7.2156168
# t4*  -23.8227066  0.139224604   6.8807937
# t5*   13.8037123 -0.110938314   8.0579119
# t6*    1.9303613  0.026502488   0.5952659
# t7*  -17.6550266 -0.232132654   2.8695344
# t8*    8.9533539 -0.066965218   1.0867943
# t9*   -7.1901665  0.061404272   3.0377145
# t10*  -1.4321454  0.066866384   1.4784622
# t11* -11.3091066  0.107383886   2.8599145
# t12*  -2.3222271 -0.062913251   1.3139384
# t13*  -0.4230606 -0.024882567   0.4328235
# t14*   0.6278080 -0.005685178   0.4370371
# t15*   1.0007620  0.003488981   0.4171471
# t16*  -0.4093679  0.006939485   0.4593920
# t17*  -0.5834747  0.216287096   3.2012419
# t18*   5.2176274 -0.044381023   2.9000943
# t19*   6.0734575 -0.038748710   2.6139784
# t20*  -0.4725522  0.033025086   3.0434731
# t21*  -2.5127304  0.051540848   0.8510388
# t22*  -1.8954393  0.002476937   0.7833432
# t23*  -1.3473288  0.018494195   0.7289319
# t24*   2.8896212 -0.060208379   0.8342788
# t25* 127.4263202  0.002426687   0.7464347
# t26*  32.1889629  0.009205714   1.7469254
# t27*   6.4887329 -0.932593448   4.0751791
# t28*  35.9491453 -0.251860525   4.3704286
# t29*  -9.3415606 -0.002229849   1.9728622
# t30*   7.4517493 -0.297939056   1.7750586


# draw significant main effects:
yRange <- c(200,500)
yrate <- 0.85
# region_C
X <- region; xlab <- 'region'; Y <- fpurt; ylab <- fpurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_f_coca_log
X <- f; xlab <- f; Y <- fpurt; ylab <- fpurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# reglen_C
X <- reglen; xlab <- reglen; Y <- fpurt; ylab <- fpurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_nextf_coca_log
X <- nextf; xlab <- nextf; Y <- fpurt; ylab <- fpurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# gort.wpm.st
X <- gort; xlab <- gort; Y <- fpurt; ylab <- fpurt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# draw significant interactions:
yrate <- 0.85
# sspan.corr.st:reglen_C
LabQuartile <- c("Low Work. Mem.", "High Work. Mem.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, reglen, fpurt, sspan, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word length", "First-pass Reading Time (ms)", "Working Memory x Word Length", 
         file.path(resdir, "Int_fpurt_sspan_reglen"))

# oral.comp_bct:reglen_C (marginal)
LabQuartile <- c("Low Comp.+Vocab.", "High Comp.+Vocab.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, reglen, fpurt, oral, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word length", "First-pass Reading Time (ms)", "Oral Comp.+Vocab. x Word Length", 
         file.path(resdir, "Int_fpurt_oral_reglen"))



### turt
dir.create(file.path('.', 'results', 'turt'), showWarnings = FALSE)
resdir <- file.path('.', 'results', 'turt')

# D1 (without stopwords)
# 1) with only skill measures
lmer_turt_i_skill <- lmer(turt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                           + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_turt_i_skill)
# REML criterion at convergence: 210645.7
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9366 -0.6006 -0.1919  0.3658 11.3590 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     4900.7   70.00        
# itemF      (Intercept)      303.1   17.41        
# subj       (Intercept)    10515.8  102.55        
# std_f_coca_log  1610.2   40.13   -0.92
# Residual                  43304.3  208.10        
# Number of obs: 15520, groups:  word:itemF, 416; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)    305.5205    10.6143  66.3022  28.784   <2e-16 ***
#   oral.comp_bct   -5.9403    10.3155  38.2621  -0.576    0.568    
# decod.comp_bct   4.8555     9.8791  38.4027   0.491    0.626    
# gort.wpm.st    -12.0675     9.2343  38.2605  -1.307    0.199    
# sspan.corr.st   -0.2524    10.1644  38.8341  -0.025    0.980    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_turt_i_skill, D1, 'lme4')
# t       df            d
# oral.comp_bct  -0.5758630 38.26205 -0.186193728
# decod.comp_bct  0.4914955 38.40273  0.158623853
# gort.wpm.st    -1.3068065 38.26051 -0.422538133
# sspan.corr.st  -0.0248286 38.83412 -0.007968482

# get Kappa and VIF
kappa.mer(lmer_turt_i_skill)
# 2.820187
vif.mer(lmer_turt_i_skill)
# oral.comp_bct decod.comp_bct    gort.wpm.st  sspan.corr.st 
# 1.930422       1.779785       1.552735       1.873778


# 2) mode without interaction, without print experience, using original frequency (not residualized frequency)
lmer_turt_i_skillword <- lmer(turt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                              + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                              + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_turt_i_skillword)
# REML criterion at convergence: 201736
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7868 -0.5966 -0.1892  0.3587 11.4400 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     3200.8   56.58        
# itemF      (Intercept)      407.1   20.18        
# subj       (Intercept)     5752.8   75.85        
# std_f_coca_log   589.8   24.28   -0.88
# Residual                  43155.1  207.74        
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        374.7870    12.1515  38.8785  30.843  < 2e-16 ***
#   oral.comp_bct       -6.5890     9.9896  38.1265  -0.660 0.513485    
# decod.comp_bct       4.9608     9.5657  38.2480   0.519 0.607026    
# gort.wpm.st         -8.4310     8.9437  38.1468  -0.943 0.351780    
# sspan.corr.st        0.3618     9.8475  38.7561   0.037 0.970884    
# region_C            -2.9718     1.0529 369.0107  -2.823 0.005022 ** 
#   std_f_coca_log     -29.8193     5.4156 124.7961  -5.506 2.00e-07 ***
#   reglen_C            12.9931     1.9958 387.6592   6.510 2.33e-10 ***
#   std_prevf_coca_log -18.5068     5.3597 381.6934  -3.453 0.000617 ***
#   prevwlen_C          -9.8106     2.6105 389.0628  -3.758 0.000197 ***
#   std_nextf_coca_log -21.5831     5.2558 383.2091  -4.106 4.91e-05 ***
#   nextwlen_C          -3.6350     2.3829 381.0171  -1.525 0.127978    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(lmer_turt_i_skillword, D1, 'lme4')
# t        df           d
# oral.comp_bct      -0.65958548  38.12646 -0.21364258
# decod.comp_bct      0.51860209  38.24803  0.16771031
# gort.wpm.st        -0.94267299  38.14677 -0.30525458
# sspan.corr.st       0.03673559  38.75607  0.01180177
# region_C           -2.82258120 369.01073 -0.29387114
# std_f_coca_log     -5.50618349 124.79614 -0.98578021
# reglen_C            6.51021318 387.65924  0.66130246
# std_prevf_coca_log -3.45296813 381.69340 -0.35348032
# prevwlen_C         -3.75816247 389.06281 -0.38106204
# std_nextf_coca_log -4.10649393 383.20907 -0.41954958
# nextwlen_C         -1.52544279 381.01709 -0.15629810

# get Kappa and VIF
kappa.mer(lmer_turt_i_skillword)
# 3.015076
vif.mer(lmer_turt_i_skillword)
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1.928178           1.778219           1.553250           1.873106           1.044590           1.179507           1.212361 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 2.529130           2.531280           2.387054           2.363009


# 3) with skill, word, and interaction
lmer_turt_i <- lmer(turt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(lmer_turt_i)
# REML criterion at convergence: 201632.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9036 -0.5949 -0.1924  0.3642 11.3379 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)     3213.1   56.68        
# itemF      (Intercept)      397.9   19.95        
# subj       (Intercept)     4503.1   67.11        
# std_f_coca_log   342.1   18.50   -0.85
# Residual                  43025.2  207.43        
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     374.9093    10.9155    49.1178  34.346  < 2e-16 ***
#   oral.comp_bct                   -12.4208    14.4813    38.8664  -0.858 0.396308    
# decod.comp_bct                  -18.3167    13.8555    38.8673  -1.322 0.193901    
# gort.wpm.st                     -34.4185    12.9641    38.8828  -2.655 0.011437 *  
#   sspan.corr.st                    17.0229    14.2194    38.9184   1.197 0.238486    
# region_C                         -2.8666     1.0538   369.5872  -2.720 0.006834 ** 
#   std_f_coca_log                  -29.8104     4.8695   155.6970  -6.122 7.22e-09 ***
#   reglen_C                         12.9602     1.9967   387.9432   6.491 2.61e-10 ***
#   std_prevf_coca_log              -18.5729     5.3624   381.9583  -3.464 0.000594 ***
#   prevwlen_C                       -9.8304     2.6116   389.3106  -3.764 0.000193 ***
#   std_nextf_coca_log              -21.5060     5.2583   383.4576  -4.090 5.26e-05 ***
#   nextwlen_C                       -3.6426     2.3842   381.3004  -1.528 0.127383    
# oral.comp_bct:region_C            1.1795     0.7410 14435.7882   1.592 0.111427    
# decod.comp_bct:region_C           1.4317     0.7070 14428.0604   2.025 0.042865 *  
#   gort.wpm.st:region_C              2.0131     0.6612 14437.0461   3.044 0.002335 ** 
#   sspan.corr.st:region_C           -2.1957     0.7324 14436.5910  -2.998 0.002724 ** 
#   oral.comp_bct:std_f_coca_log      0.7784     4.8074    45.7636   0.162 0.872090    
# decod.comp_bct:std_f_coca_log     7.9387     4.6035    45.9058   1.724 0.091347 .  
# gort.wpm.st:std_f_coca_log        9.7050     4.3059    45.8715   2.254 0.029024 *  
#   sspan.corr.st:std_f_coca_log     -3.2504     4.7331    46.3047  -0.687 0.495668    
# oral.comp_bct:reglen_C           -1.7813     1.3764 14429.7141  -1.294 0.195629    
# decod.comp_bct:reglen_C          -2.1924     1.3164 14424.5917  -1.665 0.095836 .  
# gort.wpm.st:reglen_C             -1.6247     1.2349 14447.9907  -1.316 0.188309    
# sspan.corr.st:reglen_C            3.8852     1.3528 14433.0763   2.872 0.004084 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# critical p value is 0.05/23 = 0.002173913
# significant main effects: region_C (marginal), std_f_coca_log, reglen_C, std_prevf_coca_log, prevwlen_C, std_nextf_coca_log
# significant interactions: gort.wpm.st:region_C (marginal), sspan.corr.st:region_C (marginal), sspan.corr.st:reglen_C (marginal)

# get effect size
lme.dscore(lmer_turt_i, D1, 'lme4')
# t          df           d
# oral.comp_bct                 -0.8577148    38.86641 -0.27516026
# decod.comp_bct                -1.3219867    38.86731 -0.42409665
# gort.wpm.st                   -2.6549058    38.88284 -0.85153036
# sspan.corr.st                  1.1971577    38.91842  0.38379890
# region_C                      -2.7201441   369.58723 -0.28298501
# std_f_coca_log                -6.1218016   155.69702 -0.98122612
# reglen_C                       6.4907351   387.94319  0.65908255
# std_prevf_coca_log            -3.4635379   381.95827 -0.35443939
# prevwlen_C                    -3.7641221   389.31056 -0.38154486
# std_nextf_coca_log            -4.0899160   383.45760 -0.41772042
# nextwlen_C                    -1.5278324   381.30036 -0.15648478
# oral.comp_bct:region_C         1.5919114 14435.78822  0.02649895
# decod.comp_bct:region_C        2.0252045 14428.06036  0.03372057
# gort.wpm.st:region_C           3.0444469 14437.04607  0.05067564
# sspan.corr.st:region_C        -2.9978130 14436.59099 -0.04990019
# oral.comp_bct:std_f_coca_log   0.1619090    45.76361  0.04786750
# decod.comp_bct:std_f_coca_log  1.7244961    45.90584  0.50904744
# gort.wpm.st:std_f_coca_log     2.2538940    45.87154  0.66556710
# sspan.corr.st:std_f_coca_log  -0.6867452    46.30475 -0.20184262
# oral.comp_bct:reglen_C        -1.2941660 14429.71410 -0.02154721
# decod.comp_bct:reglen_C       -1.6654928 14424.59167 -0.02773454
# gort.wpm.st:reglen_C          -1.3156594 14447.99069 -0.02189121
# sspan.corr.st:reglen_C         2.8720963 14433.07633  0.04781339

kappa.mer(lmer_turt_i)
# 5.742707
vif.mer(lmer_turt_i)
# oral.comp_bct                decod.comp_bct                   gort.wpm.st                 sspan.corr.st 
# 3.978539                      3.662476                      3.203982                      3.834833 
# region_C                std_f_coca_log                      reglen_C            std_prevf_coca_log 
# 1.045123                      1.231900                      1.264612                      2.529635 
# prevwlen_C            std_nextf_coca_log                    nextwlen_C        oral.comp_bct:region_C 
# 2.532831                      2.386742                      2.363200                      1.954278 
# decod.comp_bct:region_C          gort.wpm.st:region_C        sspan.corr.st:region_C  oral.comp_bct:std_f_coca_log 
# 1.800108                      1.575515                      1.900737                      4.328874 
# decod.comp_bct:std_f_coca_log    gort.wpm.st:std_f_coca_log  sspan.corr.st:std_f_coca_log        oral.comp_bct:reglen_C 
# 3.994723                      3.496176                      4.195805                      2.306033 
# decod.comp_bct:reglen_C          gort.wpm.st:reglen_C        sspan.corr.st:reglen_C 
# 2.150242                      1.882443                      2.253608 

# run the party implementation
cf_turt_i <- cforest(turt ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                      + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                      + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                      + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                      + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                      , data=D1, control=cforest_unbiased(mtry=2,ntree=50))
cf_turt_i_varimp <- varimp(cf_turt_i)
cf_turt_i_varimp
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 4449.519           3806.801           5018.327           1815.958           2749.082           3922.223           3512.452 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 1033.129           1416.382           1556.524            926.854 
png(file.path(resdir, "cf_turt_i_varimp.png"), width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_turt_i_varimp), panel=function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_turt_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

r.squaredGLMM(lmer_turt_i_skill)
# R2m       R2c
# 0.00294022 0.2879071
r.squaredGLMM(lmer_turt_i_skillword)
# R2m      R2c
# 0.05458757 0.2329715
r.squaredGLMM(lmer_turt_i)
# R2m       R2c
# 0.09493955 0.2444159

# anova model selection
anova(lmer_turt_i, lmer_turt_i_skillword)
# npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)   
# lmer_turt_i_skillword   18 201829 201966 -100896   201793                        
# lmer_turt_i             30 201779 202008 -100860   201719  73.2 12  8.031e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# get bootstrapping results
r3 <- report(lmer_turt_i, bootstrap=TRUE, iterations = 500)
table_long(r3)
# Parameter          | Coefficient |               CI |      p |      Fit
# -----------------------------------------------------------------------
#   (Intercept)        |      374.32 | [354.98, 397.13] | < .001 |         
#   oral.comp_bct      |      -13.11 | [-41.27,  14.89] | 0.399  |         
#   decod.comp_bct     |      -17.25 | [-44.55,  10.54] | 0.208  |         
#   gort.wpm.st        |      -33.84 | [-57.84,  -8.84] | 0.004  |         
#   sspan.corr.st      |       16.35 | [-14.64,  42.21] | 0.263  |         
#   region_C           |       -2.77 | [ -4.90,  -0.76] | 0.016  |         
#   std_f_coca_log     |      -29.53 | [-39.38, -20.26] | < .001 |         
#   reglen_C           |       12.94 | [  9.24,  16.50] | < .001 |         
#   std_prevf_coca_log |      -18.20 | [-28.68,  -8.73] | < .001 |         
#   prevwlen_C         |       -9.51 | [-15.23,  -4.46] | < .001 |         
#   std_nextf_coca_log |      -21.58 | [-31.83, -11.19] | < .001 |         
#   nextwlen_C         |       -3.60 | [ -7.99,   0.88] | 0.136  |         
#   |        1.20 | [ -0.34,   2.54] | 0.116  |         
#   |        1.44 | [  0.11,   2.92] | 0.032  |         
#   |        2.03 | [  0.62,   3.38] | 0.008  |         
#   |       -2.11 | [ -3.73,  -0.73] | < .001 |         
#   |        0.99 | [ -8.50,   9.42] | 0.834  |         
#   |        7.68 | [ -0.98,  17.22] | 0.104  |         
#   |        9.78 | [  0.54,  17.50] | 0.032  |         
#   |       -2.91 | [-11.83,   7.06] | 0.515  |         
#   |       -1.85 | [ -4.34,   0.58] | 0.136  |         
#   |       -2.16 | [ -4.84,   0.32] | 0.112  |         
#   |       -1.65 | [ -4.03,   1.09] | 0.212  |         
#   |        3.92 | [  1.55,   6.61] | < .001 |         
#   |             |                  |        |         
#   AIC                |             |                  |        | 2.02e+05
# BIC                |             |                  |        | 2.02e+05
# R2 (conditional)   |             |                  |        |     0.24
# R2 (marginal)      |             |                  |        |     0.09
# ICC                |             |                  |        |     0.17
# RMSE               |             |                  |        |   204.83

# using boot for bootstrap
confint(lmer_turt_i, method="boot", nsim=500, boot.type="perc")
# 2.5 %      97.5 %
#   .sig01                         50.37373336  62.9851693
# .sig02                          0.00509059  29.1900120
# .sig03                         52.97146235  82.4267123
# .sig04                         -0.99977827  -0.6682288
# .sig05                         13.26184632  24.2781387
# .sigma                        204.84203326 209.9646105
# (Intercept)                   353.62398776 398.3293102
# oral.comp_bct                 -39.98472275  15.0068474
# decod.comp_bct                -45.94970709   8.8632921
# gort.wpm.st                   -60.43649410  -7.7253917
# sspan.corr.st                  -9.16354687  44.3156016
# region_C                       -4.91844252  -0.5121837
# std_f_coca_log                -39.08169907 -19.8518295
# reglen_C                        9.33535973  17.2729553
# std_prevf_coca_log            -29.02591076  -7.9972308
# prevwlen_C                    -15.49035442  -5.1858755
# std_nextf_coca_log            -32.28898595 -10.5061620
# nextwlen_C                     -8.50854315   0.8370745
# oral.comp_bct:region_C         -0.39179512   2.6037689
# decod.comp_bct:region_C         0.08758880   2.8649209
# gort.wpm.st:region_C            0.80954767   3.3112504
# sspan.corr.st:region_C         -3.68755261  -0.7597656
# oral.comp_bct:std_f_coca_log   -7.76585531  10.1088650
# decod.comp_bct:std_f_coca_log  -1.70721583  17.1623082
# gort.wpm.st:std_f_coca_log      1.66741862  17.9720924
# sspan.corr.st:std_f_coca_log  -13.15836487   5.3425246
# oral.comp_bct:reglen_C         -4.37886146   0.9011370
# decod.comp_bct:reglen_C        -4.58389204   0.5298939
# gort.wpm.st:reglen_C           -3.93207020   0.9378532
# sspan.corr.st:reglen_C          0.99049548   6.3844870

mySumm <- function(.) {
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}
boo3 <- bootstrap(model = lmer_turt_i, fn = mySumm, type = "parametric", B = 500)
requireNamespace("boot")
boo3
# Bootstrap Statistics :
#   original        bias    std. error
# t1*  374.909306 -0.3977005901  10.6914129
# t2*  -12.420795 -1.0977217563  14.5665133
# t3*  -18.316731  0.1639085653  13.5727436
# t4*  -34.418465 -0.0917763553  12.6998217
# t5*   17.022917  0.1781137696  13.8901764
# t6*   -2.866574 -0.0089792644   0.9816581
# t7*  -29.810404  0.0202358860   5.0232963
# t8*   12.960228  0.1399218807   1.8598432
# t9*  -18.572876 -0.0996089816   5.3012204
# t10*  -9.830437 -0.1123745661   2.4554311
# t11* -21.505998 -0.0914585378   5.4128507
# t12*  -3.642619  0.0192260720   2.4350239
# t13*   1.179543  0.0162551510   0.8075166
# t14*   1.431720  0.0014070087   0.6908507
# t15*   2.013102  0.0011548119   0.6747511
# t16*  -2.195661 -0.0036492183   0.7257701
# t17*   0.778368  0.2638092766   4.8678971
# t18*   7.938656 -0.2067453506   4.5265448
# t19*   9.704998 -0.2327672209   4.4275488
# t20*  -3.250416  0.4615214578   4.8175948
# t21*  -1.781330  0.0257503706   1.3791261
# t22*  -2.192409  0.0005905055   1.3334823
# t23*  -1.624714 -0.0924951618   1.2362287
# t24*   3.885247  0.0859673432   1.4640114
# t25* 207.425260 -0.0267637286   1.2370673
# t26*  56.684524 -0.2032680973   2.9975801
# t27*  19.947250 -0.5766390152   5.9262887
# t28*  67.105068 -0.4772283627   8.0720372
# t29* -15.761061  0.1144288355   3.0742043
# t30*   9.679615 -0.5721051708   3.1809996


# draw significant main effects:
yRange <- c(250,600)
yrate <- 0.85
# region_C (marginal)
X <- region; xlab <- 'region'; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_f_coca_log
X <- f; xlab <- f; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# reglen_C
X <- reglen; xlab <- reglen; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_prevf_coca_log
X <- prevf; xlab <- prevf; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# prevwlen_C
X <- prevwlen; xlab <- prevwlen; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_nextf_coca_log
X <- nextf; xlab <- nextf; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# gort.wpm.st (marginal)
X <- gort; xlab <- gort; Y <- turt; ylab <- turt; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# draw significant interactions:
yrate <- 0.85
# gort.wpm.st:region_C (marginal)
LabQuartile <- c("Low Oral Fluency", "High Oral Fluency")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, region, turt, gort, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word position", "Total Reading Time (ms)", "Oral Fluency x Word Position", 
         file.path(resdir, "Int_turt_gort_region"))

# sspan.corr.st:region_C (marginal)
LabQuartile <- c("Low Work. Mem.", "High Work. Mem.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, region, turt, sspan, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word position", "Total Reading Time (ms)", "Working Memory x Word Position", 
         file.path(resdir, "Int_turt_sspan_region"))

# sspan.corr.st:reglen_C (marginal)
LabQuartile <- c("Low Work. Mem.", "High Work. Mem.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, reglen, turt, sspan, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word length", "Total Reading Time (ms)", "Working Memory x Word Length", 
         file.path(resdir, "Int_turt_sspan_reglen"))



### fpregres
dir.create(file.path('.', 'results', 'fpregres'), showWarnings = FALSE)
resdir <- file.path('.', 'results', 'fpregres')

# D1 (without stopwords)
# 1) with only skill measures
glmer_fpregres_i_skill <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                          + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_i_skill)
# AIC      BIC   logLik deviance df.resid 
# 13019.9  13096.4  -6499.9  12999.9    15510 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7537 -0.4579 -0.3410 -0.2401  5.2126 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)    0.539869 0.73476       
# itemF      (Intercept)    0.000000 0.00000       
# subj       (Intercept)    0.205589 0.45342       
# std_f_coca_log 0.008307 0.09114  -0.26
# Number of obs: 15520, groups:  word:itemF, 416; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -1.88485    0.08228 -22.908   <2e-16 ***
#   oral.comp_bct   0.08300    0.10104   0.821   0.4114    
# decod.comp_bct  0.07112    0.09875   0.720   0.4714    
# gort.wpm.st    -0.15656    0.09029  -1.734   0.0829 .  
# sspan.corr.st  -0.25726    0.10313  -2.495   0.0126 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(glmer_fpregres_i_skill, D1, 'lme4')
# t       df            d
# oral.comp_bct   0.9710151 38.82741  0.3116641
# decod.comp_bct  0.4634514 38.82590  0.1487556
# gort.wpm.st    -1.7898450 38.90345 -0.5739200
# sspan.corr.st  -2.5906744 39.06607 -0.8289779

# get Kappa and VIF
kappa.mer(glmer_fpregres_i_skill)
# 2.820187
vif.mer(glmer_fpregres_i_skill)
# oral.comp_bct decod.comp_bct    gort.wpm.st  sspan.corr.st 
# 1.796085       1.590194       1.463643       1.653127


# 2) mode without interaction, without print experience, using original frequency (not residualized frequency)
glmer_fpregres_i_skillword <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                              + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                              + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_i_skillword)
# AIC      BIC   logLik deviance df.resid 
# 12467.1  12596.4  -6216.5  12433.1    14864 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7593 -0.4584 -0.3426 -0.2403  4.9842 
# 
# Random effects:
#   Groups     Name           Variance Std.Dev. Corr 
# word:itemF (Intercept)    0.447620 0.66904       
# itemF      (Intercept)    0.006408 0.08005       
# subj       (Intercept)    0.210861 0.45920       
# std_f_coca_log 0.006961 0.08343  -0.20
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.880036   0.082347 -22.831  < 2e-16 ***
#   oral.comp_bct       0.082846   0.103187   0.803  0.42205    
# decod.comp_bct      0.067576   0.101078   0.669  0.50378    
# gort.wpm.st        -0.158354   0.091999  -1.721  0.08520 .  
# sspan.corr.st      -0.238403   0.105785  -2.254  0.02422 *  
#   region_C            0.010760   0.012860   0.837  0.40278    
# std_f_coca_log     -0.067960   0.050268  -1.352  0.17639    
# reglen_C            0.003664   0.024295   0.151  0.88012    
# std_prevf_coca_log -0.186889   0.066788  -2.798  0.00514 ** 
#   prevwlen_C         -0.172200   0.032679  -5.269 1.37e-07 ***
#   std_nextf_coca_log -0.008369   0.064009  -0.131  0.89597    
# nextwlen_C          0.010761   0.029070   0.370  0.71125    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(glmer_fpregres_i_skillword, D1, 'lme4')
# t        df           d
# oral.comp_bct       0.96099387  38.81081  0.308513551
# decod.comp_bct      0.37886039  38.80407  0.121638363
# gort.wpm.st        -1.79087760  38.90221 -0.574260261
# sspan.corr.st      -2.52393041  39.08254 -0.807450594
# region_C            0.85594119 391.36312  0.086533441
# std_f_coca_log     -1.28899230 245.91246 -0.164395546
# reglen_C           -0.02123212 383.78081 -0.002167612
# std_prevf_coca_log -2.56530547 385.84454 -0.261193825
# prevwlen_C         -4.98423835 389.99790 -0.504774799
# std_nextf_coca_log -0.23849224 383.62847 -0.024352796
# nextwlen_C          0.10900343 384.97028  0.011111088

# get Kappa and VIF
kappa.mer(glmer_fpregres_i_skillword)
# 3.015076
vif.mer(glmer_fpregres_i_skillword)
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1.803165           1.570198           1.447460           1.640204           1.051756           1.312361           1.348932 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 2.495540           2.505984           2.343158           2.346250 


# 3) with skill, word, and interaction
glmer_fpregres_i <- glmer(fpregres ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                    + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                    + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                    + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                    + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                    + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_i)
# AIC      BIC   logLik deviance df.resid 
# 12446.7  12667.3  -6194.3  12388.7    14852 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7807 -0.4578 -0.3405 -0.2360  5.0862 
# 
# Random effects:
#   Groups     Name           Variance  Std.Dev. Corr 
# word:itemF (Intercept)    0.4494306 0.67040       
# itemF      (Intercept)    0.0062467 0.07904       
# subj       (Intercept)    0.2094292 0.45763       
# std_f_coca_log 0.0002052 0.01433  -1.00
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.8864692  0.0821982 -22.950  < 2e-16 ***
#   oral.comp_bct                  0.0763244  0.1027152   0.743  0.45744    
# decod.comp_bct                 0.0691550  0.0988265   0.700  0.48408    
# gort.wpm.st                   -0.1619540  0.0920286  -1.760  0.07844 .  
# sspan.corr.st                 -0.2202989  0.1007218  -2.187  0.02873 *  
#   region_C                       0.0123968  0.0129337   0.958  0.33782    
# std_f_coca_log                -0.0796258  0.0487847  -1.632  0.10264    
# reglen_C                      -0.0006564  0.0244539  -0.027  0.97859    
# std_prevf_coca_log            -0.1881890  0.0669285  -2.812  0.00493 ** 
#   prevwlen_C                    -0.1734169  0.0327539  -5.295 1.19e-07 ***
#   std_nextf_coca_log            -0.0072698  0.0641355  -0.113  0.90975    
# nextwlen_C                     0.0103040  0.0291279   0.354  0.72353    
# oral.comp_bct:region_C         0.0071927  0.0095302   0.755  0.45041    
# decod.comp_bct:region_C        0.0309399  0.0094451   3.276  0.00105 ** 
#   gort.wpm.st:region_C           0.0097684  0.0085123   1.148  0.25115    
# sspan.corr.st:region_C        -0.0199162  0.0093122  -2.139  0.03246 *  
#   oral.comp_bct:std_f_coca_log  -0.0117567  0.0360504  -0.326  0.74433    
# decod.comp_bct:std_f_coca_log -0.0228381  0.0356984  -0.640  0.52233    
# gort.wpm.st:std_f_coca_log    -0.0363010  0.0321918  -1.128  0.25947    
# sspan.corr.st:std_f_coca_log  -0.0495883  0.0350411  -1.415  0.15703    
# oral.comp_bct:reglen_C        -0.0215209  0.0181532  -1.186  0.23581    
# decod.comp_bct:reglen_C        0.0260052  0.0181068   1.436  0.15094    
# gort.wpm.st:reglen_C          -0.0367704  0.0165436  -2.223  0.02624 *  
#   sspan.corr.st:reglen_C         0.0028118  0.0175674   0.160  0.87284    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# critical p value is 0.05/23 = 0.002173913
# significant main effects: std_prevf_coca_log (marginal), prevwlen_C
# significant interactions: decod.comp_bct:region_C

# get effect size
lme.dscore(glmer_fpregres_i, D1, 'lme4')
# t          df           d
# oral.comp_bct                  0.83730650    38.93551  0.268374679
# decod.comp_bct                 0.59195700    38.93762  0.189729753
# gort.wpm.st                   -1.80093547    38.98797 -0.576849893
# sspan.corr.st                 -2.15685306    39.10080 -0.689854902
# region_C                       0.92568836   391.80283  0.093532171
# std_f_coca_log                -1.31680462   247.08317 -0.167544336
# reglen_C                      -0.04123035   383.99265 -0.004208095
# std_prevf_coca_log            -2.56547223   385.90439 -0.261190548
# prevwlen_C                    -4.99145340   390.05805 -0.505466518
# std_nextf_coca_log            -0.23558414   383.69602 -0.024053728
# nextwlen_C                     0.09435878   385.05672  0.009617229
# oral.comp_bct:region_C         0.66753770 14442.89550  0.011109094
# decod.comp_bct:region_C        3.32314335 14433.84730  0.055320745
# gort.wpm.st:region_C           1.41150384 14444.08892  0.023489133
# sspan.corr.st:region_C        -2.32410805 14443.84863 -0.038676293
# oral.comp_bct:std_f_coca_log  -0.06312328    63.83972 -0.015800618
# decod.comp_bct:std_f_coca_log -0.64742329    64.23769 -0.161556096
# gort.wpm.st:std_f_coca_log    -1.04570186    64.21601 -0.260985398
# sspan.corr.st:std_f_coca_log  -1.27429137    65.69620 -0.314433366
# oral.comp_bct:reglen_C        -1.14217049 14435.20363 -0.019012949
# decod.comp_bct:reglen_C        1.28390830 14429.17700  0.021376826
# gort.wpm.st:reglen_C          -2.37039979 14455.03308 -0.039431387
# sspan.corr.st:reglen_C         0.21053829 14440.73422  0.003504019

kappa.mer(glmer_fpregres_i)
# 5.742707
vif.mer(glmer_fpregres_i)
# oral.comp_bct                decod.comp_bct                   gort.wpm.st                 sspan.corr.st 
# 1.936633                      1.803749                      1.577537                      1.880240 
# region_C                std_f_coca_log                      reglen_C            std_prevf_coca_log 
# 1.058638                      1.341271                      1.388982                      2.496932 
# prevwlen_C            std_nextf_coca_log                    nextwlen_C        oral.comp_bct:region_C 
# 2.508249                      2.343814                      2.347008                      1.844159 
# decod.comp_bct:region_C          gort.wpm.st:region_C        sspan.corr.st:region_C  oral.comp_bct:std_f_coca_log 
# 1.928639                      1.684742                      1.925713                      2.339677 
# decod.comp_bct:std_f_coca_log    gort.wpm.st:std_f_coca_log  sspan.corr.st:std_f_coca_log        oral.comp_bct:reglen_C 
# 2.442499                      2.161107                      2.450304                      2.346030 
# decod.comp_bct:reglen_C          gort.wpm.st:reglen_C        sspan.corr.st:reglen_C 
# 2.523978                      2.237090                      2.477993 

# run the party implementation
cf_fpregres_i <- cforest(fpregres ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                     + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                     + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                     + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                     + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                     , data=D1, control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_i_varimp <- varimp(cf_fpregres_i)
cf_fpregres_i_varimp
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 0.002774459        0.002616645        0.004804613        0.004119330        0.004992387        0.002380098        0.001972755 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 0.003954039        0.004392853        0.002109052        0.001187818 
png(file.path(resdir, "cf_fpregres_i_varimp.png"), width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_i_varimp), panel=function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

r.squaredGLMM(glmer_fpregres_i_skill)
# R2m       R2c
# theoretical 0.01759632 0.2007240
# delta       0.00899170 0.1025697
r.squaredGLMM(glmer_fpregres_i_skillword)
# R2m      R2c
# theoretical 0.03348885 0.1974878
# delta       0.01707822 0.1007123
r.squaredGLMM(glmer_fpregres_i)
# R2m       R2c
# theoretical 0.03826375 0.2001045
# delta       0.01954449 0.1022101

# anova model selection
anova(glmer_fpregres_i, glmer_fpregres_i_skillword)
# npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)   
# glmer_fpregres_i_skillword   17 12467 12596 -6216.5    12433                         
# glmer_fpregres_i             29 12447 12667 -6194.3    12389 44.411 12    1.3e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# for glmer object, report bootstrap does not work
# # get bootstrapping results
# r3 <- report(glmer_fpregres_i, bootstrap=TRUE, iterations = 500)
# table_long(r3)

# mySumm <- function(.) {
#   s <- getME(., "sigma")
#   c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
# }
# boo4 <- bootstrap(model = lmer_fpregres_i, fn = mySumm, type = "parametric", B = 500)
# requireNamespace("boot")
# boo4
  

# draw significant main effects:
yRange <- c(0,0.5)
yrate <- 0.85
# std_prevf_coca_log (marginal)
X <- prevf; xlab <- prevf; Y <- fpregres; ylab <- fpregres; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 2)

# prevwlen_C
X <- prevwlen; xlab <- prevwlen; Y <- fpregres; ylab <- fpregres; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# draw significant interactions:
yrate <- 0.85
# decod.comp_bct:region_C
LabQuartile <- c("Low Decod.", "High Decod.")
# to get interval based on real values, use "interval"; to get equal number of participants, use "number"
Draw_Int("linear", "number", 0, D1, region, fpregres, decod, numQuartile, LabQuartile, yRange, yrate, 0,
         "Word position", "First-pass Regression (prob)", "Decoding x Word Position", 
         file.path(resdir, "Int_fpregres_decod_region"))



### fpregres_in
dir.create(file.path('.', 'results', 'fpregres_in'), showWarnings = FALSE)
resdir <- file.path('.', 'results', 'fpregres_in')

# D1 (without stopwords)
# 1) with only skill measures
glmer_fpregres_in_i_skill <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                                + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_in_i_skill)
# AIC      BIC   logLik deviance df.resid 
# 16625.4  16701.9  -8302.7  16605.4    15510 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.9497 -0.5922 -0.4404  0.7530  4.7066 
# 
# Random effects:
#   Groups     Name           Variance  Std.Dev. Corr 
# word:itemF (Intercept)    0.3013085 0.54892       
# itemF      (Intercept)    0.0350578 0.18724       
# subj       (Intercept)    0.2985233 0.54637       
# std_f_coca_log 0.0002844 0.01686  -1.00
# Number of obs: 15520, groups:  word:itemF, 416; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -1.253858   0.094808 -13.225   <2e-16 ***
#   oral.comp_bct  -0.003534   0.118478  -0.030    0.976    
# decod.comp_bct  0.021827   0.113633   0.192    0.848    
# gort.wpm.st    -0.068615   0.105997  -0.647    0.517    
# sspan.corr.st  -0.010242   0.121530  -0.084    0.933    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(glmer_fpregres_in_i_skill, D1, 'lme4')
# t       df            d
# oral.comp_bct   0.06848876 38.99430  0.02193556
# decod.comp_bct  0.19894695 39.02202  0.06369604
# gort.wpm.st    -0.48852513 39.03636 -0.15638038
# sspan.corr.st  -0.38993452 39.25692 -0.12446969

# get Kappa and VIF
kappa.mer(glmer_fpregres_in_i_skill)
# 2.820187
vif.mer(glmer_fpregres_in_i_skill)
# oral.comp_bct decod.comp_bct    gort.wpm.st  sspan.corr.st 
# 1.885763       1.765965       1.538856       1.828405 


# 2) mode without interaction, without print experience, using original frequency (not residualized frequency)
glmer_fpregres_in_i_skillword <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st
                                    + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                                    + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_in_i_skillword)
# AIC      BIC   logLik deviance df.resid 
# 15744.2  15873.5  -7855.1  15710.2    14864 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8962 -0.5894 -0.4377 -0.1933  4.7094 
# 
# Random effects:
#   Groups     Name           Variance  Std.Dev.  Corr 
# word:itemF (Intercept)    1.658e-01 4.072e-01      
# itemF      (Intercept)    4.946e-02 2.224e-01      
# subj       (Intercept)    3.023e-01 5.498e-01      
# std_f_coca_log 4.205e-09 6.485e-05 -1.00
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.240201   0.091992 -13.482  < 2e-16 ***
#   oral.comp_bct      -0.007521   0.120257  -0.063 0.950132    
# decod.comp_bct      0.009133   0.115195   0.079 0.936809    
# gort.wpm.st        -0.060725   0.107878  -0.563 0.573503    
# sspan.corr.st       0.024129   0.121876   0.198 0.843064    
# region_C           -0.063240   0.009172  -6.895 5.39e-12 ***
#   std_f_coca_log     -0.055725   0.035222  -1.582 0.113628    
# reglen_C            0.062365   0.017456   3.573 0.000353 ***
#   std_prevf_coca_log -0.144642   0.047170  -3.066 0.002167 ** 
#   prevwlen_C         -0.106287   0.023207  -4.580 4.65e-06 ***
#   std_nextf_coca_log -0.160462   0.045739  -3.508 0.000451 ***
#   nextwlen_C         -0.027093   0.020733  -1.307 0.191300    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# get effect size
lme.dscore(glmer_fpregres_in_i_skillword, D1, 'lme4')
# t        df           d
# oral.comp_bct       0.006350273  38.87531  0.002036974
# decod.comp_bct      0.068035698  38.88607  0.021820756
# gort.wpm.st        -0.264868392  38.93403 -0.084897609
# sspan.corr.st      -0.235162988  39.12368 -0.075193299
# region_C           -6.930579755 363.89949 -0.726622491
# std_f_coca_log     -1.678813209 342.94082 -0.181310427
# reglen_C            3.324705451 381.22930  0.340557175
# std_prevf_coca_log -3.128399996 373.86910 -0.323588060
# prevwlen_C         -4.636128368 382.67726 -0.473989938
# std_nextf_coca_log -3.534773899 375.50557 -0.364824038
# nextwlen_C         -1.367072233 373.24571 -0.141522028

# get Kappa and VIF
kappa.mer(glmer_fpregres_in_i_skillword)
# 3.015076
vif.mer(glmer_fpregres_in_i_skillword)
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 1.889046           1.756584           1.559957           1.831820           1.041549           1.374342           1.393409 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 2.524472           2.514133           2.363288           2.323576 


# 3) with skill, word, and interaction
glmer_fpregres_in_i <- glmer(fpregres_in ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                          + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                          + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                          + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                          + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                          + (1|itemF/word) + (1+std_f_coca_log|subj), data = D1, family = binomial(link='logit'), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)), nAGQ = 1)
summary(glmer_fpregres_in_i)
# AIC      BIC   logLik deviance df.resid 
# 15754.7  15975.3  -7848.3  15696.7    14852 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8693 -0.5881 -0.4375 -0.1885  4.8885 
# 
# Random effects:
#   Groups     Name           Variance  Std.Dev. Corr
# word:itemF (Intercept)    1.664e-01 0.407867     
# itemF      (Intercept)    4.961e-02 0.222723     
# subj       (Intercept)    3.035e-01 0.550942     
# std_f_coca_log 2.169e-06 0.001473 1.00
# Number of obs: 14881, groups:  word:itemF, 399; itemF, 72; subj, 44
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.242786   0.092182 -13.482  < 2e-16 ***
#   oral.comp_bct                 -0.011010   0.120524  -0.091 0.927215    
# decod.comp_bct                 0.011492   0.115465   0.100 0.920721    
# gort.wpm.st                   -0.060815   0.107919  -0.564 0.573077    
# sspan.corr.st                  0.017868   0.118231   0.151 0.879874    
# region_C                      -0.063060   0.009191  -6.861 6.83e-12 ***
#   std_f_coca_log                -0.055916   0.035252  -1.586 0.112704    
# reglen_C                       0.063193   0.017494   3.612 0.000303 ***
#   std_prevf_coca_log            -0.145538   0.047227  -3.082 0.002058 ** 
#   prevwlen_C                    -0.106389   0.023232  -4.580 4.66e-06 ***
#   std_nextf_coca_log            -0.160968   0.045796  -3.515 0.000440 ***
#   nextwlen_C                    -0.027020   0.020760  -1.302 0.193063    
# oral.comp_bct:region_C         0.008327   0.008401   0.991 0.321556    
# decod.comp_bct:region_C        0.008238   0.008128   1.014 0.310794    
# gort.wpm.st:region_C          -0.003820   0.007417  -0.515 0.606500    
# sspan.corr.st:region_C        -0.012549   0.008131  -1.543 0.122733    
# oral.comp_bct:std_f_coca_log   0.013739   0.030956   0.444 0.657182    
# decod.comp_bct:std_f_coca_log  0.000349   0.030062   0.012 0.990738    
# gort.wpm.st:std_f_coca_log     0.009048   0.027527   0.329 0.742388    
# sspan.corr.st:std_f_coca_log  -0.022940   0.030029  -0.764 0.444899    
# oral.comp_bct:reglen_C         0.016893   0.015476   1.092 0.275030    
# decod.comp_bct:reglen_C       -0.003895   0.014950  -0.261 0.794463    
# gort.wpm.st:reglen_C           0.001433   0.013715   0.104 0.916779    
# sspan.corr.st:reglen_C         0.017489   0.014863   1.177 0.239323    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# critical p value is 0.05/23 = 0.002173913
# significant main effects: region_C, reglen_C, std_prevf_coca_log, prevwlen_C, std_nextf_coca_log
# significant interactions: NA

# get effect size
lme.dscore(glmer_fpregres_in_i, D1, 'lme4')
# t          df           d
# oral.comp_bct                 -0.026141190    38.91189 -0.0083813540
# decod.comp_bct                 0.023991303    38.91297  0.0076919535
# gort.wpm.st                   -0.379034228    38.94140 -0.1214794019
# sspan.corr.st                  0.204327031    39.00445  0.0654332913
# region_C                      -6.890584713   364.56218 -0.7217723927
# std_f_coca_log                -1.674098579   341.89300 -0.1810780965
# reglen_C                       3.352994373   381.63598  0.3432718276
# std_prevf_coca_log            -3.146540484   374.08022 -0.3253725776
# prevwlen_C                    -4.639518446   382.88440 -0.4742082071
# std_nextf_coca_log            -3.545498563   375.73785 -0.3658178014
# nextwlen_C                    -1.364560972   373.50350 -0.1412133008
# oral.comp_bct:region_C         1.167156254 14483.64804  0.0193963502
# decod.comp_bct:region_C        0.965124207 14475.13010  0.0160436052
# gort.wpm.st:region_C          -0.287654185 14487.93193 -0.0047796654
# sspan.corr.st:region_C        -1.665325817 14480.74492 -0.0276779397
# oral.comp_bct:std_f_coca_log   0.494485132   410.39000  0.0488185472
# decod.comp_bct:std_f_coca_log  0.001347338   413.52320  0.0001325124
# gort.wpm.st:std_f_coca_log     0.412383796   412.81912  0.0405930501
# sspan.corr.st:std_f_coca_log  -0.765797309   424.07175 -0.0743745176
# oral.comp_bct:reglen_C         0.930146045 14479.90257  0.0154596024
# decod.comp_bct:reglen_C       -0.213333005 14469.38867 -0.0035470144
# gort.wpm.st:reglen_C           0.044102418 14495.74397  0.0007326088
# sspan.corr.st:reglen_C         1.215969546 14485.71233  0.0202061124

kappa.mer(glmer_fpregres_in_i)
# 5.742707
vif.mer(glmer_fpregres_in_i)
# oral.comp_bct                decod.comp_bct                   gort.wpm.st                 sspan.corr.st 
# 1.939488                      1.787092                      1.563924                      1.871507 
# region_C                std_f_coca_log                      reglen_C            std_prevf_coca_log 
# 1.043379                      1.373965                      1.395260                      2.524851 
# prevwlen_C            std_nextf_coca_log                    nextwlen_C        oral.comp_bct:region_C 
# 2.514443                      2.363722                      2.324002                      1.831385 
# decod.comp_bct:region_C          gort.wpm.st:region_C        sspan.corr.st:region_C  oral.comp_bct:std_f_coca_log 
# 1.835361                      1.608014                      1.817551                      2.386144 
# decod.comp_bct:std_f_coca_log    gort.wpm.st:std_f_coca_log  sspan.corr.st:std_f_coca_log        oral.comp_bct:reglen_C 
# 2.396324                      2.113648                      2.387793                      2.385228 
# decod.comp_bct:reglen_C          gort.wpm.st:reglen_C        sspan.corr.st:reglen_C 
# 2.419755                      2.136483                      2.376386 

# run the party implementation
cf_fpregres_in_i <- cforest(fpregres_in ~ oral.comp_bct + decod.comp_bct + gort.wpm.st + sspan.corr.st 
                         + region_C + std_f_coca_log + reglen_C + std_prevf_coca_log + prevwlen_C + std_nextf_coca_log + nextwlen_C
                         + oral.comp_bct:region_C + decod.comp_bct:region_C + gort.wpm.st:region_C + sspan.corr.st:region_C
                         + oral.comp_bct:std_f_coca_log + decod.comp_bct:std_f_coca_log + gort.wpm.st:std_f_coca_log + sspan.corr.st:std_f_coca_log
                         + oral.comp_bct:reglen_C + decod.comp_bct:reglen_C + gort.wpm.st:reglen_C + sspan.corr.st:reglen_C
                         , data=D1, control=cforest_unbiased(mtry=2,ntree=50))
cf_fpregres_in_i_varimp <- varimp(cf_fpregres_in_i)
cf_fpregres_in_i_varimp
# oral.comp_bct     decod.comp_bct        gort.wpm.st      sspan.corr.st           region_C     std_f_coca_log           reglen_C 
# 0.003287009        0.003416839        0.004051294        0.002692926        0.008679828        0.002712772        0.003395212 
# std_prevf_coca_log         prevwlen_C std_nextf_coca_log         nextwlen_C 
# 0.002613566        0.002892491        0.002836609        0.002642712 
png(file.path(resdir, "cf_fpregres_in_i_varimp.png"), width = 6, height = 6, units = 'in', res = 300)
g <- dotplot(sort(cf_fpregres_in_i_varimp), panel=function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=20, cex=1.1)
  panel.abline(v=abs(min(cf_fpregres_in_i_varimp)), col = 'red', lty='longdash', lwd=4)
  panel.abline(v=0, col='blue')
})
print(g)
dev.off()

r.squaredGLMM(glmer_fpregres_in_i_skill)
# R2m       R2c
# theoretical 0.0010650515 0.1627187
# delta       0.0007000298 0.1069507
r.squaredGLMM(glmer_fpregres_in_i_skillword)
# R2m      R2c
# theoretical 0.02724643 0.1594649
# delta       0.01788513 0.1046761
r.squaredGLMM(glmer_fpregres_in_i)
# R2m       R2c
# theoretical 0.02868907 0.1611423
# delta       0.01884503 0.1058498

# anova model selection
anova(glmer_fpregres_in_i, glmer_fpregres_in_i_skillword)
# npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)   
# glmer_fpregres_in_i_skillword   17 15744 15874 -7855.1    15710                     
# glmer_fpregres_in_i             29 15755 15975 -7848.3    15697 13.537 12     0.3313


# # get bootstrapping results
# r3 <- report(glmer_fpregres_in_i, bootstrap=TRUE, iterations = 500)
# table_long(r3)

# mySumm <- function(.) {
#   s <- getME(., "sigma")
#   c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
# }
# boo5 <- bootstrap(model = lmer_fpregres_in_i, fn = mySumm, type = "parametric", B = 500)
# requireNamespace("boot")
# boo5


# significant main effects: region_C, reglen_C, std_prevf_coca_log, prevwlen_C, std_nextf_coca_log
# significant interactions: NA
# draw significant main effects:
yRange <- c(0,0.5)
yrate <- 0.85
# region_C
X <- region; xlab <- region; Y <- fpregres_in; ylab <- fpregres_in; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# reglen_C
X <- reglen; xlab <- reglen; Y <- fpregres_in; ylab <- fpregres_in; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_prevf_coca_log
X <- prevf; xlab <- prevf; Y <- fpregres_in; ylab <- fpregres_in; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 2)

# prevwlen_C
X <- prevwlen; xlab <- prevwlen; Y <- fpregres_in; ylab <- fpregres_in; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 1)

# std_nextf_coca_log
X <- nextf; xlab <- nextf; Y <- fpregres_in; ylab <- fpregres_in; title <- paste(X, 'vs.', Y, sep=" ")
FileName <- file.path(resdir, paste(X, "_", Y, "_loessSeg", sep=""))
drawSegLoess(D1, X, Y, xlab, ylab, yRange, yrate, title, FileName, show, 2)



# contribution of decoding and oral knowledge to reading comprehension
a <- lm(readcomp.comp_bct ~ decod.comp_bct + oral.comp_bct, data=aggregate(D1, by=list(D1$subj), FUN=mean, na.rm=TRUE))
summary(a)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.52683 -0.28718  0.01531  0.31546  0.99300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.908e-17  7.901e-02   0.000   1.0000    
# decod.comp_bct 1.950e-01  9.669e-02   2.016   0.0503 .  
# oral.comp_bct  7.342e-01  9.669e-02   7.593 2.43e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5241 on 41 degrees of freedom
# Multiple R-squared:  0.7381,	Adjusted R-squared:  0.7253 
# F-statistic: 57.77 on 2 and 41 DF,  p-value: 1.182e-12

b <- lm(readcomp.comp_bct ~ decod.comp_bct * oral.comp_bct, data=aggregate(D1, by=list(D1$subj), FUN=mean, na.rm=TRUE))
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
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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

a1 <- lm(readcomp.comp_bct ~ oral.comp_bct, data=aggregate(D1, by=list(D1$subj), FUN=mean, na.rm=TRUE))
summary(a1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.30054 -0.34687  0.01795  0.34478  0.92797 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -4.760e-17  8.185e-02    0.00        1    
# oral.comp_bct  8.439e-01  8.279e-02   10.19 6.33e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5429 on 42 degrees of freedom
# Multiple R-squared:  0.7121,	Adjusted R-squared:  0.7053 
# F-statistic: 103.9 on 1 and 42 DF,  p-value: 6.331e-13

a2 <- lm(readcomp.comp_bct ~ decod.comp_bct, data=aggregate(D1, by=list(D1$subj), FUN=mean, na.rm=TRUE))
summary(a2)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6324 -0.4048  0.1899  0.4751  1.3917 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    2.311e-16  1.211e-01   0.000        1    
# decod.comp_bct 6.081e-01  1.225e-01   4.964  1.2e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8033 on 42 degrees of freedom
# Multiple R-squared:  0.3697,	Adjusted R-squared:  0.3547 
# F-statistic: 24.64 on 1 and 42 DF,  p-value: 1.197e-05

