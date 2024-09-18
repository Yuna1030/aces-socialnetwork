#0.1 loading packages####
#R version 4.2.3 (2023-03-15 ucrt) -- "Shortstop Beagle" (update April 6, 2023)
pacman::p_load(
  foreign,
  haven,
  sjlabelled,
  labelled,
  writexl,
  table1,
  
  psych,
  
  mice,
  miceadds,
  
  lme4,
  lmerTest,
  merTools,
  
  dplyr,
  
  ggplot2,
  ggExtra,
  ggpubr,
  ggeffects,
  
  reshape,
  tidyverse,
  
  lcmm,
  tidyLPA,
  
  nnet,
  
  lavaan
)

options(max.print = 1000000)



#0.2 versions####
sessionInfo()
# R version 4.3.2 (2023-10-31)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.5
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] lavaan_0.6-17    nnet_7.3-19      tidyLPA_1.1.0    lcmm_2.1.0       lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    purrr_1.0.2      readr_2.1.5     
# [10] tidyr_1.3.1      tibble_3.2.1     tidyverse_2.0.0  reshape_0.8.9    ggeffects_1.5.0  ggpubr_0.6.0     ggExtra_0.10.1   ggplot2_3.5.1    dplyr_1.1.4     
# [19] merTools_0.6.1   arm_1.13-1       MASS_7.3-60.0.1  lmerTest_3.1-3   lme4_1.1-35.1    Matrix_1.6-5     miceadds_3.17-44 mice_3.16.0      psych_2.4.1     
# [28] table1_1.4.3     writexl_1.4.2    labelled_2.12.0  sjlabelled_1.2.0 haven_2.5.4      foreign_0.8-86  
# 
# loaded via a namespace (and not attached):
#   [1] RColorBrewer_1.1-3    rstudioapi_0.15.0     shape_1.4.6.1         datawizard_0.9.1      magrittr_2.0.3        jomo_2.7-6            farver_2.1.2         
#   [8] nloptr_2.0.3          vctrs_0.6.5           minqa_1.2.6           rstatix_0.7.2         htmltools_0.5.8.1     broom_1.0.5           Formula_1.2-5        
#  [15] mitml_0.4-5           parallelly_1.37.1     gsubfn_0.7            plyr_1.8.9            mime_0.12             lifecycle_1.0.4       iterators_1.0.14     
#  [22] pkgconfig_2.0.3       R6_2.5.1              fastmap_1.2.0         future_1.33.2         shiny_1.8.0           snakecase_0.11.1      digest_0.6.36        
#  [29] numDeriv_2016.8-1.1   colorspace_2.1-0      furrr_0.3.1           labeling_0.4.3        fansi_1.0.6           timechange_0.3.0      httr_1.4.7           
#  [36] abind_1.4-5           compiler_4.3.2        withr_3.0.0           doParallel_1.0.17     pander_0.6.5          backports_1.5.0       carData_3.0-5        
#  [43] DBI_1.2.0             fastDummies_1.7.3     broom.mixed_0.2.9.4   ggsignif_0.6.4        pan_1.9               tools_4.3.2           pbivnorm_0.6.0       
#  [50] MplusAutomation_1.1.1 httpuv_1.6.14         glue_1.7.0            quadprog_1.5-8        nlme_3.1-164          promises_1.2.1        grid_4.3.2           
#  [57] checkmate_2.3.1       generics_0.1.3        gtable_0.3.5          tzdb_0.4.0            data.table_1.15.4     hms_1.1.3             car_3.1-2            
#  [64] utf8_1.2.4            foreach_1.5.2         pillar_1.9.0          later_1.3.2           mitools_2.4           splines_4.3.2         lattice_0.22-5       
#  [71] survival_3.5-7        tidyselect_1.2.1      miniUI_0.1.1.1        knitr_1.48            blme_1.0-5            stats4_4.3.2          xfun_0.45            
#  [78] texreg_1.39.3         proto_1.0.0           stringi_1.8.4         pacman_0.5.1          boot_1.3-28.1         codetools_0.2-19      cli_3.6.3            
#  [85] rpart_4.1.23          xtable_1.8-4          munsell_0.5.1         Rcpp_1.0.12           globals_0.16.3        coda_0.19-4           marqLevAlg_2.0.8     
#  [92] rngWELL_0.10-9        parallel_4.3.2        ellipsis_0.3.2        randtoolbox_2.0.4     mclust_6.0.1          listenv_0.9.1         glmnet_4.1-8         
#  [99] mvtnorm_1.2-4         scales_1.3.0          insight_0.19.8        crayon_1.5.3          rlang_1.1.4           mnormt_2.1.1         



#1.1 importing data####
# NOT AVAILABLE



#1.2 inclusion + exclusion criteria ####
# NOT AVAILABLE
#
# WE EXCLUDED participants lacking data on ACEs in wave 1, 
# those without data of at least one measure of social network size and diversity across waves 1 to 3, 
# and those without data of psychological distress in wave 3. 



#1.3 imputing missing in covaraites####
# PARTIALLY AVAILABLE
#
# DATASET NAME LIST
# UCORONA_imp = name of original dataset to be imputed, including the following variables
# - subject ID, household ID, total number of ACEs, social network diversity at waves 1-3, social network size at waves 1-3, 
# - psychological distress at waves 1-3, sex, household income, household education, smoking, height, weight
# imp = imputed dataset

propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)*100))
propmiss(UCORONA_imp) 

inlist <- c("age","sex") 
outlist <- c("subjectID")
pred <- quickpred(UCORONA_imp, include = inlist, exclude = outlist) 
table(rowSums(pred)) # a number of predictors of 15-25 is about right; current 5 to 12
imp <- mice(UCORONA_imp,m = 30, print = T, maxit = 25, pred = pred, seed = 777)
imp$loggedEvents 
plot(imp) 



#1.4 merging data ####
# PARTIALLY AVAILABLE
# By merging with the original dataset including data of ACEs, social network diversity, social network size, and psychological distress at wave 3,
# we only imputed the missing covariates for the analytical dataset.
#
# DATASET NAME LIST
# UCORONA_small = original dataset
# MAIN_wide = analytical dataset wide version
# MAIN_long = analytical dataset long version
#
# VARIABLES NAME LIST
# id: subject ID
# setai_id: household ID
# incl_id: 0=excluded from the analysis, 1=included in the analysis
# ACE_n: total number of ACEs
# ACE_cat: 0, 1, 2+
# SND_w1: social network diversity at wave 1
# SND_w2: social network diversity at wave 2
# SND_w3: social network diversity at wave 3
# SNS_w1: social network size at wave 1
# SNS_w2: social network size at wave 2
# SNS_w3: social network size at wave 3
# k6_tot_w1: psychological distress at wave 1
# k6_tot_w2: psychological distress at wave 2
# K6_tot_w3: psychological distress at wave 3
# Age: age
# Sex: sex
# HouseIncome: household income
# HouseEdu: household education


#merging with original dataset
MAIN_wide <- as.list(1:30)
for(i in 1:30) MAIN_wide[[i]] <- imp[[i]] %>% merge(UCORONA_small %>% subset(select = c(XXX-dataname)), by = "id", all.y = T)

#wide to long
MAIN_long <- as.list(1:30)
for(i in 1:30) MAIN_long[[i]] <- MAIN_wide[[i]] %>%
  tidyr::gather("SND_w1","SND_w2","SND_w3","SNS_w1","SNS_w2","SNS_w3",key = variables, value = values) %>%
  tidyr::separate_wider_delim(variables, "_", names = c("SocialNetwork", "wave")) %>%
  tidyr::spread(SocialNetwork, values) %>%
  mutate(month = ifelse(wave=="w1",0,ifelse(wave=="w2",4,ifelse(wave=="w3",13,NA)))) 



#2.0 demographics ####
table1(~ + Age + as.factor(Sex) + HouseIncome + HouseEdu + ACE_n + ACE_cat | incl_id, data = UCORONA_small)



#2.1 analysis | ACEs -> social network diversity & size ####
Result_summary <- function(x){
  #extract effect size and SE from data list
  # x = data list obtained from lmerModList function
  var <- summary(x)$fe[,1] %>% as.character()
  eff <- summary(x)$fe[,"estimate"] %>% as.numeric()
  SE <- summary(x)$fe[,"std.error"] %>% as.numeric()
  df <- summary(x)$fe[,"df"] %>% as.numeric()
  
  #calculate necessary estimates
  Wald = eff/SE # t-statistics
  p = 2*pt(-abs(Wald),df=df) #p-value
  LCI = eff - abs((qt(.025, df=df)*SE)) #Lower bound of 95% CI 
  UCI = eff + abs((qt(.025, df=df)*SE)) #upper bound of 95% CI
  
  Result1 <- data.frame(eff, LCI, UCI, p) #combine all data
  Result1 <- round(Result1, 4)
  Result1 <- data.frame(var,Result1)
  rownames(Result1) <- Result1[,1]
  Result1 }
Result_summary_lm <- function(x){
  #extract effect size and SE from data list
  # x = data list obtained from "gee" analysis
  imp.res_Eff <- sapply(x, function(x) summary(x)$coefficient[,"Estimate"])
  imp.res_SE <- sapply(x, function(x) summary(x)$coefficient[,"Std. Error"])
  
  #calculate necessary estimates
  eff = rowMeans(imp.res_Eff) #pooled effect size with Rubin's rule
  Vw = rowMeans(imp.res_SE^2) #within-variance
  Vb = (rowSums((imp.res_Eff-eff)^2))/(ncol(imp.res_Eff)-1) #between-variance
  SE = sqrt(Vw + Vb + Vb/ncol(imp.res_Eff)) #pooled SE with Rubin's rule
  Wald = eff/SE # t-statistics
  lambda = (Vb + Vb/ncol(imp.res_Eff))/(SE^2) #fraction of missing information
  if (sum(lambda)!=0){
    DfOld = (ncol(imp.res_Eff)-1)/(lambda^2) #degree of freedom with old method
    n = nobs(x[[1]]) #sample size in the analysis
    k = nrow(imp.res_Eff) #number of parameters in the analysis
    DfObs = ((n-k+1)*(n-k)*(1-lambda))/(n-k+3) #degree of freedom of observation
    DfAdj = (DfOld*DfObs)/(DfOld + DfObs) #adjusted degree of freedom
    p = 2*pt(-abs(Wald),df=DfAdj) #p-value
    LCI = eff - abs((qt(.025, df=DfAdj)*SE)) #Lower bound of 95% CI 
    UCI = eff + abs((qt(.025, df=DfAdj)*SE)) #upper bound of 95% CI
    
    Result1 <- data.frame(eff, SE, p, LCI, UCI) #combine all data
    Result1 <- round(Result1, 4)
  } else {
    n = nobs(x[[1]]) #sample size in the analysis
    k = nrow(imp.res_Eff) #number of parameters in the analysis
    DfObs = ((n-k+1)*(n-k)*(1-lambda))/(n-k+3) #degree of freedom of observation
    p = 2*pt(-abs(Wald),df=DfObs) #p-value
    LCI = eff - abs((qt(.025, df=DfObs)*SE)) #Lower bound of 95% CI 
    UCI = eff + abs((qt(.025, df=DfObs)*SE)) #upper bound of 95% CI
    
    Result1 <- data.frame(eff, SE, p, LCI, UCI) #combine all data
    Result1 <- round(Result1, 4)
  }
  
  Result1
  
}

#social network diversity/size histogram
for(i in 1:30) MAIN_long[[i]] <- MAIN_long[[i]] %>% mutate(logSNS = log(SNS+1, base = 10))
plot_SNS <- ggplot(MAIN_long[[1]], aes(x = logSNS)) +
  geom_histogram() +
  facet_wrap(~wave) +
  theme_classic() + 
  scale_x_continuous(labels = scales::label_math()) + xlab("Social network size") + ylab("N")
plot_SND <- ggplot(MAIN_long[[1]], aes(x = SND)) +
  geom_histogram() +
  facet_wrap(~wave) +
  theme_classic() + xlab("Social network diversity") + ylab("N")
png("Plot_SNSSND_hist.png", width = 15, height = 15, units = "cm", res = 600)
ggarrange(plot_SNS,plot_SND,ncol = 1,nrow = 2)
dev.off()

#ACE distribution
png("Plot_ACE_hist.png", width = 7, height = 7, units = "cm", res = 600)
ggplot(MAIN_long[[1]], aes(x = ACE_n)) +
  geom_histogram() +
  theme_classic() + xlab("ACE scores") + ylab("N")
dev.off()



#2.2 analysis | ACEs -> social thinning ####
lapply(MAIN_long,
       FUN = function(x) glm.nb(SNS ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long) %>% Result_summary()

SNS.lmer = ggpredict(lmer(logSNS ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long[[1]]), term = c("month [all]", "ACE_cat"))
SND.lmer = ggpredict(lmer(SND ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long[[1]]), term = c("month [all]", "ACE_cat"))
plot_SNS <- ggplot(MAIN_long[[1]], aes(x = month , y = logSNS)) +
  geom_jitter(width = 0.5, height = 0.2, size = 0.8, alpha = 0.5, color = "darkgray") +
  geom_line(data = SNS.lmer, aes(x = x, y = predicted, color = group), size = 0.8)  +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Month", y = "Social network size", color = "Number of ACEs") +
  scale_y_continuous(labels = scales::label_math()) +
  theme_classic()
plot_SND <- ggplot(MAIN_long[[1]], aes(x = month , y = SND)) +
  geom_jitter(width = 0.5, height = 0.4, size = 0.8, alpha = 0.5, color = "darkgray") +
  geom_line(data = SND.lmer, aes(x = x, y = predicted, color = group), size = 0.8)  +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Month", y = "Social network diversity", color = "Number of ACEs") +
  theme_classic()
png("Plot_SNSSND_pred.png", width = 20, height = 10, units = "cm", res = 600)
ggarrange(plot_SNS,plot_SND, common.legend = T, legend = "bottom", labels = c("A", "B"),ncol = 2,nrow = 1)
dev.off()



#2.3 analysis | ACEs -> depression ####
check <- as.list(1:30)
for(i in 1:30) check[[i]] <- MAIN_long[[i]] %>% subset(month==0)
lmerModList(k6_tot_w3 ~ ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()
lmerModList(k6_tot_w3 ~ ACE_cat + k6_tot_w1 + Age + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()



#2.4 analysis | ACEs * social thinning -> depression ####
MAIN_long_slope <- as.list(1:30)
for(i in 1:30){
  datslope <- MAIN_long[[i]]
  data_slop_cbind <- MAIN_long[[i]] %>% subset(select = c(id))
  
  for(j in c("SND","SNS")){
    model <- paste0(j," ~ month + (1+month|id)")
    if (j == "SND")
      Results_1 <- lmer(model, data = datslope) 
    else Results_1 <- glmer.nb(model, data = datslope)
    data_slope <- coef(Results_1)$id %>% 
      rownames_to_column(var = "id") %>% 
      subset(select = c("id","month","(Intercept)")) %>%
      dplyr::rename_with(~paste0(j,"_month"),"month") %>%
      dplyr::rename_with(~paste0(j,"_int"),"(Intercept)")
    
    data_slop_cbind <- merge(data_slop_cbind,data_slope,by = "id", all = T)
  }
  
  MAIN_long_slope[[i]] <- MAIN_long[[i]] %>%
    merge(data_slop_cbind, by = "id", all = T) %>% group_by(id) %>% sample_n(1) %>% ungroup()
}
lmerModList(k6_tot_w3 ~ SNS_month*ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SNS_month*ACE_cat + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SND_month*ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SND_month*ACE_cat + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()



#2.5 analysis | Social thinning -> depression ####
lmerModList(k6_tot_w3 ~ SNS_month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SNS_month + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SND_month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SND_month + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()



#2.6 analysis | depression -> change in social network ####
lapply(MAIN_long,
       FUN = function(x) glmer.nb(SNS ~ k6_tot_w1*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ k6_tot_w1*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long) %>% Result_summary()



#3.1 posthoc analysis ####
#potential outliers from visualization, lymph, neutrophil, wbc, alb, need to be transformed CRP -> after exclusion there is no outlier in logCRP, NLR, MLR, SII
# VARIABLES NAME LIST
# CRP_w1: highly sensitive CRP at wave 1
# CRP_w2: highly sensitive CRP at wave 2
# WBC_w1: WBC concentration at wave 1
# Neut_w1: Neutrophil level at wave 1
# Plt_w1: Platelet level at wave 1
# Lymph_w1: Lymphocyte level at wave 1
# Mono_w1: Monocyte level at wave 1
# Hypervigilance1-8: response to inquiries about hypervigilance regarding COVID-19
# Smk: smoking habit

findoutlier <- function(x) return(x < quantile(x, .25, na.rm = T) - 3*IQR(x, na.rm = T) | x > quantile(x, .75, na.rm = T) + 3*IQR(x, na.rm = T))

UCORONA_mechanism <- as.list(1:30)
for(i in 1:30) UCORONA_mechanism[[i]] <- MAIN_long_slope[[i]] %>%
  merge(MAIN_wide[[i]] %>% subset(select = c(id,SND_w1,SND_w2,SND_w3,SNS_w1,SNS_w2,SNS_w3)), 
        by = "id", all.x = T) %>%
  merge(UCORONA_small %>% subset(select = c(id,k6_tot_w2,
                                          CRP_w1,CRP_w2,WBC_w1,Neut_w1,Plt_w1,Lymph_w1,Mono_w1,
                                          Hypervigilance1,Hypervigilance2,Hypervigilance3,Hypervigilance4,
                                          Hypervigilance5,Hypervigilance6,Hypervigilance7,Hypervigilance8)), by = "id", all.x = T) %>%
  mutate(logCRP = ifelse(CRP_w1<10000,log(CRP_w1),NA), #since <50 changed to 50, 50-inflation
         logCRP2 = ifelse(CRP_w2<10000,log(CRP_w2),NA), 
         outlier_lymph = ifelse(findoutlier(Lymph_w1), id, NA),
         outlier_neut = ifelse(findoutlier(Neut_w1), id, NA),
         outlier_mono = ifelse(findoutlier(Mono_w1), id, NA),
         outlier_plt = ifelse(findoutlier(Plt_w1), id, NA),
         NLR = ifelse(is.na(outlier_lymph)&is.na(outlier_neut),I(Neut_w1/Lymph_w1),NA),
         MLR = ifelse(is.na(outlier_lymph)&is.na(outlier_mono),I(Mono_w1/Lymph_w1),NA),
         SII = ifelse(is.na(outlier_lymph)&is.na(outlier_neut)&is.na(outlier_plt),I(Plt_w1*Neut_w1/Lymph_w1),NA),
         BMI = weight_w1/((height_w1/100)^2),
         Hypervigilance1_scale = Hypervigilance1 %>% scale() %>% as.numeric(),
         Hypervigilance2_scale = Hypervigilance2 %>% scale() %>% as.numeric(),
         Hypervigilance3_scale = Hypervigilance3 %>% scale() %>% as.numeric(),
         Hypervigilance4_scale = Hypervigilance4 %>% scale() %>% as.numeric(),
         Hypervigilance5_scale = Hypervigilance5 %>% scale() %>% as.numeric(),
         Hypervigilance6_scale = Hypervigilance6 %>% scale() %>% as.numeric(),
         Hypervigilance7_scale = Hypervigilance7 %>% scale() %>% as.numeric(),
         Hypervigilance8_scale = Hypervigilance8 %>% scale() %>% as.numeric()) %>%
  rowwise(id) %>%
  mutate(hypervigilance = sum(Hypervigilance1_scale,Hypervigilance2_scale,Hypervigilance3_scale,Hypervigilance4_scale,Hypervigilance5_scale,Hypervigilance6_scale,Hypervigilance7_scale,Hypervigilance8_scale,na.rm = T)) %>%
  ungroup() %>% #high hypervigilance score indicating higher levels of hypervigilance
  mutate(ACE_n = scale(ACE_n) %>% as.numeric(),
         k6_tot_w1 = scale(k6_tot_w1) %>% as.numeric(),
         k6_tot_w3 = scale(k6_tot_w3) %>% as.numeric(),
         SND_int = scale(SND_int) %>% as.numeric(),
         SNS_int = scale(SNS_int) %>% as.numeric(),
         logCRP = scale(logCRP) %>% as.numeric(),
         logCRP2 = scale(logCRP2) %>% as.numeric(),
         SND_month = scale(SND_month) %>% as.numeric(),
         SNS_month = scale(SNS_month) %>% as.numeric(),
         NLR = scale(NLR) %>% as.numeric(),
         MLR = scale(MLR) %>% as.numeric(),
         SII = scale(SII) %>% as.numeric(),
         hypervigilance = scale(hypervigilance) %>% as.numeric()) 

#multilevel partial correlations (https://github.com/easystats/correlation) n = 436
for(i in 1:30) UCORONA_mechanism[[i]] = UCORONA_mechanism[[i]] %>% 
  subset(!is.na(ACE_n)&!is.na(k6_tot_w1)&!is.na(k6_tot_w3)&!is.na(SND_int)&!is.na(SND_month)&!is.na(SNS_int)&!is.na(SNS_month)&!is.na(hypervigilance)&!is.na(logCRP)&!is.na(NLR)&!is.na(MLR)&!is.na(SII))

listdata1 = list()
for(i in 1:30) {
  data_resi = data.frame(id = c(1:436))
  
  for(y in c("ACE_n","k6_tot_w1","k6_tot_w3","SND_int","SND_month","SNS_int","SNS_month","hypervigilance")){
    
    model <- paste0(y," ~ Age + Sex + HouseIncome + HouseEdu + (1|setai_id)")
    Results_1 <- lmer(model, data = UCORONA_mechanism[[i]]) 
    data_resi1 <- residuals(Results_1) %>% as.data.frame() %>% 
      rownames_to_column(var = "id") %>% 
      dplyr::rename("resi" = ".") %>%
      mutate(resi = resi %>% scale() %>% as.numeric()) %>%
      dplyr::rename_with(~paste0(y,"_resi"),"resi")
    data_resi <- merge(data_resi,data_resi1,by = "id", all = T)
  }
  listdata1[[i]] = data_resi 
}
listdata2 = list()
for(i in 1:30) {
  data_resi = data.frame(id = c(1:436))
  
  for(y in c("logCRP","NLR","MLR","SII")){
    
    model <- paste0(y," ~ Age + Sex + HouseIncome + HouseEdu + Smk + BMI + (1|setai_id)")
    Results_1 <- lmer(model, data = UCORONA_mechanism[[i]]) 
    data_resi1 <- residuals(Results_1) %>% as.data.frame() %>% 
      rownames_to_column(var = "id") %>% 
      dplyr::rename("resi" = ".") %>%
      mutate(resi = resi %>% scale() %>% as.numeric()) %>%
      dplyr::rename_with(~paste0(y,"_resi"),"resi")
    data_resi <- merge(data_resi,data_resi1,by = "id", all = T)
  }
  listdata2[[i]] = data_resi 
}
listdata = list()
for(i in 1:30) listdata[[i]] = merge(listdata1[[i]], listdata2[[i]], by = "id", all = T) %>% subset(select = -c(id))


df = data.frame()
for(x in c("ACE_n_resi","hypervigilance_resi","logCRP_resi","NLR_resi","MLR_resi","SII_resi","SNS_int_resi","SND_int_resi","k6_tot_w1_resi","SNS_month_resi","SND_month_resi","k6_tot_w3_resi")){
  for(y in c("ACE_n_resi","hypervigilance_resi","logCRP_resi","NLR_resi","MLR_resi","SII_resi","SNS_int_resi","SND_int_resi","k6_tot_w1_resi","SNS_month_resi","SND_month_resi","k6_tot_w3_resi")){
    
    if (x == y) {
      outcomes <- cbind(x,y,NA,NA,NA,NA,NA)
    } else {
      #making datalist for each regression model
      Model <- (paste0(y, " ~ ",x))
      #making summary statistics for each model
      Results <- lapply(listdata, function(j) lm(Model, data = j)) %>% Result_summary_lm()
      #taking out outcome values from each summary
      outcomes <- cbind(x,y,Results[x,"eff"],Results[x,"LCI"],Results[x,"UCI"],Results[x,"p"],ifelse(Results[x,"p"]<0.05,"*",NA))
    }
    df = rbind(df,outcomes)
    df}}

png("Plot_mechanism_result.png", width = 20, height = 16, units = "cm", res = 600)
df %>% 
  mutate(V3 = V3 %>% as.numeric(), V4 = V4 %>% as.numeric(), V5 = V5 %>% as.numeric(), V6 = V6 %>% as.numeric()) %>%
  rowwise() %>%
  mutate(pair = sort(c(x, y)) %>% paste(collapse = ",")) %>%
  group_by(pair) %>%
  distinct(pair, .keep_all = T) %>% ungroup() %>%
  ggplot(aes(x = factor(x, levels=unique(x)), y = factor(y, levels = unique(y)), fill = V3)) +
  geom_raster() + 
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Coeffiecients")+ 
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  scale_y_discrete(labels=c("ACE_n_resi" = "ACEs",
                            "hypervigilance_resi" = "Hypervigilance",
                            "logCRP_resi" = "CRP",
                            "NLR_resi" = "NLR",
                            "MLR_resi" = "MLR",
                            "SII_resi" = "SII",
                            "SNS_int_resi" = "Social network size baseline",
                            "SND_int_resi" = "Social network diversity baseline",
                            "k6_tot_w1_resi" = "Psychological distress baseline",
                            "SNS_month_resi" = "Social network size change",
                            "SND_month_resi" = "Social network diversity change",
                            "k6_tot_w3_resi" = "Psychological distress T3")) +
  scale_x_discrete(labels=c("ACE_n_resi" = "ACEs",
                            "hypervigilance_resi" = "Hypervigilance",
                            "logCRP_resi" = "CRP",
                            "NLR_resi" = "NLR",
                            "MLR_resi" = "MLR",
                            "SII_resi" = "SII",
                            "SNS_int_resi" = "Social network size baseline",
                            "SND_int_resi" = "Social network diversity baseline",
                            "k6_tot_w1_resi" = "Psychological distress baseline",
                            "SNS_month_resi" = "Social network size change",
                            "SND_month_resi" = "Social network diversity change",
                            "k6_tot_w3_resi" = "Psychological distress T3")) +
  geom_text(aes(label = V7)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13))
dev.off()



#3.2 hypothesis 1: association between ACEs, social thinning, and depression ####
SEM_data <- UCORONA_mechanism[[1]] %>% mutate(logSNS_w1 = log(SNS_w1 + 1, base = 10),
                                              logSNS_w2 = log(SNS_w2 + 1, base = 10),
                                              logSNS_w3 = log(SNS_w3 + 1, base = 10))
SEMmodel1 <- '
logSNS_w1 + k6_tot_w1 + logSNS_w2 + k6_tot_w2 + logSNS_w3 + k6_tot_w3 ~ ACE_n + Age + Sex + HouseIncome + HouseEdu
logSNS_w2 + k6_tot_w2 ~ logSNS_w1 + k6_tot_w1
logSNS_w3 + k6_tot_w3 ~ logSNS_w2 + k6_tot_w2
logSNS_w1~~k6_tot_w1
logSNS_w2~~k6_tot_w2
logSNS_w3~~k6_tot_w3'
sem(SEMmodel1, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)

model.RICLPM1 <- '
        # Create between components (random intercepts)
        RIx =~ 1*logSNS_w1 + 1*logSNS_w2 + 1*logSNS_w3 #where 1* fixes the factor loading to one.
        RIy =~ 1*k6_tot_w1 + 1*k6_tot_w2 + 1*k6_tot_w3 
       
       # Create within-person centered variables
        wx1 =~ 1*logSNS_w1
        wx2 =~ 1*logSNS_w2
        wx3 =~ 1*logSNS_w3
        wy1 =~ 1*k6_tot_w1
        wy2 =~ 1*k6_tot_w2
        wy3 =~ 1*k6_tot_w3
        
        RIx + RIy ~ Age + Sex + HouseIncome + HouseEdu
        wx1 + wy1 + wx2 + wy2 + wx3 + wy3 ~ ACE_n
          # Constrained over time. 
        
        # Estimate lagged effects between within-person centered variables
        wx2 + wy2 ~ wx1 + wy1 
        wx3 + wy3 ~ wx2 + wy2  
        
        # Estimate covariance between within-person centered variables at first wave
        wx1 ~~ wy1 # Covariance
        
        # Estimate covariances between residuals of within-person centered variables
        # (i.e., innovations)
        wx2 ~~ wy2
        wx3 ~~ wy3
        
        # Estimate variance and covariance of random intercepts
        RIx ~~ RIx
        RIy ~~ RIy
        RIx ~~ RIy
        
        # Estimate (residual) variance of within-person centered variables
        wx1 ~~ wx1 # Variances
        wy1 ~~ wy1
        wx2 ~~ wx2 # Residual variances
        wy2 ~~ wy2
        wx3 ~~ wx3
        wy3 ~~ wy3
'
lavaan(model.RICLPM1, data = SEM_data, missing = "ML", meanstructure = T, int.ov.free = T) %>%
  #meanstructure: If TRUE, the means of the observed variables enter the model. If "default", the value is set based on the user-specified model, and/or the values of other arguments.
  #int.ov.free: If FALSE, the intercepts of the observed variables are fixed to zero.
  summary(standardized = T, fit.measures = T)


SEMmodel2 <- '
SND_w1 + k6_tot_w1 + SND_w2 + k6_tot_w2 + SND_w3 + k6_tot_w3 ~ ACE_n + Age + Sex + HouseIncome + HouseEdu
SND_w2 + k6_tot_w2 ~ SND_w1 + k6_tot_w1
SND_w3 + k6_tot_w3 ~ SND_w2 + k6_tot_w2 
SND_w1~~k6_tot_w1
SND_w2~~k6_tot_w2
SND_w3~~k6_tot_w3'
sem(SEMmodel2, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)

model.RICLPM2 <- '
        # Create between components (random intercepts)
        RIx =~ 1*SND_w1 + 1*SND_w2 + 1*SND_w3 #where 1* fixes the factor loading to one.
        RIy =~ 1*k6_tot_w1 + 1*k6_tot_w2 + 1*k6_tot_w3 
        
        # Create within-person centered variables
        wx1 =~ 1*SND_w1
        wx2 =~ 1*SND_w2
        wx3 =~ 1*SND_w3
        wy1 =~ 1*k6_tot_w1
        wy2 =~ 1*k6_tot_w2
        wy3 =~ 1*k6_tot_w3
        
        RIx + RIy ~ Age + Sex + HouseIncome + HouseEdu
        wx1 + wy1 + wx2 + wy2 + wx3 + wy3 ~ ACE_n
        
        # Estimate lagged effects between within-person centered variables
        wx2 + wy2 ~ wx1 + wy1
        wx3 + wy3 ~ wx2 + wy2
        
        # Estimate covariance between within-person centered variables at first wave
        wx1 ~~ wy1 # Covariance
        
        # Estimate covariances between residuals of within-person centered variables
        # (i.e., innovations)
        wx2 ~~ wy2
        wx3 ~~ wy3
        
        # Estimate variance and covariance of random intercepts
        RIx ~~ RIx
        RIy ~~ RIy
        RIx ~~ RIy
        
        # Estimate (residual) variance of within-person centered variables
        wx1 ~~ wx1 # Variances
        wy1 ~~ wy1
        wx2 ~~ wx2 # Residual variances
        wy2 ~~ wy2
        wx3 ~~ wx3
        wy3 ~~ wy3
'
lavaan(model.RICLPM2, data = SEM_data, missing = "ML", meanstructure = T, int.ov.free = T) %>%
  summary(standardized = T, fit.measures = T)



#3.3 hypothesis 2: including inflammation and hypervigilance ####
SEMmodel3 <-'
k6_tot_w3 + SNS_month + SND_month + SNS_int + SND_int + hypervigilance + logCRP + ACE_n ~ Age + Sex + HouseIncome + HouseEdu
logCRP ~ Smk + BMI

k6_tot_w3 ~ SNS_month + SND_month + SNS_int + SND_int 
SNS_month + SND_month + SNS_int + SND_int ~ hypervigilance + logCRP
hypervigilance + logCRP ~ ACE_n

SND_month ~~ SND_int
SNS_month ~~ SNS_int
SND_month ~~ SNS_month
SND_int ~~ SNS_int

hypervigilance ~~ logCRP
'
sem(SEMmodel3, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)


SEMmodel4 <-'
k6_tot_w3 + SNS_month + SND_month + SNS_int + SND_int + hypervigilance + logCRP + ACE_n ~ Age + Sex + HouseIncome + HouseEdu
logCRP ~ Smk + BMI

k6_tot_w3 ~ hypervigilance + logCRP
hypervigilance + logCRP ~ SNS_month + SND_month + SNS_int + SND_int
SNS_month + SND_month + SNS_int + SND_int ~ ACE_n

SND_month ~~ SND_int
SNS_month ~~ SNS_int
SND_month ~~ SNS_month
SND_int ~~ SNS_int

hypervigilance ~~ logCRP
'
sem(SEMmodel4, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)


#interaction between ACEs and hypervigilance
lmerModList(SNS_month ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()
lmerModList(SND_month ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()

lmerModList(k6_tot_w3 ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()
lmerModList(k6_tot_w3 ~ ACE_cat*hypervigilance + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()



#4.1 sensitivity analysis ####
# ACEs continuous scores
lapply(MAIN_long,
       FUN = function(x) glm.nb(SNS ~ ACE_n*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ ACE_n*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long) %>% Result_summary()

lmerModList(k6_tot_w3 ~ ACE_n + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()
lmerModList(k6_tot_w3 ~ ACE_n + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SNS_month*ACE_n + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SNS_month*ACE_n + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SND_month*ACE_n + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SND_month*ACE_n + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_long_slope) %>% Result_summary()


# log10-transformed SNS
lmerModList(logSNS ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long) %>% Result_summary()


# dimensional approach
MAIN_long_added = list()
for(i in 1:30){
  MAIN_long_added[[i]] =
    MAIN_long[[i]] %>% 
    merge(UCORONA_small %>% dplyr::select(ACE_Pmental,ACE_Palc,ACE_Pviobw,ACE_PhyAb,ACE_negl,ACE_EmoAb,ACE_divorce,ACE_death,ACE_Poverty,ACE_bullied,ACE_SexAb,ACE_hospital,ACE_disaster, id), by = "id", all.x = T) %>%
    rowwise() %>%
    mutate(Family = sum(ACE_Pmental,ACE_Palc,ACE_Pviobw,ACE_divorce,ACE_death),
           maltre = sum(ACE_PhyAb,ACE_negl,ACE_EmoAb,ACE_SexAb),
           other = sum(ACE_Poverty,ACE_bullied,ACE_hopital,ACE_disaster)) %>%
    ungroup()
}

lapply(MAIN_long_added,
       FUN = function(x) glm.nb(SNS ~ Family*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ Family*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long_added) %>% Result_summary()

lapply(MAIN_long_added,
       FUN = function(x) glm.nb(SNS ~ maltre*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ maltre*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long_added) %>% Result_summary()

lapply(MAIN_long_added,
       FUN = function(x) glm.nb(SNS ~ other*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = x)) %>% 
  Result_summary_lm()
lmerModList(SND ~ other*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_long_added) %>% Result_summary()

