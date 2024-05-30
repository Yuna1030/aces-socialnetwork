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



#1.1 importing data####
# NOT AVAILABLE



#1.2 inclusion + exclusion criteria ####
# NOT AVAILABLE
#
# WE EXCLUDED participants lacking data on ACEs in wave 1, 
# those without data of at least one measure of social network size and diversity across wave 1 to wave 3, 
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

#social network diversity/size histogram
for(i in 1:30) MAIN_wide[[i]] <- MAIN_wide[[i]] %>% mutate(logSNS = log(SNS+1))
plot_SNS <- ggplot(MAIN_wide[[1]], aes(x = logSNS)) +
  geom_histogram() +
  facet_wrap(~wave) +
  theme_classic() + xlab("Social network size (log)") + ylab("N")
plot_SND <- ggplot(MAIN_wide[[1]], aes(x = SND)) +
  geom_histogram() +
  facet_wrap(~wave) +
  theme_classic() + xlab("Social network diversity") + ylab("N")
png("Plot_SNSSND_hist.png", width = 15, height = 15, units = "cm", res = 600)
ggarrange(plot_SNS,plot_SND,ncol = 1,nrow = 2)
dev.off()



#2.2 analysis | ACEs -> social thinning ####
lmerModList(logSNS ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide) %>% Result_summary()
lmerModList(SND ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide) %>% Result_summary()

SNS.lmer = ggpredict(lmer(logSNS ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide[[1]]), term = c("month [all]", "ACE_cat"))
SND.lmer = ggpredict(lmer(SND ~ ACE_cat*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide[[1]]), term = c("month [all]", "ACE_cat"))
plot_SNS <- ggplot(MAIN_wide[[1]], aes(x = month , y = logSNS)) +
  geom_jitter(width = 0.5, height = 0.2, size = 0.8, alpha = 0.5, color = "darkgray") +
  geom_line(data = SNS.lmer, aes(x = x, y = predicted, color = group), size = 0.8)  +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Month", y = "Social network size (log)", color = "Number of ACEs") +
  theme_classic()
plot_SND <- ggplot(MAIN_wide[[1]], aes(x = month , y = SND)) +
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
for(i in 1:30) check[[i]] <- MAIN_wide[[i]] %>% subset(month==0)
lmerModList(k6_tot_w3 ~ ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()
lmerModList(k6_tot_w3 ~ ACE_cat + k6_tot_w1 + Age + HouseIncome + HouseEdu + (1|setai_id), data = check) %>% Result_summary()



#2.4 analysis | ACEs * social thinning -> depression ####
MAIN_wide_slope <- as.list(1:30)
for(i in 1:30){
  datslope <- MAIN_wide[[i]]
  data_slop_cbind <- MAIN_wide[[i]] %>% subset(select = c(id))
  
  for(j in c("SND","logSNS")){
    model <- paste0(j," ~ month + (1+month|id)")
    Results_1 <- lmer(model, data = datslope) 
    data_slope <- coef(Results_1)$id %>% 
      rownames_to_column(var = "id") %>% 
      subset(select = c("id","month","(Intercept)")) %>%
      dplyr::rename_with(~paste0(j,"_month"),"month") %>%
      dplyr::rename_with(~paste0(j,"_int"),"(Intercept)")
    
    data_slop_cbind <- merge(data_slop_cbind,data_slope,by = "id", all = T)
  }
  
  MAIN_wide_slope[[i]] <- MAIN_wide[[i]] %>%
    merge(data_slop_cbind, by = "id", all = T) %>% group_by(id) %>% sample_n(1) %>% ungroup()
}
lmerModList(k6_tot_w3 ~ logSNS_month*ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ logSNS_month*ACE_cat + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SND_month*ACE_cat + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SND_month*ACE_cat + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()



#2.5 analysis | Social thinning -> depression ####
lmerModList(k6_tot_w3 ~ logSNS_month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ logSNS_month + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()

lmerModList(k6_tot_w3 ~ SND_month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()
lmerModList(k6_tot_w3 ~ SND_month + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = MAIN_wide_slope) %>% Result_summary()



#2.6 analysis | depression -> change in social network ####
lmerModList(logSNS ~ k6_tot_w1*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide) %>% Result_summary()
lmerModList(SND ~ k6_tot_w1*month + Age + Sex + HouseIncome + HouseEdu + (1|setai_id/id), data = MAIN_wide) %>% Result_summary()



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
for(i in 1:30) UCORONA_mechanism[[i]] <- MAIN_wide_slope[[i]] %>%
  merge(MAIN_wide[[i]] %>% subset(select = c(id,SND_w1,SND_w2,SND_w3,SNS_w1,SNS_w2,SNS_w3)), 
        by = "id", all.x = T) %>%
  merge(UCORONA_all %>% subset(select = c(id,k6_tot_w2,
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
         logSNS_int = scale(logSNS_int) %>% as.numeric(),
         logCRP = scale(logCRP) %>% as.numeric(),
         logCRP2 = scale(logCRP2) %>% as.numeric(),
         SND_month = scale(SND_month) %>% as.numeric(),
         logSNS_month = scale(logSNS_month) %>% as.numeric(),
         NLR = scale(NLR) %>% as.numeric(),
         MLR = scale(MLR) %>% as.numeric(),
         SII = scale(SII) %>% as.numeric(),
         hypervigilance = scale(hypervigilance) %>% as.numeric()) 

#multilevel partial correlations (https://github.com/easystats/correlation) n = 436
for(i in 1:30) UCORONA_mechanism[[i]] = UCORONA_mechanism[[i]] %>% 
  subset(!is.na(ACE_n)&!is.na(k6_tot_w1)&!is.na(k6_tot_w3)&!is.na(SND_int)&!is.na(SND_month)&!is.na(logSNS_int)&!is.na(logSNS_month)&!is.na(hypervigilance)&!is.na(logCRP)&!is.na(NLR)&!is.na(MLR)&!is.na(SII))

listdata1 = list()
for(i in 1:30) {
  data_resi = data.frame(id = c(1:436))
  
  for(y in c("ACE_n","k6_tot_w1","k6_tot_w3","SND_int","SND_month","logSNS_int","logSNS_month","hypervigilance")){
    
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

df = data.frame()
for(x in c("ACE_n_resi","hypervigilance_resi","logCRP_resi","NLR_resi","MLR_resi","SII_resi","logSNS_int_resi","SND_int_resi","k6_tot_w1_resi","logSNS_month_resi","SND_month_resi","k6_tot_w3_resi")){
  for(y in c("ACE_n_resi","hypervigilance_resi","logCRP_resi","NLR_resi","MLR_resi","SII_resi","logSNS_int_resi","SND_int_resi","k6_tot_w1_resi","logSNS_month_resi","SND_month_resi","k6_tot_w3_resi")){
    
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

png("Plot_mechanism_result.png", width = 25, height = 20, units = "cm", res = 600)
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
                            "logSNS_int_resi" = "Social network size baseline",
                            "SND_int_resi" = "Social network diversity baseline",
                            "k6_tot_w1_resi" = "Psychological distress baseline",
                            "logSNS_month_resi" = "Social network size change",
                            "SND_month_resi" = "Social network diversity change",
                            "k6_tot_w3_resi" = "Psychological distress T3")) +
  scale_x_discrete(labels=c("ACE_n_resi" = "ACEs",
                            "hypervigilance_resi" = "Hypervigilance",
                            "logCRP_resi" = "CRP",
                            "NLR_resi" = "NLR",
                            "MLR_resi" = "MLR",
                            "SII_resi" = "SII",
                            "logSNS_int_resi" = "Social network size baseline",
                            "SND_int_resi" = "Social network diversity baseline",
                            "k6_tot_w1_resi" = "Psychological distress baseline",
                            "logSNS_month_resi" = "Social network size change",
                            "SND_month_resi" = "Social network diversity change",
                            "k6_tot_w3_resi" = "Psychological distress T3")) +
  geom_text(aes(label = V7)) +
  theme_classic() + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()



#3.2 hypothesis 1: association between ACEs, social thinning, and depression ####
SEM_data <- UCORONA_mechanism[[1]] %>% mutate(logSNS_w1 = log(SNS_w1 + 1),
                                              logSNS_w2 = log(SNS_w2 + 1),
                                              logSNS_w3 = log(SNS_w3 + 1))
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
k6_tot_w3 + logSNS_month + SND_month + logSNS_int + SND_int + hypervigilance + logCRP + ACE_n ~ Age + Sex + HouseIncome + HouseEdu
logCRP ~ Smk + BMI

k6_tot_w3 ~ logSNS_month + SND_month + logSNS_int + SND_int 
logSNS_month + SND_month + logSNS_int + SND_int ~ hypervigilance + logCRP
hypervigilance + logCRP ~ ACE_n

SND_month ~~ SND_int
logSNS_month ~~ logSNS_int
SND_month ~~ logSNS_month
SND_int ~~ logSNS_int

hypervigilance ~~ logCRP
'
sem(SEMmodel3, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)


SEMmodel4 <-'
k6_tot_w3 + logSNS_month + SND_month + logSNS_int + SND_int + hypervigilance + logCRP + ACE_n ~ Age + Sex + HouseIncome + HouseEdu
logCRP ~ Smk + BMI

k6_tot_w3 ~ hypervigilance + logCRP
hypervigilance + logCRP ~ logSNS_month + SND_month + logSNS_int + SND_int
logSNS_month + SND_month + logSNS_int + SND_int ~ ACE_n

SND_month ~~ SND_int
logSNS_month ~~ logSNS_int
SND_month ~~ logSNS_month
SND_int ~~ logSNS_int

hypervigilance ~~ logCRP
'
sem(SEMmodel4, data = SEM_data, estimator = "ML", missing = "fiml") %>% summary(fit.measures = T, standardized = T)


#interaction between ACEs and hypervigilance
lmerModList(logSNS_month ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()
lmerModList(SND_month ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()

lmerModList(k6_tot_w3 ~ ACE_cat*hypervigilance + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()
lmerModList(k6_tot_w3 ~ ACE_cat*hypervigilance + k6_tot_w1 + Age + Sex + HouseIncome + HouseEdu + (1|setai_id), data = UCORONA_mechanism) %>% Result_summary()


