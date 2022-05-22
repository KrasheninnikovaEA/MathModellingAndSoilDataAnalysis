library(tidyverse)
library(stringr)
library(lubridate)
eddy=read_csv("eddypro.csv", skip=1, na=c(" ","NA","-9999","-9999.0"), comment=c("[")); 
eddy=eddy[-1,]
eddy=select(eddy,-(roll))
eddy=eddy %>% mutate_if(is.character, factor) %>% mutate(month = month(date))

names(eddy) = names(eddy) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_slash_") %>%
  str_replace_all("[%]","_pecent_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(eddy)

eddy_num=filter(eddy, month==5, daytime==TRUE)[,sapply(eddy, is.numeric)]

cor_td=cor(drop_na(eddy_num)) %>% as.data.frame %>% select(co2_flux)
cor_td=cbind(cor_td,adjusted=sqrt(cor_td$co2_flux^2))
vars=row.names(cor_td)[cor_td$adjusted>.1] %>% na.exclude()
correlation_formula=as.formula(paste("co2_flux~",paste(vars,collapse="+"),sep=" "))

model1=lm(correlation_formula,data=eddy_num)

names(model1)

summary(model1)

anova(model1)

vars_cut=c("co2_var","w_slash_ts_cov",
           "co2...125")
correlation_formula_cut=as.formula(paste("co2_flux~",paste(vars_cut,collapse="+"),sep=" "))

modelc=lm(correlation_formula_cut,data=eddy_num)
anova(modelc)
summary(modelc)

cor_lrm=cor(drop_na(eddy_num) %>% as.data.frame %>% select(vars_cut))
cor_lrm=sqrt(cor_lrm^2)