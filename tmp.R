

format_county_str <- function(x){
  tmp1 <- str_split(x,",")[[1]][1]
  return (str_remove(tmp1," County"))
}

library(stringr)
library(dplyr)
##
confirmed <- read.csv("/Users/gcgibson/Downloads/covid_us_county.csv")
confirmed_nm <- confirmed[confirmed$state == "New Mexico",]
num_counties  <- length(unique(confirmed_nm$county))
num_times <- nrow(confirmed_nm)/num_counties
confirmed_nm <- confirmed_nm %>% group_by(county) %>% mutate(gr=c(0,cases[2:length(cases)]/cases[1:(length(cases)-1)]))

confirmed_nm[is.nan(confirmed_nm$gr) ,]$gr <- 0

confirmed_nm[is.infinite(confirmed_nm$gr) ,]$gr <- 0


##manually add in population density
## source: https://www.nmhealth.org/publication/view/report/4442/


pop_density <- c(575.3,.5,10.8,
                 6.0,3.5,35.6,.8,55.8,13.1,7.4,
                 1.5,.3,1.4,15.2,4.2,164.5,
                 8.4,13.4,2.4,9.9,3.1,6.8,
                 8.2,36.4,23,6.1,76.6,2.8,
                 2.6,14.9,4.8,1.2,71.6,0,0)
confirmed_nm$pop_density <- rep(pop_density,each=num_times)

## food access
## source: https://www.nmhealth.org/publication/view/report/4442/

food_access <- c(14.9,18.7,14.8,19.8,16.3,18.0,16.0,15.5,
                 13.1,15.3,9.4,16.7,14.9,13.3,15.4,12.7,
                 20.7,26.9,11.4,17.9,16.8,13.2,18.8,
                 14.2,19.6,14.6,12.4,19.0,16.4,15.0,
                 18.5,14.1,13.4,0,0)
confirmed_nm$food_access <- rep(food_access,each=num_times)

## access to healthcare

## source : https://www.nmhealth.org/publication/view/report/4442/

access_p <- c(70,63,70,70,74,63,78,70,70,74,NA,80,63,57,74,70,57,57,70,74,
              70,74,63,70,63,74,70,63,70,70,70,66,70,NA,NA)
confirmed_nm$access_p <- rep(access_p,each=num_times)

confirmed_nm$t <- rep(1:num_times,num_counties)





##### general covariate data
format_county_str <- function(x){
  tmp1 <- str_split(x,",")[[1]][1]
  return (str_remove(tmp1," County"))
}


covariate_data <- readxl::read_xls("/Users/gcgibson/Downloads/ACS_17_5YR_DP02_Selected_Social_Characteristics.xls",col_names = F)
covariate_data_t <- data.frame(t(covariate_data))
covariate_data_t$county <- covariate_data_t$X3
covariate_data_t$living_with_senior <- covariate_data_t$X20
covariate_data_t_subset <- covariate_data_t[colnames(covariate_data_t) %in% c("county","living_with_senior")]
covariate_data_t_subset <- covariate_data_t_subset[complete.cases(covariate_data_t_subset),]
#covariate_data_t_subset <- covariate_data_t_subset[3:nrow(covariate_data_t_subset),]
covariate_data_t_subset$county  <- unlist(lapply(covariate_data_t_subset$county,format_county_str))

confirmed_nm <- confirmed_nm %>% left_join(covariate_data_t_subset,by="county")
confirmed_nm$living_with_senior <- as.numeric(gsub(",", "", confirmed_nm$living_with_senior))

### County population size
populations <- rep(c(677692,3539,65459,26978,12353,50199,2060,215338,
  57437,28061,NA,459,4371,70126,19482,18356,24264,
  72849,4563,65745,8373,39307,19117,140769,
  127455,28034,148917,11135,17000,32888,15595,
  4175,75956,NA,NA),each=num_times)
confirmed_nm$pop <- populations


confirmed_nm <- confirmed_nm %>% group_by(county) %>% mutate(living_with_senior_normalized = living_with_senior/pop)


library(ggplot2)

confirmed_nm <- confirmed_nm %>% group_by(county) %>% mutate(normalized_gr=log(cases)/pop)
log_case_plot <- ggplot(confirmed_nm[confirmed_nm$county %in% c("Taos","Santa Fe","Bernalillo","McKinley"),],aes(x=t,y=normalized_gr,col=county)) +  geom_line() + facet_wrap(~county) + theme_bw()
ggsave("log_case_plot.png",log_case_plot,device = "png",height = 4,width = 6)

living_with_senior_plot <- ggplot(confirmed_nm[confirmed_nm$county %in% c("Taos","Santa Fe","Bernalillo","McKinley"),],aes(x=county,y=living_with_senior_normalized,size=4)) +  geom_point() + theme_bw() + ylab("Living With Senior") + xlab("County")
ggsave("living_with_senior_plot.png",living_with_senior_plot,device = "png",height = 4,width = 6)


pop_density_plot <- ggplot(confirmed_nm[confirmed_nm$county %in% c("Taos","Santa Fe","Bernalillo","McKinley"),],aes(x=county,y=pop_density,size=4)) +  geom_point() + theme_bw() + ylab("Population Density") + xlab("County")
ggsave("pop_density_plot.png",pop_density_plot,device = "png",height = 4,width = 6)





food_access_plot <- ggplot(confirmed_nm[confirmed_nm$county %in% c("Taos","Santa Fe","Bernalillo","McKinley"),],aes(x=county,y=food_access,size=4)) +  geom_point() + theme_bw() + ylab("Food Insecurity") + xlab("County")
ggsave("food_access_plot.png",food_access_plot,device = "png",height = 4,width = 6)


healthcare_access_plot <- ggplot(confirmed_nm[confirmed_nm$county %in% c("Taos","Santa Fe","Bernalillo","McKinley"),],aes(x=county,y=access_p,size=4)) +  geom_point() + theme_bw() + ylab("Healthcare access") + xlab("County")
ggsave("healthcare_access_plot.png",healthcare_access_plot,device = "png",height = 4,width = 6)

confirmed_nm[is.infinite(confirmed_nm$normalized_gr),]$normalized_gr <- 0
confirmed_nm[is.na(confirmed_nm$normalized_gr),]$normalized_gr <- 0

#### model 1
library(lme4)
library(sjPlot)

basic_fit <- lmer(normalized_gr~t+I(t^2) + (1|county),data=confirmed_nm)
p <-sjPlot::plot_model(basic_fit,type = "re") 
p$t+ geom_line(aes(x=t,y=normalized_gr),data=confirmed_nm) + facet_grid(~county)
summary(basic_fit)



fit_w_covariates <- lmer(normalized_gr~t+I(t^2) + pop_density + (1|county),data=confirmed_nm)
summary(fit_w_covariates)

fit_w_covariates_2 <- lmer(normalized_gr~t+I(t^2) + pop_density + food_access+living_with_senior_normalized+access_p+ (1|county),data=confirmed_nm)
summary(fit_w_covariates_2)

confirmed_nm_complete <- confirmed_nm[colnames(confirmed_nm) %in% c("t","pop_density","food_access","living_with_senior_normalized","access_p","county", "normalized_gr")]
confirmed_nm_complete <- confirmed_nm_complete[complete.cases(confirmed_nm_complete),]
### linear moddes
null_model <-lm(normalized_gr~t+I(t^2),data=confirmed_nm_complete)
summary(null_model)
lm_fit <-lm(normalized_gr~t+I(t^2) + pop_density + food_access+living_with_senior_normalized+access_p+ county,data=confirmed_nm_complete)
summary(lm_fit)
stargazer::stargazer(lm_fit,type="latex")

anova_res <- anova(null_model,lm_fit,test="Chisq")

stargazer::stargazer(anova_res,type="latex")


######
library(forecast)
xreg_cols <- c("pop_density","food_access","living_with_senior_normalized","access_p")
xreg_mat <- scale(as.matrix(sapply(confirmed_nm_complete[,colnames(confirmed_nm_complete)%in%xreg_cols], as.numeric)))

ar_fit <-auto.arima(confirmed_nm_complete$normalized_gr*1e6,xreg=xreg_mat)
summary(ar_fit)
