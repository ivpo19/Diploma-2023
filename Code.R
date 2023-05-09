##### ТЕХНИЧЕСКАЯ ЗОНА #####
library(tidyverse)
library(dplyr)
library(janitor)
library(DT)
library(scales)
library(DescTools)
library(readr)
library(tidyr)
library(survival)
library(survminer)
library(writexl)
library(readxl)
library(ggplot2)
library(ggfortify)
library(LongCART)
library(rpart)
library(gt)
library(gbm)
library(pROC)
library(PHInfiniteEstimates)
library(Greg)
library(Rcpp)
library(coin)
library(timereg)
library(ciTools)
library(knitr)
library(stargazer)
library(MASS)
library(boot.pval)

#Загрузка данных
Data1 <- read_xlsx("DATA_1.xlsx")
Data1_alt <- read_xlsx("DATA_1_alt.xlsx")
df_summary <- read_xlsx("graph_data.xlsx")
debut_data <- read_xlsx("debut_data.xlsx")
exit_data <- read_xlsx("exit_data.xlsx")



# Разведочный анализ данных -----------------------------------------------

#Самые результативные пилоты 
df_summary %>% dplyr::select(1, 9, 2:8) %>% arrange(desc(PpR)) %>% filter(PpR >= 8.45) %>% 
  filter(Races > 50) %>% gt() %>% fmt_number(columns = c(Place, PpR), decimals = 2)

#Пилоты с наибольшим количеством гонок
df_summary %>% dplyr::select(1, 9, 2:8) %>% arrange(desc(Races)) %>% filter(Races >= 247) %>% 
  gt() %>% fmt_number(columns = c(Place, PpR), decimals = 2)

#Топ-10 по победам
ggplot(data = (df_summary %>% filter(Wins >=22)) , mapping=aes(x = Wins, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral")  + xlab("Количество побед") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Топ-10 по победам, относительные величины
ggplot(data = (df_summary %>% filter(Wins >=22)) , mapping=aes(x = WpR, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral") + xlab("Отношение побед к гонкам") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Топ-10 по поулам
ggplot(data = (df_summary %>% filter(Poles >=22)) , mapping=aes(x = Poles, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral") + xlab("Количество поулов") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Топ-10 по поулам, относительные величины
ggplot(data = (df_summary %>% filter(Poles >=22)) , mapping=aes(x = PpR, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral") + xlab("Отношение поулов к гонкам") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Многократные чемпионы, титулы
ggplot(data = (df_summary %>% filter(Titles >= 2)), mapping=aes(x = Titles, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral") + xlab("Количество титулов") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Многократные чемпионы, относительные величины
ggplot(data = (Data1 %>% filter(Titles >= 2) %>% filter(driverRef != "lauda")), mapping = aes(x = titleps, y = driverRef, fill = driverRef)) + 
  geom_bar(stat="Identity") + scale_fill_brewer(palette="Spectral")+ xlab("Отношение титулов к сезонам") +
  ylab("Пилот") + guides(fill="none") + theme(axis.title = element_text(size = 14))


#Распределение возрастов дебюта по эпохам
ggplot(data = debut_data, aes(x = age)) + geom_bar() + facet_wrap(vars(decada)) + xlab("Возраст") +
  ylab("Количество") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Распределение возрастов окончания карьеры по эпохам
ggplot(data = exit_data, aes(x = age)) + geom_bar() + facet_wrap(vars(decada)) + xlab("Возраст") +
  ylab("Количество") + guides(fill="none") + theme(axis.title = element_text(size = 14))

#Посмотрим на числа
debut_data %>% group_by(decada) %>% summarise(debut = mean(age))
exit_data %>% group_by(decada) %>% summarise(exit = mean(age))
exit_data %>% group_by(decada) %>% summarise(career = mean(sum_seasons))
exit_data %>% group_by(decada) %>% summarise(career = mean(sum_races))

#####По командам#####


# Каплан-Майер ---------------------------------------------------------------------


#Выживаемость в целом
km_fit1 <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~1)
summary(km_fit1)
autoplot(km_fit1)

ggsurvplot(km_fit1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)


km_fit1a <- survfit(Surv(Data1_alt$races_for_team, Data1_alt$last_alt)~1)
summary(km_fit1a)
autoplot(km_fit1a)


#по эпохам
km_trt_eras <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$eighties +
                         Data1_alt$nineties +
                         Data1_alt$nulls +
                         Data1_alt$tenths)
autoplot(km_trt_eras) 

ggsurvplot(km_trt_eras,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317", "#69d7cd", "#f2a93b"),
           data = Data1_alt)

km_trt_eras <- survfit(Surv(Data1_alt$races_for_team, Data1_alt$last_alt)~Data1_alt$eighties +
                         Data1_alt$nineties +
                         Data1_alt$nulls +
                         Data1_alt$tenths)
autoplot(km_trt_eras)


#Титулы
km_trt_titles <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anytitles)
autoplot(km_trt_titles) 

ggsurvplot(km_trt_titles,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

#log-rank тест
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anytitles, data = Data1_alt)
#Тест Вилькоксона 
gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anytitles, Data1_alt, gehan = TRUE)



#Победы
km_trt_wins <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anywins)
autoplot(km_trt_wins) 
#Тест Вилькоксона 
gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anywins, Data1_alt, gehan = TRUE)


#Поулы
km_trt_poles <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypoles)
autoplot(km_trt_poles) 

ggsurvplot(km_trt_poles,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)
#Тест Вилькоксона
gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypoles, Data1_alt, gehan = TRUE)


#Подиумы
km_trt_podiums <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypodiums)
autoplot(km_trt_podiums) 

#другая визуализация
ggsurvplot(km_trt_podiums,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

#log-rank тест
surv_diff <- survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypodiums, data = Data1_alt)

#тест Вилькоксона
gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypodiums, Data1_alt, gehan = TRUE)


#Очки
km_trt_points <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypoints)
autoplot(km_trt_points) 

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$anypoints, Data1_alt, gehan = TRUE)



#Историческая команда
km_trt_history <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$History)
autoplot(km_trt_history)

#другая визуализация
ggsurvplot(km_trt_history,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$History, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$History, data = Data1_alt)


#Заводская команда
km_trt_factory <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Factory)
autoplot(km_trt_factory)

#другая визуализация
ggsurvplot(km_trt_factory,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Factory, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Factory, data = Data1_alt)


#Чемпион младших серий
km_trt_F2 <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$F2_champ)
autoplot(km_trt_F2)

#другая визуализация
ggsurvplot(km_trt_F2,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$F2_champ, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$F2_champ, data = Data1_alt)


#Спортивная династия 
km_trt_family <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Family_business)
autoplot(km_trt_family)

#другая визуализация
ggsurvplot(km_trt_family,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Family_business, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$Family_business, data = Data1_alt)


#Совпадение гражданства 
km_trt_nation <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$driver_team_nation)
autoplot(km_trt_nation)

#другая визуализация
ggsurvplot(km_trt_nation,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$driver_team_nation, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$driver_team_nation, data = Data1_alt)



#Брак
km_trt_married <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$married)
autoplot(km_trt_married)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$married, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$married, data = Data1_alt)


#Дети 
km_trt_kids <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$children)
autoplot(km_trt_kids)

ggsurvplot(km_trt_kids,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$children, Data1_alt, gehan = TRUE)
survdiff(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$children, data = Data1_alt)



#Быстрее партнера
km_trt_partner <- survfit(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$beatpartner)
autoplot(km_trt_partner)

gehan.wilcoxon.test(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt)~Data1_alt$beatpartner, Data1_alt, gehan = TRUE)

ggsurvplot(km_trt_partner,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1_alt)




# Cox ---------------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------
#Модель Кокса
Cox_mod <- coxph(Surv(sum_seasons_alt, last_alt) ~ age + 
                   winpr + pointpr + 
                   titleps + children +  gainedpr + 
                   mean_share_of_points_team, data = Data1_alt, robust = TRUE)

summary(Cox_mod)
extractAIC(Cox_mod)

#Воспользуемся критерием Акаике
Cox_mod_AIC <- stepAIC(coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ + Family_business +
                               Factory + History + driver_team_nation + age + winpr + pointpr + polepr + 
                               bestlappr + podiumpr + titleps + married + children +  gainedpr + 
                               driver_mist_team + mean_share_of_points_team + transfers + 
                               nineties + nulls + tenths, data = Data1_alt, robust = TRUE))

summary(Cox_mod_AIC)
extractAIC(Cox_mod_AIC)

stargazer(Cox_mod, Cox_mod_AIC,   
          title="Длинная и короткая модели", 
          column.labels=c("Короткая", "Длинная"), 
          df=FALSE, digits=3, type = "html", out = "1.htm")

stargazer(Cox_mod_AIC, align=TRUE,
          title = "Модель №1",
          apply.coef = exp, p.auto = FALSE, 
          t.auto = FALSE, digits = 3, report=('vc*p'), type = "html", out = "2.htm")

#Сравним два метода оценки: Эфрон и Бреслоу
Cox_mod_efron <- coxph(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                         winpr + pointpr + 
                         titleps + children +  gainedpr + 
                         mean_share_of_points_team, data = Data1_alt, ties = "efron", robust = TRUE)

summary(Cox_mod_efron)
extractAIC(Cox_mod_efron)

Cox_mod_breslow <- coxph(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                           winpr + pointpr + 
                           titleps + children +  gainedpr + 
                           mean_share_of_points_team, data = Data1_alt, ties = "breslow", robust = TRUE)

summary(Cox_mod_breslow)
extractAIC(Cox_mod_breslow)



#Проверка мультиколлинеарности
mod <- lm(sum_seasons_alt ~ driver_team_nation + age + 
            winpr + pointpr +
            titleps + children +  gainedpr + 
            mean_share_of_points_team, data = Data1_alt)
VIF(mod)


#Проверка пропорциональности рисков
cox.zph(Cox_mod_AIC) 

#Проверка на наличие выбросов 
ggcoxdiagnostics(Cox_mod_AIC, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

Cox_mod_boot <- coxph(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                        winpr + pointpr + 
                        titleps + children +  gainedpr + 
                        mean_share_of_points_team, data = Data1_alt, 
                      ties = "efron", robust = TRUE, model = TRUE)
set.seed(154)
censboot_summary(Cox_mod_boot, type = "perc", sim = "ordinary", strata = NULL,
                 coef = "exp", conf.level = 0.95, R = 999, pval_precision = NULL, adjust.method = "none")
set.seed(NULL)

# Бинарные переменные, оценка в сезонах -----------------------------------

#Модель Кокса
Cox_mod <- stepAIC(coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                           Family_business + Factory + married + children +
                           History + driver_team_nation + age + anywins + anypoints + anypoles + 
                           anypodiums + anytitles + beatpartner + transfers + 
                           nineties + nulls + tenths, data = Data1_alt, robust = TRUE))

summary(Cox_mod)
extractAIC(Cox_mod)

stargazer(Cox_mod, align=TRUE,
          title = "Модель №2",
          apply.coef = exp, p.auto = FALSE, 
          t.auto = FALSE, digits = 3, report=('vc*p'), type = "html", out = "2.htm")

Cox_mod_breslow <- coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                           children +
                           History + driver_team_nation + age + anywins + anypoints + anypoles + 
                           anypodiums + beatpartner + transfers + tenths, data = Data1_alt, ties = "breslow", robust = TRUE)

summary(Cox_mod_breslow)
extractAIC(Cox_mod_breslow)



Cox_mod_efron <- coxph(Surv(Data1_alt$sum_seasons_alt, Data1_alt$last_alt) ~ F2_champ +
                         children +
                         History + driver_team_nation + age + anywins + anypoints + anypoles + 
                         anypodiums + beatpartner + transfers + tenths, data = Data1_alt, ties = "efron", robust = TRUE)

summary(Cox_mod_efron)
extractAIC(Cox_mod_efron)



#Проверка мультиколлинеарности
mod <- lm(sum_seasons_alt ~ F2_champ + children +
            History + driver_team_nation + age + anywins + anypoints + anypoles + 
            anypodiums + beatpartner + transfers + tenths, data = Data1_alt)
VIF(mod)


#Проверка пропорциональности рисков
cox.zph(Cox_mod)


#Проверка на наличие выбросов 
ggcoxdiagnostics(Cox_mod, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())


Cox_mod_boot <- coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ + children +
                        History + driver_team_nation + age + anywins + anypoints + anypoles + 
                        anypodiums + beatpartner + transfers + tenths, data = Data1_alt, 
                      ties = "efron", robust = TRUE, model = TRUE)

#p-val с помощью бутстрапа 
set.seed(5)
censboot_summary(Cox_mod_boot, type = "perc", sim = "ordinary", strata = NULL,
                 coef = "exp", conf.level = 0.95, R = 999, pval_precision = NULL, adjust.method = "none")



# Аален -------------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------

Cox_mod <- coxph(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                   winpr + pointpr + 
                   titleps + children +  gainedpr + 
                   mean_share_of_points_team, data = Data1_alt, robust = TRUE)

summary(Cox_mod)

#Модель Аалена
Aalen_mod <- aareg(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                     winpr + pointpr + 
                     titleps + children +  gainedpr + 
                     mean_share_of_points_team, data = Data1_alt)

#Посмотрим значимость переменных в зависимости от отрезка времени
summary(Aalen_mod)
summary(Aalen_mod, maxtime = 1)
summary(Aalen_mod, maxtime = 2)
summary(Aalen_mod, maxtime = 3)
summary(Aalen_mod, maxtime = 4)
summary(Aalen_mod, maxtime = 5)
summary(Aalen_mod, maxtime = 6)
summary(Aalen_mod, maxtime = 7)

autoplot(Aalen_mod)
     
# Бинарные переменные, оценка в сезонах -----------------------------------
Cox_mod <- coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                   History + driver_team_nation + age + anywins + anypoints + anypoles + 
                   anypodiums + children + 
                   beatpartner + transfers +
                   tenths, data = Data1_alt)

summary(Cox_mod)

#Модель Аалена
Aalen_mod <- aareg(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                     History + driver_team_nation + age + anywins + anypoints + anypoles + 
                     anypodiums + children + 
                     beatpartner + transfers +
                     tenths, data = Data1_alt)

summary(Aalen_mod)
summary(Aalen_mod, maxtime = 1)
summary(Aalen_mod, maxtime = 2)
summary(Aalen_mod, maxtime = 3)
summary(Aalen_mod, maxtime = 4)
summary(Aalen_mod, maxtime = 5)
summary(Aalen_mod, maxtime = 6)
summary(Aalen_mod, maxtime = 7)

autoplot(Aalen_mod)


# Кокс-Аален --------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------

Cox_mod <- coxph(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                   winpr + pointpr + 
                   titleps + children +  gainedpr + 
                   mean_share_of_points_team, data = Data1_alt, robust = TRUE)

summary(Cox_mod)

#Проверка пропорциональности рисков 
cox.zph(Cox_mod)

Aalen_mod <- aareg(Surv(sum_seasons_alt, last_alt) ~ driver_team_nation + age + 
                     winpr + pointpr + 
                     titleps + children +  gainedpr + 
                     mean_share_of_points_team, data = Data1_alt)

autoplot(Aalen_mod)

#Модель Кокса-Аалена
set.seed(211)
Cox_aalen_mod <- cox.aalen(Surv(sum_seasons_alt, last_alt) ~ prop(driver_team_nation) + age + winpr + pointpr + 
                             prop(titleps) + children +  prop(gainedpr) + 
                             prop(mean_share_of_points_team), data = Data1_alt, n.sim = 1000)

summary(Cox_aalen_mod)
par(mfrow=c(2,3))
plot(Cox_aalen_mod)



# Бинарные переменные, оценка в сезонах -----------------------------------
Cox_mod <- coxph(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                   History + driver_team_nation + age + anywins + anypoints + anypoles + 
                   anypodiums + children + 
                   beatpartner + transfers +
                   tenths, data = Data1_alt)
summary(Cox_mod)

#Проверка пропорциональности рисков
cox.zph(Cox_mod)


Aalen_mod <- aareg(Surv(sum_seasons_alt, last_alt) ~ F2_champ +
                     History + driver_team_nation + age + anywins + anypoints + anypoles + 
                     anypodiums + children + 
                     beatpartner + transfers +
                     tenths, data = Data1_alt)
Aalen_mod
autoplot(Aalen_mod)

#Модель Кокса-Аалена
set.seed(142)
Cox_aalen_mod <- cox.aalen(Surv(sum_seasons_alt,last_alt) ~ prop(F2_champ) +
                             History + driver_team_nation + age + anywins + anypoints + anypoles + 
                             anypodiums + children + 
                             beatpartner + transfers +
                             tenths, data = Data1_alt, n.sim = 1000)

summary(Cox_aalen_mod)
par(mfrow=c(3,4))
plot(Cox_aalen_mod)











######По карьере#####

# К-М ---------------------------------------------------------------------


#Общая выживаемость по сезонам
km_fit1 <- survfit(Surv(Data1$sum_seasons, Data1$last)~1)
summary(km_fit1)


autoplot(km_fit1, xlab = "Сезоны", ylab = "Вероятность выживания")

ggsurvplot(km_fit1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854"),
           data = Data1)

#Общая выживаемость по гонкам
km_fit1a <- survfit(Surv(Data1$sum_races, Data1$last)~1)
summary(km_fit1a)
autoplot(km_fit1a, xlab = "Гонки", ylab = "Вероятность выживания")


#По эрам
km_trt_eras <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$eighties +
                         Data1$nineties +
                         Data1$nulls +
                         Data1$tenths)

autoplot(km_trt_eras, xlab = "Сезоны", ylab = "Вероятность выживания")


km_trt_eras <- survfit(Surv(Data1$sum_races, Data1$last)~Data1$eighties +
                         Data1$nineties +
                         Data1$nulls +
                         Data1$tenths)

autoplot(km_trt_eras, xlab = "Гонки", ylab = "Вероятность выживания")

#Титулы
km_trt_titles <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$anytitles)
autoplot(km_trt_titles) 

ggsurvplot(km_trt_titles,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1)

#Тест Вилькоксона
gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$anytitles, Data1, gehan = TRUE)


#Победы
km_trt_wins <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$anywins)
autoplot(km_trt_wins) 

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$anywins, Data1, gehan = TRUE)


#Поулы
km_trt_poles <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$anypoles)
autoplot(km_trt_poles) 

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$anypoles, Data1, gehan = TRUE)


#Подиумы
km_trt_podiums <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$anypodiums)
autoplot(km_trt_podiums) 

#другая визуализация
ggsurvplot(km_trt_podiums,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1)

#log-rank тест
survdiff(Surv(Data1$sum_seasons, Data1$last)~Data1$anypodiums, data = Data1)

#тест Вилькоксона
gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$anypodiums, Data1, gehan = TRUE)


#Очки
km_trt_points <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$anypoints)
autoplot(km_trt_points) 

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$anypoints, Data1, gehan = TRUE)


#Историческая команда
km_trt_history <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$History)
autoplot(km_trt_history)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$History, Data1, gehan = TRUE)


#Заводская команда
km_trt_factory <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$Factory)
autoplot(km_trt_factory)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$Factory, Data1, gehan = TRUE)


#Чемпион младших серий
km_trt_F2 <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$F2_champ)
autoplot(km_trt_F2)

#другая визуализация
ggsurvplot(km_trt_F2,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$F2_champ, Data1, gehan = TRUE)
#log-rank тест
survdiff(Surv(Data1$sum_seasons, Data1$last)~Data1$F2_GP2_F3000_champ, data = Data1)


#Спортивная династия 
km_trt_family <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$Family_business)
autoplot(km_trt_family)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$Family_business, Data1, gehan = TRUE)


#Совпадение гражданства
km_trt_nation <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$driver_team_nation)
autoplot(km_trt_nation)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$driver_team_nation, Data1, gehan = TRUE)


#Брак
km_trt_married <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$married)
autoplot(km_trt_married)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$married, Data1, gehan = TRUE)

#Дети 
km_trt_kids <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$children)
autoplot(km_trt_kids)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$children, Data1, gehan = TRUE)


#Быстрее партнера
km_trt_partner <- survfit(Surv(Data1$sum_seasons, Data1$last)~Data1$beatpartner)
autoplot(km_trt_partner)

#другая визуализация
ggsurvplot(km_trt_partner,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#111854", "#a62317"),
           data = Data1)

gehan.wilcoxon.test(Surv(Data1$sum_seasons, Data1$last)~Data1$beatpartner, Data1, gehan = TRUE)




# Cox ---------------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------
#Модель Кокса
Cox_mod_AIC <- stepAIC(coxph(Surv(sum_seasons, last) ~ F2_champ + Family_business +
                               age + winpr + pointpr + polepr + winstopoles3 +
                               bestlappr + podiumpr + titleps + married + children +  gainedpr + 
                               driver_mist + mean_share_of_points + 
                               nineties + nulls + tenths, data = Data1, robust = TRUE))

summary(Cox_mod_AIC)
extractAIC(Cox_mod_AIC)

Cox_mod <- coxph(Surv(sum_seasons, last) ~ F2_champ +
                   age + winpr + pointpr + 
                   nineties + nulls + tenths, data = Data1, robust = TRUE)
summary(Cox_mod)


stargazer(Cox_mod, align=TRUE,
          title = "Модель №3",
          p.auto = FALSE, 
          t.auto = FALSE, digits = 3, type = "html", out = "12.htm")

stargazer(Cox_mod, align=TRUE,
          title = "Модель №3.1",
          apply.coef = exp, p.auto = FALSE, 
          t.auto = FALSE, digits = 3, report=('vc*p'), type = "html", out = "14.htm")


#Проверка мультиколлинеарности
mod <- lm(sum_seasons ~ F2_champ +
            age + winpr + pointpr +
            nineties + nulls + tenths, data = Data1)
VIF(mod)


#Проверка пропорциональности рисков
cox.zph(Cox_mod)
ggcoxzph(cox.zph(Cox_mod))


#Проверка на наличие выбросов
ggcoxdiagnostics(Cox_mod, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#Сравним два метода оценки: Эфрон и Бреслоу
Cox_mod_efron <- coxph(Surv(sum_seasons, last) ~ F2_champ +
                         age + winpr + pointpr +
                         nineties + nulls + tenths, data = Data1, ties = "efron", robust = TRUE)

summary(Cox_mod_efron)
extractAIC(Cox_mod_efron)

Cox_mod_breslow <- coxph(Surv(sum_seasons, last) ~ F2_champ +
                           age + winpr + pointpr +
                           nineties + nulls + tenths, data = Data1, ties = "breslow", robust = TRUE)

summary(Cox_mod_breslow)
extractAIC(Cox_mod_breslow)



Cox_mod_boot <- coxph(Surv(sum_seasons, last) ~ F2_champ +
                        age + winpr + pointpr +
                        nineties + nulls + tenths, data = Data1, 
                      ties = "efron", robust = TRUE, model = TRUE)

#Подсчет p-val с помощью бутстрапа 
set.seed(77)
censboot_summary(Cox_mod_boot, type = "perc", sim = "ordinary", strata = NULL,
                 coef = "exp", conf.level = 0.95, R = 999, pval_precision = NULL, adjust.method = "none")





# Бинарные переменные, оценка в сезонах -----------------------------------
#Модель Кокса
Cox_mod <- stepAIC(coxph(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                           Family_business + married + children +
                           age + anywins + anypoints + anypoles + 
                           anypodiums + anytitles + beatpartner +
                           nineties + nulls + tenths, data = Data1, robust = TRUE))

summary(Cox_mod)
extractAIC(Cox_mod)

stargazer(Cox_mod, align=TRUE,
          title = "Модель №4",
          p.auto = FALSE, 
          t.auto = FALSE, digits = 3, type = "html", out = "22.htm")

#Проверка мультиколлинеарности
mod <- lm(sum_seasons ~ F2_champ +
            age + anypoints + anypoles + 
            anypodiums + nineties +
            nulls + tenths, data = Data1)
VIF(mod)


#Проверка пропорциональности рисков
cox.zph(Cox_mod)
ggcoxzph(cox.zph(Cox_mod))


#Проверка на наличие выбросов 
ggcoxdiagnostics(Cox_mod, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

stargazer(Cox_mod_boot, align=TRUE,
          title = "Модель №4.1",
          apply.coef = exp, p.auto = FALSE, 
          t.auto = FALSE, digits = 3, report=('vc*p'), type = "html", out = "224.htm")


Cox_mod_boot <- coxph(Surv(sum_seasons, last) ~ F2_champ +
                        age + anypoints + anypoles + 
                        anypodiums + nineties +
                        nulls + tenths, data = Data1, 
                      ties = "efron", robust = TRUE, model = TRUE)
summary(Cox_mod_boot)

set.seed(5)
censboot_summary(Cox_mod_boot, type = "perc", sim = "ordinary", strata = NULL,
                 coef = "exp", conf.level = 0.95, R = 999, pval_precision = NULL, adjust.method = "none")



# Аален -------------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------
Cox_mod <- coxph(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                   age + winpr + pointpr + 
                   nineties + nulls + tenths, data = Data1, robust = TRUE)


summary(Cox_mod)

#Модель Аалена
Aalen_mod <- aareg(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                     age + winpr + pointpr + 
                     nineties + nulls + tenths, data = Data1)
summary(Aalen_mod)

autoplot(Aalen_mod)

# Бинарные переменные, оценка в сезонах -----------------------------------
Cox_mod <- coxph(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                   age + anypoints + anypoles + 
                   anypodiums + nineties +
                   nulls + tenths, data = Data1)

summary(Cox_mod)

#Модель Аалена
Aalen_mod <- aareg(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                     age + anypoints + anypoles + 
                     anypodiums + nineties +
                     nulls + tenths, data = Data1)

summary(Aalen_mod)

autoplot(Aalen_mod)




# Кокс-Аален --------------------------------------------------------------


# Континуальные переменные, оценка в сезонах ------------------------------
Cox_mod <- coxph(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                   age + winpr + pointpr + 
                   nineties + nulls + tenths, data = Data1, robust = TRUE)


summary(Cox_mod)

cox.zph(Cox_mod)

Aalen_mod <- aareg(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                     age + winpr + pointpr + 
                     nineties + nulls + tenths, data = Data1)
summary(Aalen_mod)
autoplot(Aalen_mod)

#Модель Кокса-Аалена
set.seed(33)
Cox_aalen_mod <- cox.aalen(Surv(sum_seasons, last) ~ prop(F2_champ) +
                             age + winpr + pointpr + 
                             nineties + nulls + tenths, data = Data1, n.sim = 1000)

summary(Cox_aalen_mod)
par(mfrow=c(3,3))
plot(Cox_aalen_mod)



# Бинарные переменные,  оценка в сезонах ----------------------------------
Cox_mod <- coxph(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                   age + anypoints + anypoles + 
                   anypodiums + nineties +
                   nulls + tenths, data = Data1)

summary(Cox_mod)
cox.zph(Cox_mod)

Aalen_mod <- aareg(Surv(Data1$sum_seasons, Data1$last) ~ F2_champ +
                     age + anypoints + anypoles + 
                     anypodiums + nineties +
                     nulls + tenths, data = Data1)

summary(Aalen_mod)
autoplot(Aalen_mod)

#Модель Кокса-Аалена
set.seed(33)
Cox_aalen_mod <- cox.aalen(Surv(sum_seasons, last) ~ prop(F2_champ) +
                             age + anypoints + anypoles + 
                             anypodiums + nineties + nulls + tenths, data = Data1, n.sim = 1000)

summary(Cox_aalen_mod)
par(mfrow=c(3,3))
plot(Cox_aalen_mod)






