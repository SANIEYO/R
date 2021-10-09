
# 1. Packages / Options ---------------------------------------------------

if (!require(RSelenium)) install.packages('RSelenium'); require(RSelenium)
if (!require(stringr)) install.packages('stringr'); require(stringr)
if (!require(dplyr)) install.packages('dplyr'); require(dplyr)
if (!require(reshape2)) install.packages('reshape2'); require(reshape2)

if (!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if (!require(ggpubr)) install.packages('ggpubr'); require(ggpubr)
if (!require(plotly)) install.packages('plotly'); require(plotly)
if (!require(ggcorrplot)) install.packages('ggcorrplot'); require(ggcorrplot)

if (!require(xgboost)) install.packages('xgboost'); require(xgboost)


# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445

# 2. Data Crawling --------------------------------------------------------

links <- list(
  
  basic1 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx',
  basic2 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx',
  detail = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx'
  
)

# data <- KBO_crawl(2010, 2010, links)

getwd()
data <- read.csv("C:/Users/mingx/OneDrive/바탕 화면/data.csv") %>% 
  select(-c(GPA, SLG, OBP))

  

# 3. Data Wrangling -------------------------------------------------------

# Change Korean Variable Names to English

names(data)[1:3] <- c('index', 'player', 'team')
names(data)[10:11] <- c('X2B', 'X3B')

# Make Primary Key to erase duplicate names

data <- data %>% 
  mutate(PK = paste(year, team, player, sep = '_'))
data$PK
# Duplicated Names?

data %>% group_by(PK) %>% tally() %>% View

# I think It is not distinguishable So,,,, Erase it!

data %>% filter(PK %in% c(paste(2010:2016, 'LG', '이병규', sep = '_'))) %>% View
data <- data %>% 
  filter(!PK %in% c(paste(2010:2016, 'LG', '이병규', sep = '_')))

# Character to Numeric

str(data)
data_chr <- data %>% select(PK, year, team, player, position)
data_num <- data %>% select(-c(PK, year, team, player, position))

data_chr <- apply(data_chr, 2, as.character) %>% as.data.frame()
data_num <- apply(data_num, 2, as.numeric) %>% as.data.frame()

data <- cbind(data_chr, data_num)
str(data)

# Make X1B Variable

data <- data %>% mutate(X1B = H - HR - X2B - X3B)
data$X1B
# Erase Index

data$index <- NULL

# Erase NAs

data <- na.omit(data)

# 4. Domain Knowledge ---------------------------------------------------

# 4-1-1. Erase RBI / R (It is not about HITTING ABILITY)

data$RBI <- NULL
data <- data %>% select(-R)

# 4-1-2. Erase XBH (Linear Combination of X2B, X3B, HR)

data$XBH <- NULL

# 4-1-3. Erase G, PA (Same Information with AB)

data %>%
 dplyr::select(PA, G, AB) %>% 
  pairs()

data$G <- NULL
data$AB <- NULL
data$AB

# 4-2. Our Y Variable OPS 

data %>% 
  ggplot(aes(x = OPS)) +
  geom_density() +
  theme_bw()

# 4-2-1. OPS ~ PA

data %>% 
  ggplot(aes(x = PA,
             y = OPS)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  geom_smooth(formula = y ~ x,
              method = 'gam',
              size = 3) +
  geom_vline(aes(xintercept = 50),
             col = 'red') +
  theme_bw()

# 4-2-2. Over 50 PA

data %>% 
  filter(PA > 49) %>% 
  ggplot(aes(x = PA,
             y = OPS)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  geom_smooth(formula = y ~ x,
              method = 'gam',
              size = 3) +
  geom_vline(aes(xintercept = 50),
             col = 'red') +
  theme_bw()

### IMPORTANT

data <- data %>% filter(PA > 49)

# 5. EDA ------------------------------------------------------------------

ggplotly(
  
  data %>% 
    group_by(year) %>% 
    summarise(PA = sum(PA)) %>% 
    ggplot(aes(x = year,
               y = PA,
               fill = PA)) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient(low = 'black',
                        high = 'red') +
    labs(x = '연도',
         y = '타석 수',
         title = '연도별 타석 수') +
    theme_bw()
  
)

# 5-1. 안타 관련 변수 -----------------------------------------------------------------

# X3B????

data %>%
  select(X1B, X2B, X3B, HR, OPS) %>% 
  pairs()

# Density of X3B

quantile(data$X3B, probs = seq(0, 1, 0.1))

# H by year

ggplotly(
  
  data %>% 
    select(year, PA, H, paste0('X', 1:3, 'B')) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    melt(id.vars = c('year', 'PA', 'H')) %>% 
    ggplot(aes(x = year,
               y = value,
               col = variable)) +
    geom_point(aes(size = PA)) +
    geom_line(aes(group = variable)) +
    geom_line() +
    labs(title = '연도 별 안타 빈도',
         y = 'Freq') +
    theme_bw()
  
)

# Ratio??

ggplotly(
  
  data %>% 
    select(year, X1B, X2B, X3B, HR, H) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    mutate(X1B_ratio = X1B / H * 100,
           X2B_ratio = X2B / H * 100,
           X3B_ratio = X3B / H * 100,
           HR_ratio = HR / H * 100) %>% 
    select(year, ends_with('ratio')) %>% 
    melt(id.vars = 'year') %>%
    ggplot(aes(x = value,
               y = year,
               fill = variable)) +
    geom_bar(stat = 'identity') +
    labs(y = "Percentage") +
    theme_bw() 
)

ggplotly(
  
  data %>% 
    select(year, X1B, X2B, HR, H) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    mutate(X1B_ratio = X1B / H * 100,
           X2B_ratio = X2B / H * 100,
           HR_ratio = HR / H * 100) %>% 
    select(year, ends_with('ratio')) %>% 
    melt(id.vars = 'year') %>%
    ggplot(aes(x = value,
               y = year,
               fill = variable)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(round(value, 0), '%'))) +
    labs(y = "Percentage") +
    theme_bw() 
)

# 5-2. 출루 관련 변수 -----------------------------------------------------------

data %>%
  select(BB, IBB, HBP, OPS) %>% 
  pairs()

ggplotly(
  
  data %>% 
    select(year, PA, BB, IBB, HBP) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    melt(id.vars = c('year', 'PA')) %>% 
    ggplot(aes(x = year,
               y = value,
               col = variable)) +
    geom_point(aes(size = PA)) +
    geom_line(aes(group = variable)) +
    geom_line() +
    labs(title = '연도 별 안타 빈도',
         y = 'Freq') +
    theme_bw()
  
)

ggplotly(
  
  data %>% 
    select(year, BB, IBB, HBP) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    mutate(all = BB + IBB + HBP) %>% 
    mutate(BB_ratio = BB / all * 100,
           IBB_ratio = IBB / all * 100,
           HBP_ratio = HBP / all * 100) %>% 
    select(year, ends_with('ratio')) %>% 
    melt(id.vars = 'year') %>%
    ggplot(aes(x = value,
               y = year,
               fill = variable)) +
    geom_bar(stat = 'identity') +
    labs(y = "Percentage") +
    theme_bw() 
)


# 5-3. 아웃 관련 변수 -----------------------------------------------------------

data %>%
  select(SO, GO, AO, GO/AO, GDP, SAC, SF, OPS) %>% 
  pairs()

data %>%
  select(SO, GO, AO, GO/AO, GDP, SAC, SF, OPS) %>% 
  cor() %>% 
  ggcorrplot(lab = T,
             colors = c('blue', 'white', 'red'),
             hc.order = F)


# 5-4. Position -----------------------------------------------------------

my_comparisons <- list( c("Infielder", "Outfielder"), 
                        c("Infielder", "Catcher"), 
                        c("Outfielder", "Catcher"))

ggboxplot(data = data,
          x = 'position', 
          y = 'OPS',
          color = 'position', 
          palette = "jco", 
          bxp.errorbar = TRUE) +
  
  stat_boxplot(geom = 'errorbar', 
               data = data, 
               aes(x = position, 
                   y = OPS,
                   color = position)) +
  
  stat_compare_means(comparisons = my_comparisons) + 
  stat_compare_means(label.y = 1.7) +
  labs(x = '포지션') +
  
  theme_bw()


# 5.5. Team ----------------------------------------------------------------

ggplot(data,
       aes(x = team,
           y = OPS,
           col = team)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~ year)

# 6. Modeling -------------------------------------------------------------

data <- data %>% 
  select(-c(PK, team, player))

train <- data %>% filter(year != 2020) %>% sample_n(1500) %>% select(-year)
test <- data %>% filter(year == 2020) %>% select(-year)

# 6-1. Regression ---------------------------------------------------------


### First Regression

model1 <- lm(train, formula =  OPS ~ .) %>% step()
summary(model1)
plot(model1)

summary(model1)

### WHY???

train %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  ggcorrplot(hc.order = T,
             lab = T)

### PCA Regression

train_PCR <- prcomp(train %>% select_if(is.numeric), scale. = T)
test_PCR <- prcomp(test %>% select_if(is.numeric), scale. = T)

screeplot(train_PCR, type = 'line', npcs = 30)
biplot(train_PCR, main="Biplot")
summary(train_PCR)

train_PCR <- train_PCR$x %*% train_PCR$rotation
train_PCR <- data.frame(train_PCR) %>%
  select(paste0('PC', 1:5)) %>% 
  mutate(position = train$position,
         OPS = train$OPS)

test_PCR <- test_PCR$x %*% test_PCR$rotation
test_PCR <- data.frame(test_PCR) %>%
  select(paste0('PC', 1:5)) %>% 
  mutate(position = test$position,
         OPS = test$OPS)



model2 <- lm(train_PCR, formula = OPS ~ .) %>% step()
summary(model2)
plot(model2)


# 6-2. XGB ----------------------------------------------------------------

dtrain <- xgb.DMatrix(as.matrix(train %>% select(-c(OPS, position))),
                      label = train$OPS)
dtest <- xgb.DMatrix(as.matrix(test %>% select(-c(OPS, position))))


# Random Search

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

for (iter in 1:100) {
  
  param <- list(obj = 'reg:linear',
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .1), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  
  cv.nround <-  2000
  cv.nfold <-  5 
  seed.number  <-  sample.int(10000, 1) 
  
  
  set.seed(seed.number)
  
  
  mdcv <- xgb.cv(data = dtrain,
                 params = param,
                 nfold = cv.nfold, 
                 nrounds = cv.nround,
                 verbose = T,
                 early_stopping_rounds = 100,
                 print_every_n = 100,
                 maximize = FALSE)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  if (min_rmse < best_rmse) {
    
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_seednumber <- seed.number
    best_param <- param
    
  }
}

# Fit

set.seed(best_seednumber)
model3 <- xgb.train(data = dtrain,
                    params = best_param,
                    nround = best_rmse_index,
                    verbose = T)

xgb.plot.importance(xgb.importance(model = model3))


# 7. Compare --------------------------------------------------------------

pred1 <- predict(model1, test)
pred2 <- predict(model2, test_PCR)
pred3 <- predict(model3, dtest)

error1 <- test$OPS - pred1
error2 <- test$OPS - pred2
error3 <- test$OPS - pred3

mean(error1^2)
mean(error2^2)
mean(error3^2)

