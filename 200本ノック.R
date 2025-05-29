rm(list = ls())
print("Hello,R world!")
5 + 3 * 2 - 10 / 2
sqrt(841)
?sqrt
2^3
98^7
sqrt(-4)
x <- 42
x
hoge <- 1:10
hoge
hoge2 <- hoge * 2
hoge2
str(hoge2)
# enviromentを確認しました
hoge2[3]
hoge2[2:5]
hoge2[c(2, 4, 6, 8, 10)]
matrix(hoge2, ncol = 2)
hoge3 <- matrix(hoge2, ncol = 2, byrow = TRUE)
hoge3
dim(hoge3)
hoge3[1, ]
hoge3[, 2]
hoge3[2, 2]
hoge3 <- as.data.frame(hoge3)
str(hoge3)
colnames(hoge3) <- c("A", "B")
hoge3
install.packages("tidyverse")
library(tidyverse)
hoge3 <- as_tibble(hoge3)
hoge3
getwd()
install.packages("styler")
dat <- read_csv("BaseballDecade.csv")

# 625
# まずパッケージをインストール（初回のみ）
install.packages("readr")

# 次にパッケージを読み込む
library(readr)

# そしてCSVファイルを読み込む
dat <- read_csv("BaseballDecade.csv")

head(dat)
tail(dat)
dim(dat)
names(dat)
summary(dat)
str(dat)
class(dat)
dat.tb <- dat
dat.tb

# 626
dat.tb$Name
table(dat.tb$Name)
# 「%>% 」を使うためにmagrittrパッケージを使う
install.packages("magrittr") # 初回のみ
library(magrittr)
dat.tb$Name %>%
  table() %>%
  sort(decreasing = TRUE)
dat.tb$team %>% unique()
dat.tb$team %>%
  unique() %>%
  length()
dat.tb$team <- dat.tb$team %>% as.factor()
dat.tb$bloodType <- dat.tb$bloodType %>% as.factor()
dat.tb$position <- dat.tb$position %>% as.factor()
#
install.packages("dplyr")

dat.tb %>%
  select(team, bloodType, position) |>
  summary()

# 627
dat.tb$height %>% mean()
dat.tb$height %>% var()
dat.tb$height %>% sd()
dat.tb$height %>% range()
dat.tb$salary %>% quantile()
dat.tb$salary %>% quantile(probs = c(0, 0.25, 0.33, 0.95, 1))
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height / 100)^2))
dat.tb$bmi %>% summary()
dat.tb <- dat.tb %>% mutate(bmi_category = ifelse(bmi >= 25, "HighBMI", "Standard"))
dat.tb$bmi_category %>% table()

# 628
dat.tb <- dat.tb %>%
  mutate(position2 = case_when(position == "投手" ~ "投手", TRUE ~ "野手"))
dat.tb$position2 <- dat.tb$position2 %>% as.factor()
dat.tb$position2 %>% table()
table(dat.tb$position, dat.tb$position2)
dat.tb <- dat.tb %>%
  mutate(League = case_when(
    team %in% c("Giants", "Carp", "Tigers", "Swallows", "Dragons", "DeNA") ~ "Central",
    TRUE ~ "Pacific"
  ))
dat.tb$League <- dat.tb$League %>% as.factor()
table(dat.tb$team, dat.tb$League)
dat.tb <- dat.tb %>%
  mutate(Year_num = Year %>% str_remove("年度") %>% as.numeric())
dat.tb %>%
  select(Year, Year_num) %>%
  head()

# 629
dat.tb %>%
  filter(position2 == "野手") %>%
  head()
dat.tb %>%
  filter(position2 == "野手") %>%
  summary()
dat.tb %>%
  filter(Year_num <= 2015) %>%
  head()
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  head()
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  nrow()
dat.tb %>%
  select(Name, team, height, weight) %>%
  head()
dat.tb %>%
  select(Name, team, salary, Year_num) %>%
  filter(Year_num == 2020) %>%
  head()
dat.tb %>%
  arrange(desc(salary)) %>%
  head(1)
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  arrange(desc(salary)) %>%
  head(1)
dat.tb %>%
  filter(team == "Giants") %>%
  summarise(avg_height = mean(height), avg_weight = mean(weight))

# 6210
dat.tb %>%
  group_by(team) %>%
  summarise(mean_salary = mean(salary)) %>%
  arrange(desc(mean_salary))
dat.tb %>%
  group_by(Year_num, team) %>%
  summarise(mean_salary = mean(salary)) %>%
  head(10)
dat.tb %>%
  group_by(Year_num, team) %>%
  summarise(
    mean_salary = mean(salary),
    max_salary = max(salary),
    min_salary = min(salary)
  ) %>%
  head(10)
dat.tb %>%
  group_by(bloodType) %>%
  summarise(mean_bmi = mean(bmi)) %>%
  arrange(desc(mean_bmi))
dat.tb %>%
  group_by(League) %>%
  summarise(
    mean_salary = mean(salary),
    median_salary = median(salary)
  )
dat.tb %>%
  group_by(position) %>%
  summarise(
    avg_height = mean(height),
    avg_weight = mean(weight)
  ) %>%
  arrange(desc(avg_height))
dat.tb %>%
  group_by(Year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE))
dat.tb %>% group_by(Year_num) %>% summarise(total_HR = sum(HR, na.rm = TRUE))
dat.tb %>% select(Year_num, Name, height, weight) %>%
  filter(Year_num == 2020) %>% head()

#6211
dat.tb2 <- dat.tb %>% select(Year_num, Name, height, weight) %>%
  filter(Year_num == 2020) %>% select(-Year_num)
head(dat.tb2)
model <- lm(height ~ weight, data = dat.tb2)
summary(model)
dat.tb2 %>% pivot_longer(-Name, names_to = "variable", values_to = "value") %>% head()
dat.tb2_long <- dat.tb2 %>%
  pivot_longer(-Name, names_to = "variable", values_to = "value") 
str(dat.tb2_long)
dat.tb2_long %>% group_by(variable) %>% summarise(mean_value = mean(value))
dat.tb2_long %>% pivot_wider(names_from = variable, values_from = value) %>% head()
bat_stats <- dat.tb %>% filter(position2 == "野手") %>%
  select(Year_num, Name, AtBats, Hit, HR)

#6212
bat_stats <- bat_stats %>% mutate(avg = Hit / AtBats) 
head(bat_stats)
bat_stats %>% arrange(desc(avg)) %>% head(10)
bat_stats %>% group_by(Year_num) %>% summarise(avg_batting = mean(avg, na.rm = TRUE))
bat_stats_long <- bat_stats %>%
  pivot_longer(c(AtBats, Hit, HR, avg), names_to = "stat", values_to = "value")
head(bat_stats_long)
bat_stats_long %>% group_by(Name, stat) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% head()
player_avgs <- bat_stats_long %>% group_by(Name, stat) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = stat, values_from = mean_value)
player_avgs %>% arrange(desc(avg)) %>% head(1)

#6213
ggplot() 
ggplot(dat.tb, aes(x = height)) + geom_histogram() 
ggplot(dat.tb, aes(x = height)) + geom_histogram(binwidth = 2)
ggplot(dat.tb, aes(x = height)) + geom_histogram(fill = "blue", color = "black")
ggplot(dat.tb, aes(x = height, y = weight)) + geom_point() 
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point()
ggplot(dat.tb, aes(x = height, y = weight, shape = bloodType)) + geom_point() 
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3)
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3) + labs(title = "身長と体重の関係")
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3) + labs(title = "身長と体重の関係",
                              x = "身長(cm)", y = "体重(kg)")

#6214
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + facet_wrap(~ team)
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth(method = "lm")
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth() 
ggplot(dat.tb, aes(x = height, y = weight, color = League)) +
  geom_point() + geom_smooth(method = "lm")
ggplot(dat.tb, aes(x = position, y = height)) + geom_boxplot()
ggplot(dat.tb, aes(x = position2, y = height)) + geom_boxplot()
ggplot(dat.tb, aes(x = position2, y = height)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.5)
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin() 
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + geom_boxplot(width = 0.1)
ggplot(dat.tb, aes(x = bloodType, y = weight, fill = League)) +
  geom_violin()

#6215
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_wrap(~ League)
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_grid(League ~ .)
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_grid(League ~ Year_num)
dat.tb %>% group_by(Year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line()
dat.tb %>% group_by(Year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line() + geom_point()
dat.tb %>% group_by(Year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = Year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_minimal() 
dat.tb %>% group_by(Year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = Year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_light()
dat.tb %>% group_by(Year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = Year_num, y = total_HR)) + geom_col()
dat.tb %>% group_by(Year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) +
  geom_col()
