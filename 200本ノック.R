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

#625
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

#626
dat.tb$Name
table(dat.tb$Name)
# 「%>% 」を使うためにmagrittrパッケージを使う
install.packages("magrittr")  # 初回のみ
library(magrittr)
dat.tb$Name %>% table() %>%  sort(decreasing = TRUE)
dat.tb$team %>% unique()
dat.tb$team %>% unique() %>% length()
dat.tb$team <- dat.tb$team %>% as.factor()
dat.tb$bloodType <- dat.tb$bloodType %>% as.factor()
dat.tb$position <- dat.tb$position %>% as.factor()
#
install.packages("dplyr")

dat.tb %>% select(team, bloodType, position) |> summary()

#627
dat.tb$height %>% mean()
dat.tb$height %>% var()
dat.tb$height %>% sd() 
dat.tb$height %>% range()
dat.tb$salary %>% quantile() 
dat.tb$salary %>% quantile(probs = c(0, 0.25, 0.33, 0.95, 1))
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height/100)^2))
dat.tb$bmi %>% summary()
dat.tb <- dat.tb %>% mutate(bmi_category = ifelse(bmi >= 25, "HighBMI", "Standard"))
dat.tb$bmi_category %>% table()

#628
dat.tb <- dat.tb %>%
  mutate(position2 = case_when(position == "投手" ~ "投手", TRUE ~ "野手"))
dat.tb$position2 <- dat.tb$position2 %>% as.factor()
dat.tb$position2 %>% table()
table(dat.tb$position, dat.tb$position2)
dat.tb <- dat.tb %>%
  mutate(League = case_when(
    team %in% c("Giants", "Carp", "Tigers", "Swallows", "Dragons", "DeNA") ~ "Central",
    TRUE ~ "Pacific"))
dat.tb$League <- dat.tb$League %>% as.factor()
table(dat.tb$team, dat.tb$League)
dat.tb <- dat.tb %>%
  mutate(Year_num = Year %>% str_remove("年度") %>% as.numeric())
dat.tb %>% select(Year, Year_num) %>% head()
