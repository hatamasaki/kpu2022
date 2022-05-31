#いつものデータを入れる
library(readxl)
library(gplots)
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)

#いつものデータを入れる
data<-read_excel("trialdata.xls")

#教育程度の欠損値処理
data <- data %>% 
  mutate(edu = 
           if_else(q2_2 == 6, NA_real_, q2_2)) %>% 
  mutate(edu = factor(edu,labels = c("中卒","高卒", "専門卒", "大卒", "院卒")))

#割合を見る
data %>% 
  with(prop.table(table(edu))*100)

#PIDを処理
data <- data %>% 
  mutate(pid=
           case_when(q11_1==1|q11_1==5 ~ "与党",
                     q11_1 == 9 ~ "無党派",
                     q11_1== 10|q11_1== 11 ~ NA_character_,
                     TRUE ~ "野党")) %>% 
  mutate(pid = factor(pid,levels = c("与党","野党", "無党派")))

#割合を見る
table(data$pid)#実数

rate.pid <- data %>% 
  with(prop.table(table(pid))*100)
rate.pid

cross.pe <- data %>% 
  with(round(prop.table(table(edu,pid),margin = 1)*100,2))

cross.pe

#教育程度を3区分けしてみよう
data <- data %>% 
  mutate(edu3 =
           case_when(q2_2==1|q2_2==2 ~ "中高卒",
                     q2_2==3 ~ "専門卒",
                     q2_2==4|q2_2==5 ~ "大院卒",
                     TRUE ~ NA_character_)) %>% 
  mutate(edu3 = factor(edu3,levels = c("中高卒","専門卒","大院卒")))

data %>% 
  with(table(edu3, useNA = "always"))

#クロス表
data %>% 
  with(round(prop.table(table(edu3,pid),margin = 1)*100,2))


###############################################################################
###############################################################################
###############################################################################
#連続変数（年齢）の統計量
data <- data %>% 
  mutate(age = age_raw)

d1 <- data %>%
  summarize(Mean   =     mean(age, na.rm = TRUE),  # 平均値
            Median =   median(age, na.rm = TRUE),  # 中央値
            SD     =       sd(age, na.rm = TRUE),  # 標準偏差
            Min    =      min(age, na.rm = TRUE),  # 最小値
            Max    =      max(age, na.rm = TRUE),  # 最大値
            Q1     = quantile(age, 0.25, na.rm = TRUE),  # 第一四分位点
            Q3     = quantile(age, 0.75, na.rm = TRUE))  # 第三四分位点

d1

#ちなみに図示もできます（hist関数）
hist(data$age)

#ggplot
data %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(bins = 60) +
  ylim(0, 100) +
  labs(x = "年齢", y = "出現度数")

#年齢でクロス表を書く
data %>%
  with(round(prop.table(table(age,pid),1)*100,2))

#解決策1：世代ごとに分けてみる
data <- data %>% 
  mutate(generation = 
           case_when(age>=18 & age<30 ~ "20代",
                     age>=30 & age<40 ~ "30代",
                     age>=40 & age<50 ~ "40代",
                     age>=50 & age<60 ~ "50代",
                     age>=60 & age<70 ~ "60代",
                     age>=70 ~ "70代以上"
           ))

#度数分布確認
data %>% 
  with(table(generation))

#世代と支持政党のクロス
data %>% 
  with(round(prop.table(table(generation, pid),1)*100,2))

######################################################################
########################################################
# 安倍感情温度
data <- data %>% 
  mutate(abe = q4_2_1)

#"gender"変数を作成してみよう（sexを使う）


#性別の度数分布
data %>% 
  with(round(prop.table(table(gender))*100,2))

#2値で平均値比較
data %>% 
  group_by(gender) %>% # genderの値ごとに分けると宣言
  summarize(mean = mean(abe, na.rm = TRUE),
            sd = sd(abe, na.rm = TRUE),
            n = n()) 

#3値以上で平均値比較
data %>% 
  filter(!is.na(pid)) %>%
  group_by(pid) %>% # genderの値ごとに分けると宣言
  summarize(mean = mean(abe, na.rm = TRUE),
            sd = sd(abe, na.rm = TRUE),
            n = n())

#3群以上で比較（方法は色々ある）
TukeyHSD(aov(data$abe~ data$pid))
pairwise.t.test(data$abe, data$pid, p.adj = "bonf")

#絵を書いてみる（シンプル）
plotmeans(data$abe ~ data$pid)

#絵を書いてみる（シンプル）
data %>% 
  filter(!is.na(pid)) %>%
  ggplot() +
  geom_boxplot(aes(x=pid, y=abe))

#絵を書いてみる（カラフル）
data %>% 
  filter(!is.na(pid)) %>%
  ggplot(aes(x=pid, y=abe)) +
  geom_jitter(aes(color = pid),
              width = 0.2, height = 0,
              show.legend = FALSE) +
  geom_boxplot(aes(fill = pid),
               alpha = 0.5,
               show.legend = FALSE) + 
  labs(x = "支持政党", y = "安倍感情温度") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
  )

########################################################
#宿題

#感情温度変数
data <- data %>% 
  mutate(自民 = q4_1_1,
           立民 = q4_1_2,
           共産 = q4_1_3,
           社民 = q4_1_4,
           国民 = q4_1_5)

#無党派層に嫌われてる政党
data %>% 
  filter(!is.na(pid)) %>% 
  group_by(pid) %>% 
  summarize(自民 = mean(自民, na.rm = TRUE),
              立民 = mean(立民, na.rm = TRUE),
              共産 = mean(共産, na.rm = TRUE),
              社民 = mean(社民, na.rm = TRUE),
              国民 = mean(国民, na.rm = TRUE))



#男女で最も好感度に差がある政党
#これもっと効率的にできる方法があれば…
自民g <- TukeyHSD(aov(data$自民~ data$gender))
立民g <- TukeyHSD(aov(data$立民~ data$gender))
共産g <- TukeyHSD(aov(data$共産~ data$gender))
社民g <- TukeyHSD(aov(data$社民~ data$gender))
国民g <- TukeyHSD(aov(data$国民~ data$gender))

自民g
立民g
共産g
社民g
国民g

#20 代で最も好感度に差がある政党
data <- data %>% 
  mutate(age20 = 
           case_when(age>=20 & age<30 ~ "20代",
                     TRUE ~ "その他"))

自民g2 <- TukeyHSD(aov(data$自民~ data$age20))
立民g2 <- TukeyHSD(aov(data$立民~ data$age20))
共産g2 <- TukeyHSD(aov(data$共産~ data$age20))
社民g2 <- TukeyHSD(aov(data$社民~ data$age20))
国民g2 <- TukeyHSD(aov(data$国民~ data$age20))

自民g2
立民g2
共産g2
社民g2
国民g2

########################################################


#無党派ダミー
data <- data %>% 
  mutate(pid.dummy = 
           case_when(pid=="与党"|pid=="野党" ~ "支持あり",
                     TRUE ~ "支持なし")) %>% 
  mutate(pid.dummy = factor(pid.dummy,levels=c("支持あり","支持なし")))

#確認
data %>% 
  with(table(pid.dummy,pid))

#クロス表
cross.gp <- data %>% 
  with(round(prop.table(table(gender,pid.dummy),1)*100,2))

cross.gp

#カイ二乗検定
chisq.test(data$gender,data$pid,correct=FALSE)
#chisq.test(data$gender,data$pid,correct=FALSE)$observed

############################################################
#比率の差の検定

#たとえば，朝日新聞世論調査の内閣支持率と
#朝日新聞：https://digital.asahi.com/articles/ASNCJ5WYMNCJUZPS008.html?iref=pc_extlink
#読売新聞：https://www.yomiuri.co.jp/election/yoron-chosa/20201124-OYT1T50021/
#c（朝日の支持者数, 読売支持者数），c(朝日調査全体N，読売調査全体N)

prop.test(c(866, 1289), c(1547, 1953))

#では，朝日10月調査（https://digital.asahi.com/articles/ASNBL77D9NBLUZPS006.html）と比べると？
prop.test(c(***, 866), c(***, 1547))


############################################################
#平均値の検定
#平均値の検定の場合，カテゴリ間の対応関係を見極める必要がある（スライド参照）

#韓国 (q4_3_4)  日本 (q4_3_6)*18-39歳（若年層）と40歳−59歳（中年世代）と60歳以上（高齢世代）で
#日本と韓国の世代別好感度に差はあるかTurkeyの方法で検定してみましょう

#世代（gen3)を作成
data <- data %>% 
  mutate(gen3 =
           case_when(age>=18 & age<40 ~ "若年層",
                     age>=40 & age<60 ~ "ミドル層",
                     age>=60 ~ "高齢層")) %>% 
  mutate(gen3 = factor(gen3,level=c("若年層","ミドル層","高齢層")))


#海外好感度（変数名 = US,韓国,日本）を作成

#平均値自体も調べてみよう（いろんな方法があるので書きやすいのでどうぞ）
data %>% 
  group_by(gen3) %>% 
  summarise(us = mean(US,na.rm = TRUE),
            jpn = mean(日本,na.rm = TRUE),
            kea = mean(韓国,na.rm = TRUE))

##############################################################
#お絵かき
data %>% 
  ggplot() +
  geom_boxplot(aes(x=gen3, y=US))

data %>% 
  ggplot() +
  geom_boxplot(aes(x=gen3, y=日本)) 

data %>% 
  ggplot() +
  geom_boxplot(aes(x=gen3, y=韓国)) 

#####合成してみよう
data <- data %>% 
  mutate(US_k = q4_3_1,
         韓国_k = q4_3_4,
         日本_k = q4_3_6)

data %>% 
  select(日本_k, US_k,韓国_k,gen3) %>%
  pivot_longer(cols      = contains("_"),
               names_to  = "国別",
               values_to = "th") %>%
  mutate(国別 = recode(国別,
                       "日本_k" = "日本",
                       "韓国_k" = "韓国",
                       "US_k" = "US")) %>% 
  ggplot() +
  geom_boxplot(aes(x = gen3, y = th, fill = 国別)) +
  labs(x = "世代", y = "感情温度", fill = "国ごと")
#ファイルに保存
ggsave("国別感情温度.png",dpi = 100, width = 15, height = 5)

##############################################################
#単回帰分析
#install.packages("coefplot")
library(coefplot)

#年齢と自民感情温度の関係を見てみよう
data %>% 
  ggplot(aes(x = age, y = 自民)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "年齢", y = "自民党感情温度") +
  scale_x_continuous(breaks = seq(18,79, by=5),limits=c(18,79))

#自民党の感情温度に与える年齢の効果を検証
result1 <-  lm(自民 ~ age, data = data)         
summary(result1)
modelsummary(result1)


#ビジュアライズしようぜ！
coefplot(result1, intercept = FALSE)
#ggplotだと簡単に保存できる！
ggsave("回帰分析.png",dpi = 100, width = 10, height = 5)

##############################################################
#重回帰分析
#たとえば，政党支持別に見てみると…？

data %>%
  filter(!is.na(pid)) %>% 
  #drop_na(pid) %>% 
  ggplot(aes(x = age, y = 自民, colour = pid)) +
  geom_smooth(method = "lm") +
  facet_wrap(~pid, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme( legend.position = "none" )


#教育程度ごとには？
data %>% 
  filter(!is.na(edu3)) %>% 
  ggplot(aes(x = age, y = 自民, color = edu3)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~edu3, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme( legend.position = "none" )
#scale_y_continuous(breaks=seq(40,70,length=5),limits=c(40,70))


#性別ごと
data %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = age, y = 自民, color = gender)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ gender, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme( legend.position = "none" )

#都市規模ごと
data <- data %>% 
  mutate(citysize = 5 - q2_4) %>% 
  mutate(citysize = factor(citysize, label=c("町村","一般市","政令市","東京23区")))

data <- data %>% 
  mutate(citysize_n = 5 - q2_4)

data %>% 
  with(table(citysize))

data %>% 
  #filter(!is.na(citysize)) %>% 
  ggplot(aes(x = age, y = 自民, color = citysize)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ citysize, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme( legend.position = "none" )

#収入
data <- data %>% 
  mutate(income =
           if_else(q2_3 == 21, NA_real_, q2_3))

data %>% 
  mutate(income3 =
           case_when(q2_3 == 21 ~ NA_character_,
                     q2_3>=1 & q2_3<7 ~ "低所得者層",
                     q2_3>=7 & q2_3<14 ~ "中所得者層",
                     q2_3>=14 ~ "高所得者層"
           )) %>% 
  mutate(income3 = factor(income3, level=c("低所得者層","中所得者層","高所得者層"))) %>% 
  filter(!is.na(income3)) %>% 
  ggplot(aes(x = age, y = 自民, color = income3)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ income3, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme( legend.position = "none" )

#イデオロギー
data <- data %>% 
  mutate(ideology = q12_4)

result2 <- lm(自民 ~ age + gender + edu + income + citysize_n + ideology + pid, data = data)                   
summary(result2)
modelsummary(result2)

#ビジュアライズしようぜ！2
reg2 <- coefplot(result2, intercept = FALSE, lwdOuter = 1,
                 xlab = "係数の推定値",
                 ylab = "説明変数",
                 title = "",
                 newNames = c(pid無党派 = "支持政党:無党派",
                              pid野党 = "支持政党:野党",
                              age = "年齢",
                              gender女性 = "性別(女性)",
                              edu = "教育程度",
                              income = "世帯収入",
                              citysize_n = "都市規模",
                              ideology = "イデオロギー"),
                 )

print(reg2)
#ggsave("reg2.png")

#############################################################
#予測値の計算

#install.packages("marginaleffects")
#install.packages("ggeffects")

library(marginaleffects)
library(ggeffects)

##年齢の予測値をプロット

#marginaleffects
result3 <- lm(自民 ~ age + gender + edu + income + citysize_n + ideology + pid, data = data)                

pred <- predictions(result3,
                    newdata = datagrid(
                      age = c(18:79),
                      pid = "無党派",
                      gender = "男性",
                      edu ="大卒"))

pre1 <- ggplot(pred, aes(age, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  coord_cartesian(xlim = c(18, 79), ylim = c(30, 60)) + 
  labs(x = "年齢", y = "自民党感情温度の予測値",title = " 男性・無党派・大卒の場合")

print(pre1)
ggsave("年齢予測.png",pre1,dpi = 100, width = 10, height = 5)

#ggeffects
age.pre <- ggpredict(result3, terms = "age")

ggplot(age.pre, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  coord_cartesian(xlim = c(18, 79), ylim = c(40, 90)) + 
  labs(x = "年齢", y = "自民党感情温度の予測値")

##性別ごとの予測値:marginseffect
gender.pre <- predictions(result3,
                          newdata = datagrid(
                            gender =c("男性","女性"),
                            pid = "無党派",
                            edu ="大卒"))

pre2 <- ggplot(gender.pre, aes(gender, predicted)) +
  geom_point(position = position_dodge(.1),
             size = 5) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1),
    width = 0.2, 
    size = 0.75) +
  coord_cartesian(ylim = c(30, 50)) + 
  labs(x = "性別", y = "自民党感情温度の予測値",title = "無党派・大卒の場合") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
  ) 

print(pre2)
#ggsave("性別予測.png", pre2, dpi = 100, width = 10, height = 5)


##イデオロギーの予測値:marginseffect
ideology.pre <- predictions(result3, 
                            newdata = datagrid(ideology=c(0:10), 
                                               gender ="男性",
                                               pid = "無党派",
                                               edu ="大卒"))

pre3 <- ggplot(ideology.pre, aes(ideology, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 70)) + 
  labs(x = "イデオロギー", y = "自民党感情温度の予測値",title = "男性・無党派・大卒の場合")

print(pre3)
#ggsave("イデオロギー予測.png", pre3, dpi = 100, width = 10, height = 5)


##補足1：条件（支持政党別）をつけて，特定の人を想定したイデオロギーの予測値:marginseffect
ideology.pre.1 <- predictions(result3, 
                              newdata = datagrid(ideology = c(0:10), 
                                                 gender = "男性",
                                                 pid = c("与党","野党","無党派"), 
                                                 edu ="大卒"))

pre3.1 <- ggplot(ideology.pre.1, aes(x= ideology, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 100)) + 
  labs(x = "イデオロギー", y = "自民党感情温度の予測値") + 
  facet_wrap(~pid)

print(pre3.1)

##補足2：量的変数と質的変数の交差項を使った予測値（年齢と政党支持）
result4 <- lm(自民 ~ age * pid + gender + edu + income + citysize_n + ideology, data = data)

pre3.2 <- predictions(result3,
                      newdata = datagrid(age = c(18:79),
                                         pid = c("与党","野党","無党派"), 
                                         gender = "男性",
                                         edu ="大卒"))

ggplot(pre3.2, aes(x = age, y = predicted, color=pid, fill = pid)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
  coord_cartesian(xlim = c(18, 79), ylim = c(0, 100)) + 
  labs(x = "年齢", y = "自民党感情温度の予測値")

##補足3：連続変数同士の交差項を使った有意確認の推定方法（年齢とイデオロギー）
result5 <- lm(自民 ~ age * ideology + pid + gender + edu + income + citysize_n, data = data)
summary(result5)

plot_cme(result5, effect = "age", condition = "ideology", draw = TRUE) +
  theme_get() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "イデオロギー", y = "自民党感情温度に対する年齢の限界効果")


################################################################################
