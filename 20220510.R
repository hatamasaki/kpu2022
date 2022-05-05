#いつものデータを入れる
library(readxl)
#install.packages("gplots") #インストールしたらこの行は消してOK
library(gplots)

#いつものデータを入れる
data<-read_excel("trialdata.xls")
#以下の分析では"data2020"でお願いね!
attach(data)

#まずは変数処理をしましょう
#教育程度（説明変数側）
#欠損値の処理
edu<- q2_2
edu[q2_2== 6] <- NA
#単純度数をedu1で格納
edu1 <- table(edu)
#パーセントで見てみる（パーセンタイルなので100倍にする）
edu_per <- prop.table(edu1)*100
edu_per

#支持政党（アウトカム側）の処理
#まずは簡単に政党支持者の割合を見てみよう
table(q11_1)

#欠損値(10と11）の処理
pid <- q11_1
pid[q11_1== 10|q11_1== 11] <- NA

#与党:自民+公明を"与党"
pid[q11_1== 1|q11_1== 5] <- "1"
#野党:自民+公明以外を"2"
pid[q11_1== 2|q11_1== 3|q11_1== 4|q11_1== 6|q11_1== 7|q11_1== 8] <- "2"
#無党派:「9.支持する政党はない」を"3"
pid[q11_1== 9] <- "3"

#単純度数をpid1で格納
pid1 <- table(pid)
#パーセントで見てみる（パーセンタイルなので100倍にする）
pid_per <- prop.table(pid1)*100
pid_per
#まとめてみたい
cbind(pid1, pid_per)

#さて，クロス表を見てみよう！
prop.table(table(edu,pid),margin = 1)
#うーんわかりずらい！
prop.table(table(edu,pid),margin = 1)*100


#数字じゃわかりずれぇなぁ
pid_n <- pid
  #与党:自民+公明を"与党"
pid_n[pid== 1] <- "government_party"
#野党:自民+公明以外を"2"
pid_n[pid== 2] <- "opposit_party"
#無党派:「9.支持する政党はない」を"3"
pid_n[pid== 3] <- "independent"


#少数桁も多いのでだるい．2桁までにしてクロス表を見てみよう！
round(prop.table(table(edu, pid_n),1)*100,2)

#教育程度も3段階でラベル付けしてみよう
edu_n <- edu
#1:中卒・高卒
edu_n[edu== 1|edu==2] <- "中/高卒"
edu_n[edu== 3] <- "専門卒"
edu_n[edu== 4|edu ==5] <- "大卒以上"

#少数桁も多いのでだるい．次は1桁までにしてクロス表を見てみよう！
round(prop.table(table(edu_n, pid_n),1)*100,1)
#ちなみに逆にしてみると？
round(prop.table(table(pid_n,edu_n),1)*100,1)



###############################################################################
###############################################################################
###############################################################################
#年齢
age <- age_raw
summary(age)

#ちなみに図示もできます
hist(age)

#年齢とクロス
round(prop.table(table(age,pid_n),1)*100,1)

#世代
generation <- age
generation[age>=18 &age<30] <- "20代"
generation[age>=30 &age<40] <- "30代"
generation[age>=40 &age<50] <- "40代"
generation[age>=50 &age<60] <- "50代"
generation[age>=60 &age<70] <- "60代"
generation[age>=70] <- "70代"
table(generation)
#世代と支持政党のクロス
round(prop.table(table(generation, pid_n),1)*100,1)

# 安倍感情温度
abe <- q4_2_1
summary(abe)

#性別ごとの平均値の比較
gender = sex
gender[sex==1] <- "男性"
gender[sex==0] <- "女性"

#2値で平均値比較
t.test(abe~gender,var.equal = TRUE)

#3群以上で比較（方法は色々ある）
TukeyHSD(aov(abe~pid_n)) 
pairwise.t.test(abe, pid_n, p.adj = "bonf")

#絵を書いてみる
#install.packages("gplots") #インストールしてない人はここから
library(gplots)
plotmeans(abe ~ pid_n)



