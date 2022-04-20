

set.seed(20000929) # 乱数のseedを指定

X1 <- rnorm(5, 0, 100) # N(0, 100)から乱数を5個抽出する
print(X1) # 抽出された乱数を出力する

mean(X1)

X2 <- rnorm(100, 0, 100) # N(0, 100)から乱数を100個抽出する
print(X2) # 抽出された乱数を出力する

mean(X2)

X3 <- rnorm(10000, 0, 100) # N(0, 100)から乱数を1万個抽出する
mean(X3) # 抽出された乱数の平均値を出力する

X4 <- rnorm(20000929, 0, 100) # N(0, 100)から乱数を1万個抽出する
mean(X4) # 抽出された乱数の平均値を出力する


library(tidyverse)

X_bar_vec <- rep(NA, 1000)     # 長さ1000の空ベクトルを作成
for (i in 1:1000) {            # iを1から1000へ増やしながら反復
  temp_vec <- rnorm(i, 0, 100) # N(0, 100)からi個の乱数を抽出し、temp_vecに格納
  # temp_vecの平均値をX_bar_vecのi番目要素として格納
  X_bar_vec[i] <- mean(temp_vec)
}

# 可視化
ggplot() +
  geom_line(aes(x = 1:1000, y = X_bar_vec)) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "n", y = expression(hat(theta)))

# 実際のデータでも見てみよう！
mean(age_raw)


times <- 100
Smean <- vector(length=times)
sum <- 0
for ( i in 1:times ) {
  data <- mean( sample( age_raw, 5 ) )
  sum <- sum + data
  Smean[i] <- sum / i
}
plot( 1:times, Smean, xlab="times", ylab="sample mean" )
myu <- round( mean( age_raw ), 3 )
mtext( paste( "μ=", myu ) )
abline( h=mean(age_raw), col="red", lwd=2, lty=2 )