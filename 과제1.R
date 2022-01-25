2021320322 데이터과학과 윤민서

#1
n <- scan()
fib <- c(2, 2)
for (i in 3:n) fib[i] <- fib[i-1] - fib[i-2]
fib

#2
A <- list(const = seq(1, 158), prop = seq(159, 169))
B <- list(const = seq(170, 254), prop = seq(255, 273))
C <- list(const = 274, prop = seq(275, 279))
D <- list(const = NULL, prop = c(280, 281, 282))
E <- list(const = NULL, prop = c(283, 284, 285))
F <- list(const = NULL, prop = 286)
G <- list(const = NULL, prop = 287)
H <- list(const = seq(288, 293), prop = seq(294, 297))
nation <- list(A = A, B = B, C = C, D = D, E = E, F = F, G = G, H = H)
sample <- sample(seq(1, 297), 30, replace = FALSE)
comm <- matrix(0, nrow = 8, ncol = 2)
rownames(comm) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(comm) <- c("const", "prop")
for (i in sample) {
  for (j in 1:8) {
    for (k in 1:2) {
      if (i %in% nation[[j]][[k]]) comm[j, k] <- comm[j, k] + 1
    }
  }
}
comm

#3
pas <- list(c(1, 1))
for (i in 2:10) {
  pas[[i]] <- c(pas[[i-1]], 0) + c(0, pas[[i-1]])
}
pas

install.packages("Lahman")
library(Lahman)
Salaries.1 <- Salaries[Salaries$yearID == 2015, ]

#4
sal <- quantile(Salaries.1$salary, probs = seq(0, 1, 0.1))
sal
logsal <- log(sal)
logsal
plot(sal)
plot(logsal)

-	수열 sal은 지수함수 모양으로 증가하는 패턴을 보인다.
-	로그변환된 수열 logsal은 0~30% 구간에서는 거의 일정하다가 30% 구간 이후로 기울기가 양수인 선형 함수의 패턴을 보인다.

#5
midsal <- tapply(Salaries.1$salary, Salaries.1$teamID, median)
midsal <- midsal[complete.cases(midsal)]
midsal
barplot(midsal, ylab = 'Salary')
barplot(midsal, ylim = c(0, 4000000), ylab = 'Salary', main = "Median of Salaries")
barplot(sort(midsal/1000), ylim = c(0, 4000), ylab = 'Salary', main = "Sorted Median of Salaries( / 1000)")
max(midsal)/ min(midsal)

#6
texsal <- subset(Salaries.1, teamID == "TEX", select = "salary")
barplot(texsal$salary)
barplot(sort(texsal$salary/1000), ylim = c(0, 25000), ylab = 'Salary', main = "Sorted Salaries( / 1000) of Texas Rangers")
midtex <- apply(texsal, 2, median)
midtex
cshsal <- subset(Salaries.1, (teamID == "TEX" & playerID == "choosh01"), select = "salary")
cshsal$salary / midtex

#7
library(dplyr)
Pitching.1 <- subset(Pitching, yearID == 2015)
sumsal <- sum(Salaries.1$salary)
ij <- inner_join(Salaries.1, Pitching.1)
pitsal <- sum(ij$salary, na.rm = TRUE)
sumsal
pitsal
pitsal / sumsal