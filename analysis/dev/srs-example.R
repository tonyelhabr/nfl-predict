
n <- 4L
set.seed(42L)
m1 <- matrix(nrow = n, ncol = n)
val_max <- 50L
# vals1 <- round(runif((n * (n + 1) / 2) - n, min = -val_max, max = val_max), 0)
vals1 <- round(runif(n * n, min = -val_max, max = val_max), 0)
vals1
vals2 <- rep(c(0L, 1L), length(vals1) / 2L) * vals1
vals2
m1[lower.tri(m1, diag = TRUE)] <- vals2
m2 <- t(m1)
m2[lower.tri(m2, diag = TRUE)] <- -vals2
diag(m2) <- 0
pd <- rowSums(m2)
m <- m2
diag(m) <- pd
# rm(list = c("m1", "m2"))

m
as_tibble(m)
pd
colSums(m)
rowMeans(m)
sum(m[1, 2:n]) / n
m[1, 1]

# Calculation loop. ----
# Parameters.
# n <- length(pd)
i_max <- 100
delta_min <- 1e-6
# dividend <- sum(idx_train[idx_train == TRUE])
# dividend <- (n * (n + 1) / 2) / 2
dividend <- n / 2

# Initiation.
r_saved <- matrix(nrow = i_max, ncol = n)
delta_saved <- matrix(nrow = i_max, ncol = 1)

i <- 1
delta <- 1
while(delta > delta_min & i < i_max) {
  if(i == 1) {
    r_i <- pd
    # r_i <- rep(1, n)
  }
  r_old <- r_i
  wts_i <- matrix(rep(r_i, n), byrow = TRUE, ncol = n)
  diag(wts_i) <- 0
  wts_out <- wts_i / dividend
  diag(wts_out) <- pd
  r_i <- rowMeans(wts_out)
  r_saved[i, ] <- r_i
  delta <- mean(abs(r_i - r_old))
  delta_saved[i] <- delta
  cat("iteration: ", i, "\n")
  cat("delta: ", round(delta, 6), "\n")
  i <- i + 1
}
r_final <- r_saved %>% as_tibble() %>% na.omit()
head(round(r_final, 4), 1)
tail(round(r_final, 4), 1)
r_final %>% tail(1) %>% as.matrix() %>% c() %>% sort(decreasing = TRUE) %>% round(2)
delta_final <- delta_saved %>% as_tibble() %>% na.omit()
round(delta_final, 6)
