# Install my version of ockhamSEM:
# From GitHub:
# install.packages("devtools")
devtools::install_github("winterstat/ockhamSEM")

library(ockhamSEM)

# Set up data covariance matrix
p<-4 # number of variables
temp_mat <- diag(p) # identity matrix
colnames(temp_mat) <- rownames(temp_mat) <- paste0("V", seq(1, p))

# Set up some models
mod1a <- 'f1 =~ V1 + V2 + V3 + V4'
mod2a <- 'V3 ~ V1 + V4
  V2 ~ V3'
mod3a <- 'V3 ~ V1
  V2 ~ V3 + V4'
mod4a <- 'f1 =~ V1 + V2 + V3
f1 ~ V4'

# Set up fit objects
mod1a.fit <- sem(mod1a, sample.cov=temp_mat, sample.nobs=500, optim.force.converged = TRUE)
mod2a.fit <- sem(mod2a, sample.cov=temp_mat, sample.nobs=500, optim.force.converged = TRUE)
mod3a.fit <- sem(mod3a, sample.cov=temp_mat, sample.nobs=500, optim.force.converged = TRUE)
mod4a.fit <- sem(mod4a, sample.cov=temp_mat, sample.nobs=500, optim.force.converged = TRUE)

# Run FP analysis
res <- run.fitprop(mod1a.fit, mod2a.fit, mod3a.fit, mod4a.fit,
                   fit.measure=c("srmr","cfi", "rmsea"),
                   rmethod="onion",reps=100)

summary(res)
#saveRDS(res, file = "fp_test.RDS")
#res <- readRDS("fp_test.RDS")

# Does this work but show messages about not using bad defaults?
intersect.fitprop(res)

# Does this work and not show messages?
intersect.fitprop(res, cutoff = c(.08, .95, .06),
                  lower.tail = c(TRUE, FALSE, TRUE))

# Does this work without showing messages? Do you see 3 plots?
plot(res, type = "euler", cutoff = c(.08, .95, .06),
     lower.tail = c(TRUE, FALSE, TRUE))

# Does this work, when there are too many cutoff and lower.tail values? Write error message about this.
# Ok add a check for this. It just uses the first valus.
plot(res, type = "euler", whichfit = "cfi", cutoff = c(.08, .95, .06),
     lower.tail = c(TRUE, FALSE, TRUE))


# Does this show only 1, 3, 4?
plot(res, type = "euler", whichmod = c(1,3,4), cutoff = c(.08, .95, .06),
     lower.tail = c(TRUE, FALSE, TRUE))

# Does this show 1, 3, 4 with custom labels
plot(res, type = "euler", whichmod = c(1,3,4), cutoff = c(.08, .95, .06),
     lower.tail = c(TRUE, FALSE, TRUE), mod.lab = c("CFA", "Regression", "SEM"))

# Does this work?
plot(res, type = "ecdf", lower.tail = c(TRUE, FALSE, TRUE))

# Does this show vertical line at cutoff?
plot(res, type = "ecdf", cutoff = c(.08, .95, .06), lower.tail = c(TRUE, FALSE, TRUE))

# Does this compare  just model 1 and 2?
plot(res, type= "ecdf", whichmod = c(1, 2), whichfit = "srmr", cutoff = .08, lower.tail= TRUE)


# Does this ranking plot work?
plot(res, type = "ranks", whichfit = "cfi", lower.tail = FALSE)

# Does this pairwise plot work?
plot(res, type = "ranks", whichmod = c(1,4), whichfit = "cfi", lower.tail = FALSE)

# Does this pairwise plot give an error

plot(res, type = "pairwise",  whichfit = "cfi", lower.tail = FALSE)

# Does this pairwise plot work?
plot(res, type = "pairwise",  whichmod = c(1,4), whichfit = "cfi", lower.tail = FALSE)

# Go back to official ockhamSEM package
devtools::install_github("carlfalk/ockhamSEM")
