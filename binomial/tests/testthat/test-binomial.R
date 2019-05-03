


context("Test for checker fuctions")

test_that("is check_prob works", {
  p1 <- 0.3
  p2 <- 2

  expect_true(check_prob(p1))
  expect_length(check_prob(p1), 1)
  expect_error(check_prob(p2), 'prob has to be a number between 0 and 1')

})

test_that("is check_trials works", {
  t1 <- 10L
  t2 <- 4.2
  t3 <- -1

  expect_true(check_trials(t1))
  expect_error(check_trials(t2), "trials has to be an non-negative integer")
  expect_error(check_trials(t3), 'trials has to be an non-negative integer')
})

test_that("is check_success works", {
  s1 <- 3L
  t1 <- 5L
  s2 <- 1L:4L
  s3 <- 7L
  s4 <- 3.3
  s5 <- -1L

  expect_true(check_success(s1,t1))
  expect_equal(check_success(s2,t1),TRUE)
  expect_error(check_success(s3,t1), "success cannot be greater than trials")
  expect_error(check_success(s4,t1), "success has to be an non-negative integer")
  expect_error(check_success(s5,t1), "success has to be an non-negative integer")
})

context("Test for the summary measures")
test_that("test the aux_mean function",{
  t1 <- 10L
  p1 <- 0.3

  expect_equal(aux_mean(t1,p1),t1*p1)
  expect_length(aux_mean(t1,p1),1)
  expect_type(aux_mean(t1,p1),'double')

})

test_that("test the aux_variance function",{
  t1 <- 10L
  p1 <- 0.3

  expect_equal(aux_variance(t1,p1),t1*p1*(1-p1))
  expect_length(aux_variance(t1,p1),1)
  expect_type(aux_variance(t1,p1),'double')

})

test_that("test the aux_mean function",{
  t1 <- 10L
  p1 <- 0.3
  t2 <- 9L
  p2 <- 0.5

  expect_equal(aux_mode(t1,p1),floor(t1*p1+p1))
  expect_equal(aux_mode(t2,p2),c(floor(t2*p2+p2),floor(t2*p2+p2)-1))
  expect_length(aux_mode(t1,p1),1)
  expect_length(aux_mode(t2,p2),2)
  expect_type(aux_mode(t1,p1),'double')

})

test_that("test the aux_skewness function",{
  t1 <- 10L
  p1 <- 0.3

  expect_equal(aux_skewness(t1,p1),(1-2*p1)/sqrt(t1*p1*(1-p1)))
  expect_length(aux_skewness(t1,p1),1)
  expect_type(aux_skewness(t1,p1),'double')

})

test_that("test the aux_kurtosis function",{
  t1 <- 10L
  p1 <- 0.3

  expect_equal(aux_kurtosis(t1,p1),(1-6*p1*(1-p1))/sqrt(t1*p1*(1-p1)))
  expect_length(aux_kurtosis(t1,p1),1)
  expect_type(aux_kurtosis(t1,p1),'double')

})


context("Test for binomial functions")
test_that("test the bin_choose function",{
  n <- 5L
  k1 <- 3L
  k2 <- c(1L:3L)
  k3 <- 8L

  expect_length(bin_choose(n,k1),length(k1))
  expect_length(bin_choose(n,k2),length(k2))
  expect_error(bin_choose(n,k3),"k cannot be greater than n")
})


context("Test for binomial functions")
test_that("test the bin_probability function",{
  s1 <- 2L
  s2 <- c(0L:2L)
  s3 <- 9L
  t1 <- 5L
  p1 <- 0.5

  expect_length(bin_probability(s1,t1,p1),length(s1))
  expect_length(bin_probability(s2,t1,p1),length(s2))
  expect_error(bin_probability(s3,t1,p1),"success cannot be greater than trials")
  expect_equal(bin_probability(s1,t1,p1),bin_choose(t1,s1)*(p1^s1)*((1-p1)^(t1-s1)))
})


context("Test for binomial functions")
test_that("test the bin_distribution function",{
  t1 <- 5L
  p1 <- 0.5

  expect_is(bin_distribution(t1,p1),c("bindis","data.frame"))
  expect_length(bin_distribution(t1,p1),2)
  expect_equal(nrow(bin_distribution(t1,p1)),t1+1)
})


context("Test for binomial functions")
test_that("test the bin_cumulative function",{
  t1 <- 5L
  p1 <- 0.5

  expect_is(bin_cumulative(t1,p1),c("bincum","data.frame"))
  expect_length(bin_cumulative(t1,p1),3)
  expect_equal(nrow(bin_cumulative(t1,p1)),t1+1)
})

