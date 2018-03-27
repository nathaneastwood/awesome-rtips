context("Test purrr hints")

test_that("Test the purrr output is identical to the expectation", {
  set.seed(10)
  nested_iris <- 
    iris %>% 
    group_by(Species) %>% 
    nest() %>% 
    mutate(n = 2:4) %>% 
    mutate(subsamp = map2(data, n, sample_n)) %>% 
    select(Species, subsamp) %>% 
    unnest()
  expect_identical(nested_iris, expected_nested_iris)
})