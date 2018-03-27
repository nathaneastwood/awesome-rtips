context("Test dplyr hints")

test_that("Test the dplyr output is identical to the expectation", {
  
  # case_when
  set.seed(10)
  mydata <- 
    data.frame(
      x = 1:10,
      y = rnorm(10),
      name = c("Apple", "Banana", "Kiwi", "Orange", "Watermelon", "Grapes", 
               "Pear", "Cantelope", "Tomato", "Satsuma"),
      stringsAsFactors = FALSE
    ) %>% 
    mutate(
      name_poor = case_when(
        y < 0 ~ name,
        TRUE ~ ""
      )
    )
  
  expect_identical(mydata, expected_case_when)
  
  # mutate_at
  mutate_at_output <- 
    iris %>% 
    mutate_at(vars(Sepal.Length:Petal.Width), as.character) %>% 
    as_tibble()
  
  expect_identical(mutate_at_output, expected_mutate_at)
  
  # mutate_if
  mutate_if_output <- 
    iris %>% 
    mutate_if(is.factor, as.character) %>% 
    as_tibble()
  
  expect_identical(mutate_if_output, expected_mutate_if)
  
  # summarise_at
  summarise_at_output <- 
    mtcars %>% 
    group_by(cyl, am) %>% 
    summarize_at(vars(disp, hp, mpg), funs(mean, sd))
  
  expect_identical(summarise_at_output, expected_summarise_at)
})