

data <- tibble(a = haven::labelled(1:3, labels = c(dog = 1, monkey = 2, cat = 3), label = 'animal'),
               b = haven::labelled(4:6, labels = c(bone = 4, banana = 5, fish = 6), label = 'favorite'))

vec <- 1:3


testthat::expect_equal(haven_value_label(data),
                       tibble(変数 = c('a', 'a', 'a', 'b', 'b', 'b'),
                              値 = c('dog' = 1, 'monkey' = 2, 'cat' = 3, 'bone' = 4, 'banana' = 5, 'fish' = 6),
                              ラベル = c('dog', 'monkey', 'cat', 'bone', 'banana', 'fish'))
                       )


testthat::expect_error(haven_value_label(vec))

