


data <- tibble(a = haven::labelled(1:3, labels = c(dog = 1, monkey = 2, cat = 3), label = 'animal'),
               b = haven::labelled(4:6, labels = c(bone = 4, banana = 5, fish = 6), label = 'favorite'))

vec <- 1:3


testthat::expect_equal(haven_variable_label(data),
                       tibble(変数 = c('a', 'b'), 位置 = 1:2, ラベル = c(a = 'animal', b = 'favorite')))


testthat::expect_error(haven_variable_label(vec))

