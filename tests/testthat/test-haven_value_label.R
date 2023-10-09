

data <- tibble(a = haven::labelled(1:3, labels = c(dog = 1, monkey = 2, cat = 3), label = 'animal'),
               b = haven::labelled(4:6, labels = c(bone = 4, banana = 5, fish = 6), label = 'favorite'),
               c = haven::labelled(c('01', '02', '03'), labels = c('wolf' = '01', 'human' = '02', 'lion' = '03'), label = 'relatives')
               )

vec <- 1:3


testthat::expect_equal(haven_value_label(data),
                       tibble(変数 = c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c'),
                              値 = c('1', '2', '3', '4', '5', '6', '01', '02', '03'),
                              ラベル = c('dog', 'monkey', 'cat', 'bone', 'banana', 'fish', 'wolf', 'human', 'lion'))
                       )


testthat::expect_error(haven_value_label(vec))

