mat_2 <- matrix(my_vec, nrow = 2, ncol = 3)
mat_2

mat_3 <- matrix(my_vec, nrow = 3, ncol = 2)
mat_3

mat_4 <- matrix(my_vec, nrow = 4, ncol = 4)

my_list_1 <- list("two" = 5.2, "one" = "five point two", "three" = c(0:5))

#1
my_list_1[[1]]
#2
my_list_1[[as.numeric("1")]]
#3
my_list_1[["1"]]
#4
my_list_1[["one"]]
#5
my_list_1$one
#6
my_list_1$"one"
#7
my_list_1$1
#8
my_list_1$"1"