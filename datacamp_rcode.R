#section 1 ----

a <- "Abigail Guinan"
b1 <- 45.6
b2 <- "45.6"
c1 <- c(1:3)

class(c1)
class(b1)
class(a)
class(b2)
b1+b2

a+b2

classtest <- b1+c1

#section 2 ----

v1 <- c(-2:2)
v2 <- v1*3
sum(v2)

#section 3----
vec4 <- c(1:12)
mat_1 <- matrix(vec4, nrow = 3, ncol = 4, byrow = TRUE)
mat_1

mat_2 <- matrix(vec4, nrow = 3, ncol = 4, byrow = FALSE)
mat_2

#section 4----
my_list_1 <- list("two" = 5.2, "one" = "five point two", "three" = c(0:5))
my_list_1

my_list_1[[3]]

my_list_1$one

my_list_1[["one"]]

#section 5 ----
my_vec = rep(1:3, 5)
my_vec

my_bool_vec <- my_vec == 3
my_bool_vec

data.frame(my_vec, my_bool_vec)

my_vec[my_bool_vec]
