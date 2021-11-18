n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 = vec_1 == 3
vec_2



#pick an n value
  n = 15
  
  for (i in 1:n){
  print(paste0("This is loop: ", i, sep =" "))
  }
  
  
n = 17

vec_1 <- c(sample(10, n, replace = TRUE))

for(i in 1:n){
  print(paste0("The element of vec_1 at index ", i," is ", vec_1[i], sep = " "))
}

create_and_print_vec <- function(n, min = 1, max = 10){
n <- sample(min:max, 1, replace = TRUE)
vec_1 <- c(sample(max, n, replace = TRUE))
for(i in 1:n){
  print(paste0("The element of vec_1 at index ", i," is ", vec_1[i], sep = " "))
}
}

create_and_print_vec()

