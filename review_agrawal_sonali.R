#Vectors and Lists (2 pts.)
vec1 = c(56, 10, 22, 11, 20)
vec2 = c(98, 7, 10, 7, 34, 73)
vec3 = c('z', 'red', 'first', 'd', 'cat')
list1 = list(vec1, vec2, vec3)
print(list1)

#Iteration and Math with While (3 pts.)
library(glue)
list2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
i = 1
while (i <= length(list2)) {
  if(list2[i] %% 2 != 0) {
    print(glue('Printing the value {list2[i]}.'))
  }
  i = i + 1
}

#Functions and Parameters (5 pts.)

my_function = function(str1, str2, int) {
  #================================
  # This function written by Sonali Agrawal
  
  if (nchar(str1) != nchar(str2)) {
    print('Unequal lengths')
  } else {
    print('Equal lengths')
  }
  outcome = nchar(str2) * int
  print(paste('Math is fun:',outcome))
}
var1 = 'Data'
var2 = 'Science'
var3 = 70
j = my_function(var1,var2,var3)