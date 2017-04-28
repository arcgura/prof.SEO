install.packages("dplyr") 
install.packages("tidyr")
install.packages("readr")
library(dplyr)  
library(tidyr)
library(readr)

### Type 1 : column headers are values

grade <- c("a", "b", "c", "d", "e")
male <- c(1,5,5,5,7)
female <- c(5,0,2,5,4)
students <- data.frame(grade, male, female) ; students
gather(students, sex, count, -grade)



### Type 2 : multiple variables are stored in one column

male_1 <- c(3,6,7,4,1)
female_1 <- c(4,4,4,0,1)
male_2 <- c(3,3,3,8,2)
female_2 <- c(4,5,8,1,7)
students2 <- data.frame(grade, male_1, female_1, male_2, female_2) ; students2

students2 %>%
  gather(sex_class, count, -grade) %>%
  separate(sex_class, c("sec", "class")) %>%
  print



### Type 3 : variables are stored in both row and columns
name<-rep(c("sally", "jeff", "roger", "karen", "brian"), each=2)
test<-rep(c("midtem", "final"), 5)
class1 <- c("A", "C", NA, NA, NA, NA, NA, NA, "B", "B")
class2 <- c(NA, NA, "D", "E", "C", "A", NA, NA, NA, NA)
class3 <- c("B", "C", NA, NA, NA, NA, "C", "C", NA, NA)
class4 <- c(NA, NA, "A", "C", NA, NA, "A", "A", NA, NA)
class5 <- c(NA, NA, NA, NA, "B", "A", NA, NA, "A", "C")
students3 <- data.frame(name, test, class1, class2, class3, class4, class5, stringsAsFactors = FALSE) ; students3

aa<- students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) %>%
  print()



### Type 4 : multiple observational units are stored in the same table

i <- sample(100:1000, 5, replace = FALSE)
id<- rep(i, each=2) ; id
sex <- sample(c("F","M"), 10, replace = TRUE)
class <- sample(1:5, 10, replace = TRUE)

students4 <- data.frame( id, name, sex, class, aa$midtem, aa$final) ; students4

student_info <- students4 %>%
  select(id,name,sex) %>%
  unique %>%
  print

gradebook <- students4 %>%
  select(id,class,aa.midtem,aa.final) %>%
  print

### Type 5 : single observational unit stored in multiple tables
pf <- select(students4, name, class, aa.final)
pf
passed <- pf[ pf$aa.final %in% c("A", "B"),]  ;  passed
failed <- pf[ pf$aa.final %in% c("C", "D", "E"),  ]  ;   failed

passed <- mutate(passed, status = "passed")
failed <- mutate(failed, status = "failed")
bind_rows(passed, failed)


### quiz
score_range<-c("700-800", "600-690", "500-590", "400-490", "300-390", "200-290")
read_male <- sample(100000:200000, 6)
read_fem <- sample(100000:200000, 6)
read_total <- read_male + read_fem
math_male <- sample(100000:200000, 6)
math_fem <- sample(100000:200000, 6)
math_total <- math_male + math_fem
write_male <- sample(100000:200000, 6)
write_fem <- sample(100000:200000, 6)
write_total <- write_male + write_fem

sat <- data.frame(score_range, read_male, read_fem, read_total, math_male, math_fem, math_total, write_male, write_fem, write_total)
sat

sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part","sex")) %>%
  group_by(part,sex) %>%
  mutate(total=sum(count), prop=count/total) %>%
  print
