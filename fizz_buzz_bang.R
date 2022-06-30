create_data = function(df, n){
  for (i in n) {
    # find modulus of number for the given vector
    df = within(df, assign(paste0('V', i), df[, 1]%%i == 0))
    # find string contains the given number
    df = within(df, assign(paste0('Vg', i), grepl(i, df[, 1])))
  }
  return(df)
}

or_and_expr = function(n){
  my_expr = paste('(', 
                  paste(unlist( lapply(n, function(x) { 
                    paste('(df$V', x, ' | ', 'df$Vg', x, ')', sep  = '')
                  })),
                  collapse=" & " ),
                  ')', 
                  sep = '')
  return(my_expr)
}

myfun = function(df, n, replace){
  res_na = which(is.na(df$res))
  my_expr = or_and_expr(n = n)
  print(my_expr)
  x = which(eval(parse(text=my_expr)))
  x = intersect(res_na, x)
  df$res[x] = replace
  return(df)
}

myfun2 = function(df, yes, no, replace) {
  res_na = which(is.na(df$res))
  yes_expr = or_and_expr(n = yes)
  no_expr = paste('!', or_and_expr(n = no), sep = '')
  yes_no_expr = paste(yes_expr, no_expr, sep = " & ")
  print(yes_expr)
  print(no_expr)
  print(yes_no_expr)
  x = which(eval(parse(text=yes_no_expr)))
  x = intersect(res_na, x)
  df$res[x] = replace
  return(df)
}

# data setup
df1 = data.frame(vec = 1:100)
df1 = create_data(df=df1, n=c(3,4, 5))

# replacement
#---------------------------------
# 1. no matches
#---------------------------------
res = which(rowSums(df1[, -1]) == 0)
df1$res = NA
df1$res[res] = res
#---------------------------------
# 2. Fizz Buzz Bang
#---------------------------------
# (multiple of 3 or string 3) and (multiple of 4 or string 4) and (multiple of 5 or string 5)
df1 = myfun(df = df1, n = c(3,4,5), replace = 'Fizz Buzz Bang')

#---------------------------------
# 2. Fizz Buzz
#---------------------------------
# (multiple of 3 or string 3) and (multiple of 5 or string 5)
df1 = myfun2(df = df1, yes = c(3,5), no = c(4), replace = 'Fizz Buzz')

#---------------------------------
# 2. Fizz Bang
#---------------------------------
# (multiple of 3 or string 3) and (multiple of 5 or string 5)
df1 = myfun2(df = df1, yes = c(3,4), no = c(5), replace = 'Fizz Bang')

#---------------------------------
# 2. Buzz Bang
#---------------------------------
# (multiple of 3 or string 3) and (multiple of 5 or string 5)
df1 = myfun2(df = df1, yes = c(4,5), no = c(3), replace = 'Buzz Bang')

#---------------------------------
# 3. 'Fizz
#---------------------------------
df1 = myfun2(df = df1, yes = c(3), no = c(4,5), replace = 'Fizz')
#---------------------------------
# 4. 'Buzz'
df1 = myfun2(df = df1, yes = c(5), no = c(3,4), replace = 'Buzz')
#---------------------------------
# 4. 'Bang'
df1 = myfun2(df = df1, yes = c(4), no = c(3,5), replace = 'Bang')
#---------------------------------
# check any missing values in df1$res
stopifnot(!any(is.na(df1$res)))
#---------------------------------
View(df1)

# Bar chart
# data setup
df2 = as.data.frame(table(df1$res[!grepl('[[:digit:]]', df1$res)]))
df2 = rbind.data.frame(df2, 
                       data.frame(Var1 = 'no_matches', Freq = sum(grepl('[[:digit:]]', df1$res)))
)
# plot
library(ggplot2)
ggplot(data = df2, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = 'identity')

