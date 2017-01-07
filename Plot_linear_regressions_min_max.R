mydata = read.csv("Individual_genes.csv") 
Slope = mydata[,c(2)] 
intercept = mydata[,c(3)]
min_T = mydata[,c(7)]
max_T = mydata[,c(8)]
min_N = mydata[,c(9)]
max_N = mydata[,c(10)]
p_val = mydata[,c(5)]

xlim <- range(-2,2)
ylim <- range(-1, 1)
y = 0
x = 0
plot(y~x, xlim = xlim, ylim=ylim, type = 'n')
#abline(a = intercept[1], b = Slope[1])
#xlim <- range(min_T[1],max_T[1])
#ylim <- range(min_N[1], max_N[1])
i = 1
while (i<length(Slope)){
  if (p_val[i] > 0.05){
    abline(a = intercept[i], b = Slope[i], col = "blue")
  }
  else if (Slope[i] > 0){
    abline(a = intercept[i], b = Slope[i], col = "red")
  }
  else {
    abline(a = intercept[i], b = Slope[i])#, h =range(min_N[1], max_N[1]),v= range(min_T[1],max_T[1])
  }
  i = i + 1
}

mydata = read.csv("Individual_genes.csv") 
Slope = mydata[,c(2)] 
intercept = mydata[,c(3)]
min_T = mydata[,c(7)]
max_T = mydata[,c(8)]
min_N = mydata[,c(9)]
max_N = mydata[,c(10)]
p_val = mydata[,c(5)]

xlim <- range(-2,2)
ylim <- range(-1, 1)
y = 0
x = 0
plot(y~x, xlim = xlim, ylim=ylim, type = 'n')
#abline(a = intercept[1], b = Slope[1])
i = 1
while (i<length(Slope)){
  if (min_T[i] < -2){
    min_T[i] = -2
  }
  if (max_T[i] > 2){
    max_T[i] = 2
  }
  if (min_N[i] < -1){
    min_N[i] = -1
  }
  if (max_N[i] > 1){
    max_N[i] = 1
  }
  #ylim <- range(min_N[i], max_N[i])
  interval = (max_T[i] - min_T[i])/100
  x <- seq.int(min_T[i], max_T[i], interval)
  df <- data.frame(x = x,
                   y = Slope[i]*x + intercept[i])
  if (p_val[i] > 0.05){
    lines(y ~ x, data = df, type = 'l', col = "blue")
  }
  else if (Slope[i] > 0){
    lines(y ~ x, data = df, type = 'l', col = "red")
  }
  else {
    lines(y ~ x, data = df, type = 'l')
  }
  i = i + 1
}
