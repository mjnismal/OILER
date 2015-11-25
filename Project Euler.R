#Project Euler

#problem 1 
x = 1:999
p1 = sum(x[x%%3==0 | x %%5 == 0])
print(p1)
#user  system elapsed 
#0.02    0.00    0.01 
#Answer:233168

#problem 2
i2 = 1
sumfib = 0
c = 0
while(i2 < 4000000){
  if(i2%%2 == 0 & i2 < 4000000){
    sumfib = sumfib+ i2
  }
  
  i1 = c
  c = i2
  i2 = i1+c
  print(i2)
}
p2 = sumfib
print(p2)

# user  system elapsed 
#0.02    0.00    0.01 
#Answer: 4613732

xx = proc.time() 
#problem 3
x= 600851475143
i= 2
while (x != 1){
if(x %% i ==0){
x = x/i
p3 = i  
i=2  
} else {
  i= i+1
  
}
}
print(p3)
#user  system elapsed 
#0.02    0.01    0.03 
#Answer: 6857


#problem 4
p4 = 0
z = c(0)
for(i in 999:900){
for(j in 999:900){
  x = i*j
  
for(k in 6:1){
  z[k]= (x%%10^(k) - x%%10^(k-1))/(10^(k-1))
}  

  
if((z[1]*100+z[2]*10+z[3])==(z[4]+z[5]*10+z[6]*100)){
p4 = x
stop(p4)
}  
}
}
# user  system elapsed 
# 0.03    0.01    0.05
# Answer: 906609


#problem 5
ans1= 1:20

for(i in 2:20){
  for(j in 1:(i-1)){
    if(i %% j == 0) {
      ans1[i] = ans1[i]/ans1[j]
    }
  }
}
p5 = prod(ans1) 
print(p5)

#user  system elapsed 
#0.01    0.01    0.03 
#Answer:232792560


#problem 6
p6 = sum(1:100)^2 - sum((1:100)^2)
print(p6)
# user  system elapsed 
#0       0       0 
#Answer: 25164150


#problem 7
n= 1000
ind= 10001
repeat{
n = as.integer(n)
if(n > 10^8){ 
  stop("n too large")
}
primes = rep(TRUE, n)
primes[1] = FALSE
last.prime = 2L
fsqr = floor(sqrt(n))
while (last.prime <= fsqr){
  primes[seq.int(2L*last.prime, n, last.prime)] = FALSE
  sel = which(primes[(last.prime+1):(fsqr+1)])
  
  if(any(sel)){
    last.prime = last.prime + min(sel)
  }else {last.prime = fsqr+1
  }
}

z = which(primes)
if(!is.na(z[ind])){ 
 p7 = z[ind]
  stop(p7)
}
n = n*2
}
#user  system elapsed 
#0.03    0.03    0.08 
#Answer: 104743


#problem 8
#skipped


#problem 9
ptm = proc.time()
for(m in 1:1000){
for(n in 1:(m-1)){
  p = m*(m+n)
  if(p == 500){
    a = m^2 + n^2
    b = 2*m*n
    c = m^2 - n^2
    p9p = a*b*c
    stop(p9p)
  }
}  
  
}
# user  system elapsed 
# 0.00    0.02    0.02
#Answer :31875000

#problem 10
  n = 2000000
  n = as.integer(n)
  if(n > 10^8){ 
    stop("n too large")
  }
  primes = rep(TRUE, n)
  primes[1] = FALSE
  last.prime = 2L
  fsqr = floor(sqrt(n))
  while (last.prime <= fsqr){
    primes[seq.int(2L*last.prime, n, last.prime)] = FALSE
    sel = which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime = last.prime + min(sel)
    }else {last.prime = fsqr+1
    }
  }
sum(as.numeric(which(primes)))
# user  system elapsed 
# 0.11    0.05    0.15
#Answer: 142913828922


#problem11
#Imported from csv file to p11ds
vert = as.data.frame(1)
hori = as.data.frame(1)
diagx = as.data.frame(1)
diagy = as.data.frame(1)
a = 1
b = 1
c = 1
d = 1 
for(i in 1:20){
for(j in 1:20){

if((j+3)<=20){
hori[a,1] = prod(p11ds[i,(j:(j+3))])
a = a +1
}
if((i+3) <= 20){
vert[b,1] = prod(p11ds[(i:(i+3)),j])
b = b +1
}
if((i+3)<= 20 & (j+3)<= 20){
diagx[c,1] =p11ds[i,j]*p11ds[i+1,j+1]*p11ds[i+2,j+2]*p11ds[i+3,j+3]


c = c+1
}
  if((i+3) <= 20 & (j-3) >= 1){
    diagy[d,1] =p11ds[i,j]*p11ds[i+1,j-1]*p11ds[i+2,j-2]*p11ds[i+3,j-3]
    d = d+1
}
}
}
max(vert,hori,diagx,diagy)
#user  system elapsed 
#0.37    0.00    0.37
#Answer: 70600674


#problem12
n = 1L
x = 6L
while(n<=250){
sums = x*(x+1)/2
x = x+1
n = sum(sums %% (1:ceiling(sqrt(sums))) == 0)

}
print(sums)
#  user  system elapsed 
#  1.93    0.02    1.97 
#Answer: 76576500
