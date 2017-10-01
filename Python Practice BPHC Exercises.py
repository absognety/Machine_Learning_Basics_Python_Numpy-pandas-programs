import random
x = random.randrange(0,21)
print (x)
if (x%2 == 0):
    print ("Your generated number is even")
else:
    print("Your generated number is odd")


import string
stn = string.ascii_lowercase
def capitalize(string):
    st = str(string) 
    first = st[0]
    if first in stn:
        up_first = first.upper()
        for i in range(0,len(st)):
            if(st[i] == " "):
                temp = st[i+1].upper()
                result = up_first+st[1:i+1]+temp+st[i+2:]
        if ' ' not in st:
            result = up_first+st[1:]
    if first in range(10):
        result = st
    return(result)


if __name__ == '__main__':
    global result
    string = input()
    capitalized_string = capitalize(string)
    print(capitalized_string)
    
arr = [9,1,10,6,0]
def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

if __name__ == '__main__':
    N = int(input())
    l = []
for i in range(N):
    name = input()
    spl = str.split(name)
    if len(spl) == 3:
        com = str(spl[0])
        ind = int(spl[1])
        obj = int(spl[2])
    elif len(spl) == 2:
        com = str(spl[0])
        obj = int(spl[1])
    elif len(spl) == 1:
        com = str(spl[0])
    if com == "insert":
        l.insert(ind,obj)
    elif com == "print":
        print (l)
    elif com == "remove":
        l.remove(obj)
    elif com == "append":
        l.append(obj)
    elif com == "sort":
        l.sort()
    elif com == "pop":
        l.pop()
    else:
        l.reverse()

if __name__ == '__main__':
    n = int(input())
    arr = map(int, input().split())
    ar = list(arr)
    ar = list(set(ar))
    m = max(ar)
    ar.remove(m)
    l = []
    for i in range(len(ar)):
        l.append(m-ar[i])
    least_dif = min(l)
    for i in range(len(l)):
        if(l[i] == least_dif):
            print (ar[i])

def word_distribution(string):
    mod_string = string.lower()
    samp = mod_string.translate({ord('!'):None,ord('?'):None,ord(','):None})
    new_list = str.split(samp)
    iter = {}
    for item in new_list:
        iter.update({item:samp.count(item)})
    return(iter)

import requests
url = "https://en.wikipedia.org/wiki/main_page"
responses = requests.get(url)
responses.status_code
mydata = responses.content.decode('utf-8')
mydata.find("Did you know")

import numpy as np
import pandas as pd
dt = {'x':[np.random.randn(3)],'y':[np.random.randn(3)],
              'z':[np.random.randn(3)]}


###Median Absolute Deviation###
l = []
import random
y = [random.random() for _ in range(1000)]
import numpy as np
y = np.array(y)       
mean = y.mean()    
med = np.median(y)
sdd = np.std(y)
out = mean +  3 * sdd
in_ = mean - 3 * sdd
z = list(y)
temp = list(filter(lambda x: x < out and x > in_, z))
len(temp)
sorted_list = np.sort(temp)
np.median(sorted_list)
from statsmodels import robust
robust.mad(y)
#Filter
#Map
#NumPy
#scipy


dist = np.random.uniform(low = 2.5,high = 7.5)

###Linear Regression###
import numpy as np
from matplotlib import pyplot as plt
X = np.array([1,2,3,4,5])
Y = np.array([2,4,6,8,9])

t1 = sum(X)
t2 = sum(Y)

sig_xy = sum(X * Y)
x_sq = np.square(X)
sum_x_sq = sum(x_sq)

y_sq = np.square(Y)
sum_y_sq = np.sum(y_sq)

a = np.array([[sum_x_sq,t1],[t1,5]])
b = np.array([sig_xy,t2])

s = np.linalg.solve(a,b)
e_list = s[0]*X + s[1] - Y

plt.plot(X,Y)

#####Pythogorean Triplets#####

import os
os.chdir("C:/Users/Vikas/Desktop")
pytrips = np.loadtxt("Triplets.txt",dtype = np.uint,delimiter = ",")
sq_sides = np.square(pytrips)
m = np.array([np.max(pytrips[i]) for i in range(len(pytrips))])
hyp_sq = np.square(m)
result = ["pythogorean triplets" if(sum(sq_sides[i][np.where(sq_sides[i] != hyp_sq[i])]) == hyp_sq[i]) 
      else "not pythogorean triplets" for i in range(len(sq_sides))]
    

z = np.arange(6).reshape(3,2) ##product of shape elements = limit in range

import numpy as np
dim = input().strip().split(' ')
dim_ = list(map(int,dim))
_ent_ = []
for i in range(dim_[0]):
    ent = input().strip().split(' ')
    ent_ = list(map(int,ent))
    _ent_.append(ent_)
    
np.array(_ent_).shape
np.array(_ent_).T
np.array(_ent_).flatten()
    
Num = (5 * sig_xy) - (t1 * t2)
Den = np.sqrt((5 * sum_x_sq - (t1**2)) * (5 * sum_y_sq - (t2**2)))
P_coeff = Num/Den

##Pearson Correlation Formula##

#Singular Value Decomposition#
import numpy as np
C = np.array([[5,5],[-1,7]])
prod = np.dot(C.T,C)
eig = np.linalg.eig(prod)
C_V = np.dot(C,eig[1])
sing_val = np.sqrt(np.diag(eig[0]))
sigma_inv = np.linalg.inv(sing_val)
U = np.dot(C_V,sigma_inv)  #left orthogonal matrix - Unit vector
print (sing_val)       #diagnol matrix (singular values)
print (eig[1])        #Right orthogonal matrix - Unit Vector

##Chi-Square Test##

obs = np.array([441,404,402,353])
expec = np.array([400,400,400,400])

error = obs - expec
num = np.square(error)
chi_sq = num/expec
chi_sq_res = sum(chi_sq)




















