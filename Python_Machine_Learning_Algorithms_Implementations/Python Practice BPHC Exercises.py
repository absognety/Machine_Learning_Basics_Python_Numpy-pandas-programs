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
##Poker Dealing machine Problem##

obs = np.array([441,404,402,353])
expec = np.array([400,400,400,400])

error = obs - expec
num = np.square(error)
chi_sq = num/expec
chi_sq_res = sum(chi_sq)

from pandas import DataFrame
df = DataFrame({'key1' : ['a', 'a', 'b', 'b', 'a'],
'key2' : ['one', 'two', 'one', 'two', 'one'],
'data1' : np.random.randn(5),
'data2' : np.random.randn(5)})
print (df.data1)
print (df['data1'])
print (df.shape)
print (df.columns)
print (df.index)

###Scikit Learn Python Practice###
##** Naive Bayes Classifier *##
import sys
if('pandas' not in sys.modules):
    print ('Pandas library has to be loaded')
import pandas as pd
import numpy as np
mydata = pd.read_csv("C:/Users/Vikas/Documents/dataset_bayes.csv")
print (mydata.shape)
print (mydata.columns)
print (mydata.index)
print (mydata)
features = mydata.iloc[:,1:5]
target = mydata['Play Tennis']
feature_names = features.columns
####Play Tennis - Target Variable - Discrete####
####Day, Outlook, Temp, Humidity, Wind - predictors

def naive_bayes(feature_name):
    df = pd.DataFrame(features[feature_name]).join(target)
    target_cat = pd.Categorical(target)
    print (target_cat.categories)
    len_yes,len_no = len(target_cat[target_cat == 'Yes']),len(target_cat[target_cat == 'No'])
    df_group = list(df.groupby(by = df[feature_name]))
    num_classes = len(df_group)
    prob = []
    for i in range(num_classes):
        temp = df_group[i][1]
        temp0 = df_group[i][0]
        num1 = len(temp['Play Tennis'][temp['Play Tennis'] == 'Yes'])
        num2 = len(temp['Play Tennis'][temp['Play Tennis'] == 'No'])
        play_yes = num1/len_yes
        play_no = num2/len_no
        tot_prob = (num1 + num2)/(len_yes + len_no)
        prob.append({temp0:[play_yes,play_no,tot_prob]})
    return prob

print (feature_names)
prob_list = list(map(naive_bayes,feature_names))
from collections import ChainMap

#print ('Enter the test data point in this order with space separations')    

test = str(input("Outlook:(Sunny,Overcast,Rain),Temperature:(Hot,Mild,Cool),Humidity:(High,Normal),Wind:(Strong,Weak):?"))

inputs_user = test.split(" ")

def probab(inputs):
    frame1 = dict(ChainMap(*prob_list[0]))
    frame2 = dict(ChainMap(*prob_list[1]))
    frame3 = dict(ChainMap(*prob_list[2]))
    frame4 = dict(ChainMap(*prob_list[3]))
    prob_yes = frame1[inputs[0]][0] * frame2[inputs[1]][0] * frame3[inputs[2]][0] * frame4[inputs[3]][0]
    prob_no = frame1[inputs[0]][1] * frame2[inputs[1]][1] * frame3[inputs[2]][1] * frame4[inputs[3]][1]
    Total = frame1[inputs[0]][2] * frame2[inputs[1]][2] * frame3[inputs[2]][2] * frame4[inputs[3]][2]
    yes_total = prob_yes/Total
    no_total = prob_no/Total
    if (max(yes_total,no_total) == yes_total):
        print ('Player plays Tennis')
    else:
        print ('Player doesnt play Tennis')
    
print (probab(inputs_user))

##Naive Bayes Classifier for Continuous Data##
####Neural Network####

class udf:
    a = 4
    b = 8
    def fun(z):
        import numpy as np
        #sigmoid function
        return (1/(1+np.exp(-z)))

print (udf.a + udf.b)
print (udf.a * udf.b)
print (udf.a/udf.b)

#***Decision Tree Classifier Hands on in Python Programming***#
import os
os.chdir("C:/Users/Vikas/Documents")
mydata1 = pd.read_csv("dataset_bayes.csv")
print (mydata1.columns)
print (mydata1.info())
bin_target = mydata1['Play Tennis']
numofyes = len(bin_target[bin_target == 'Yes'])
numofno = len(bin_target[bin_target == 'No'])

def decision_tree(feature):
    sub_tree = mydata1[[feature,'Play Tennis']]
    feature_cat = pd.Categorical(sub_tree[feature])
    num_categories = len(feature_cat.categories)
    levels = feature_cat.categories
    yes_len = None
    no_len = None
    length_yesorno = None
    for i in levels:
        subset = sub_tree[sub_tree[feature] == i]
        if('Yes' in subset['Play Tennis'].values and 'No' in subset['Play Tennis'].values):
                print('Not a pure tree...')
                yes_len = len(subset[subset['Play Tennis'] == 'Yes'])
                no_len = len(subset[subset['Play Tennis'] == 'No'])
        else:
                print('Pure Tree')
                print('The branch {} is a pure branch'.format(i))
                print('Consider another feature for building trees')
                length_yesorno = len(subset['Play Tennis'])
        return (feature,yes_len,no_len,length_yesorno,num_categories)

feature_name,feature_yes,feature_no,bothyn,num_cat = decision_tree('Outlook')
if(feature_yes > feature_no):
    print('Yes for {}'.format(feature_name))
else:
    print('No for {}'.format(feature_name))


##**Parts Of Speech Tagging**##

words_dict = {'noun':['Vikas','John','Padma','karthik'],
              'adj':['beautiful','ugly','pretty','messy'],
              'verb':['eating','swim','drinking','play']}

sentences = ['Vikas is in a swim','karthik is in a play ground',
              'John is drinking water','Padma is eating',
              'she is beautiful , karthik !','The place is messy','darkness is ugly']

def pos_tag(sentence):
    split_sen = sentence.split()
    noun_tuple = {}
    adj_tuple = {}
    verb_tuple = {}
    uncategorized = {}
    for i in split_sen:
        if(i in words_dict['noun']):
            noun_tuple.update({i:'noun'})
        elif(i in words_dict['adj']):
            adj_tuple.update({i:'adj'})
        elif(i in words_dict['verb']):
            verb_tuple.update({i:'verb'})
        else:
            uncategorized.update({i:'not tagged'})
    result = [noun_tuple,adj_tuple,verb_tuple]
    return (result,uncategorized)

f_res0 = pos_tag(sentences[0])
f_res1 = pos_tag(sentences[1])
f_res2 = pos_tag(sentences[2])
f_res3 = pos_tag(sentences[3])
f_res4 = pos_tag(sentences[4])
f_res5 = pos_tag(sentences[5])
f_res6 = pos_tag(sentences[6])

##K-means clustering hands on in Python Programming##

a = list(np.random.randn(20))
b = list(np.random.randn(20))
c = list(np.random.randn(20))

dt = {'col1': list(np.random.randn(20)),'col2':list(np.random.randn(20)),'col3':list(np.random.randn(20))}
dt_f = pd.DataFrame(dt,columns = ['col1','col2','col3'])

dt_f.columns = ['A','B','C']

k = int(input('Enter the number of clusters.......'))
print('\n')
print('Number of clusters to be formed are {}'.format(k))
means = dt_f.sample(n=k)
df = dt_f[~dt_f.isin(means)].dropna()
arr_means = np.array(means)
l = len(df)
def Kmeans(iter_means,num):
    dist_list = []
    clusters = []
    merged_dt = pd.DataFrame()
    for i in range(num):
        diff = df-iter_means[i]
        sq = diff**2
        sum_sq = np.sum(sq,axis = 1)
        dist = np.sqrt(sum_sq)
        dist_list.append(dist)
        clusters.append([iter_means[i]])
        merged_dt = pd.concat([merged_dt,dist_list[i]],axis = 1)
    merged_dt.columns = [0,1,2,3]
    min_dist = np.min(merged_dt,axis = 1)
    colindex = merged_dt.columns.where(merged_dt.isin(min_dist))
    cluster_num = colindex.dropna()
    cluster_num = cluster_num.astype(int)
    return cluster_num,clusters
 
clusternum,act_clusters = Kmeans(arr_means,k)

def means_rev(s,index_list,clust):
    for i in range(s):
        if(index_list[i] == 0):
            clust[0].append(df.iloc[i])
        elif(index_list[i] == 1):
            clust[1].append(df.iloc[i])
        elif(index_list[i] == 2):
            clust[2].append(df.iloc[i])
        else:
            clust[3].append(df.iloc[i])
    Rev_means = list(map(lambda x:np.mean(x,axis = 0),clust))
    return Rev_means

revised_means = means_rev(l,clusternum,act_clusters)