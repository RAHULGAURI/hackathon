import json
from pprint import pprint
with open('CompanyPreference.json') as data_file:
    data = json.load(data_file)


##dict1={}
##nkey=1
##dict1[nkey]=data[0]
##nkey=nkey+1
##
##dict1[nkey]=data[1]
##nkey=nkey+1
##
##dict1[nkey]=data[2]
##nkey=nkey+1
##
###pprint (dict)

x=data[0]
print (x[0])

def display():
    return (x)

def writeGfile():
    f=open("Testfile.txt","w")
    f.write("Google")
    f.close()




##for k,v in dict.items():
##    if "GOOG" in v:
##        ticker=dict[1][0]
##        openval=dict[1][1]
##        currval=dict[1][2]
##        vol=dict[1][3]
##        change1=dict[1][4]
##        change2=dict[1][5]
##
##print(ticker,openval,currval,vol,change1,change2)
##p


