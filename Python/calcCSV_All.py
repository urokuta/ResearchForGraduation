#!/usr/bin/python
# coding: UTF-8
 
 
import csv
import sys
import bsModel
#init = bsModel.BS_Call_IV_init(18000, 17500, float(30)/365, 0.004);
#iv = bsModel.BS_Call_IV(18000, 17500, float(30)/365, 0.004, 900, init);


for i in range(12):
    filename = "createData_"+ str((i+1)*10) +".csv"
    csvfile = open(filename, "rU")
    print csvfile
     
    dataList = []

    filename = "result_" + filename
    writecsv = csv.writer(file(filename, 'w'), lineterminator='\n')

    for row in csv.reader(csvfile):
        #print "--- start calc IV ---"
        
        try:
            K = int(row[3])
            Pr = int(row[5])
            T = float(row[9])/365
            S = float(row[10])
            R = 0.004
            #K = int(row[0])
            #Pr = int(row[1])
            #T = float(row[2])/365
            #S = float(row[3])
            #R = 0.004
            init = bsModel.BS_Call_IV_init(S, K, T, R)
            #print "init = " + str(init)
            #print "--- start newton calc ---"
            iv = bsModel.BS_Call_IV(S, K, T, R, Pr, init)
            #print "--- calc end ---"
            print "iv = " + str(iv)
            print "\n\n"
        except ZeroDivisionError:
            dataList.append(0)
            continue
        except ValueError:
            dataList.append(0)
            continue
        
        dataList.append([K, Pr, T, S, iv])
        #print dataList
    for row in dataList:
        #print row
        if isinstance(row, list):
          writecsv.writerow(row)
        else:
          writecsv.writerow([row])
