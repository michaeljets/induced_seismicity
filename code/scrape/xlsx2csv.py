import xlrd
import csv
import os
import re

def csv_from_excel(file):
    wb = xlrd.open_workbook(file)
    sh = wb.sheet_by_index(0)
    new_file = re.sub("xlsx", "csv", file)
    csv_file = open(new_file, 'w', newline='')
    wr = csv.writer(csv_file, delimiter=',')

    for rownum in range(sh.nrows):
        wr.writerow(sh.row_values(rownum))

    csv_file.close()

dir_name = "rawdata/wells/oklahoma"
extension = ".xlsx"

for item in os.listdir(dir_name): # loop through items in dir
    if item.endswith(extension): # check for ".xlsx" extension
        file_name = dir_name + "/" + item # get full path of files
        csv_from_excel(file_name)
        # os.remove(file_name) # delete xlsx file