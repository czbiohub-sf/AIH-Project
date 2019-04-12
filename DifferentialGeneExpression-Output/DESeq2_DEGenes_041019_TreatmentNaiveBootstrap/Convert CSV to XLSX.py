#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import glob
import csv
from xlsxwriter.workbook import Workbook


# In[2]:


for csvfile in glob.glob(os.path.join('.', '*.csv')):
    workbook = Workbook(csvfile[:-4] + '.xlsx')
    worksheet = workbook.add_worksheet()
    with open(csvfile, 'rt', encoding='utf8') as f:
        reader = csv.reader(f)
        for r, row in enumerate(reader):
            for c, col in enumerate(row):
                worksheet.write(r, c, col)
    workbook.close()


# In[ ]:




