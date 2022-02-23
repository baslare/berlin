# -*- coding: utf-8 -*-
"""
Created on Mon Jan 10 14:57:11 2022

@author: Efe
"""

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import re
import numpy as np
import pandas as pd

import time


import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

#Must be activated
pandas2ri.activate()


rvest = importr("rvest")
xml2 = importr("xml2")


sd = pd.read_html("https://www.dasoertliche.de/Themen/Postleitzahlen/Berlin.html")
plz_df = sd[0]


ber_df = plz_df.loc[plz_df["Ortsname"]=="Berlin"]
ber_plz = np.array(ber_df["PLZ"])


lfr_root = "https://www.lieferando.de/lieferservice/essen/berlin-"
df_list = []

driver = webdriver.Firefox()



j = 0

while j < len(ber_plz):
    
    driver.get(lfr_root + str(ber_plz[j]))
    
    time.sleep(0.2)
    
    rest_count = driver.find_element("class name","restaurant-amount")
    rest_count = int(rest_count.text)
    
    source = driver.page_source
    source_html = xml2.read_html(source)
    
    rest_list = rvest.html_nodes(rvest.html_node(source_html,"[id='irestaurantlist']"),".restaurant")
    #rest_list = pandas2ri.ri2py_vector(rest_list)

    
    #rest_list = driver.find_element(by="id",value="irestaurantlist").find_elements(by="class name",value="restaurant")
    
    while len(rest_list) < rest_count:
        driver.find_element("tag name","body").send_keys(Keys.PAGE_DOWN)
        #rest_list = driver.find_element(by="id",value="irestaurantlist").find_elements(by="class name",value="restaurant")
        rest_list = rvest.html_nodes(rvest.html_node(source_html,"[id='irestaurantlist']"),".restaurant")
  

      
    rest_details = rvest.html_node(rest_list,".detailswrapper")
    rest_n = rvest.html_node(rvest.html_node(rest_details,".restaurantname"),"a")

      
    rest_url = rvest.html_attr(rest_list,"data-url")
    rest_url = pandas2ri.ri2py_vector(rest_url)
    
    rest_name = rvest.html_text(rest_n)
    rest_name = pandas2ri.ri2py_vector(rest_name)
        
    rest_kit = rvest.html_node(rvest.html_node(rest_details,".kitchens"),"span")
    rest_kit = pandas2ri.ri2py_vector(rvest.html_text(rest_kit))
        

        #rest_url.append(rest_list[i].get_attribute("data-url"))
        #rest_name.append(rest_n.text)
        #rest_kit.append(rest_details.find_element("class name","kitchens").text)
        
        
        
        
    
    df_info = pd.DataFrame({"name":rest_name,"url":rest_url,"kitchen":rest_kit,"PLZ":ber_plz[j]}) 
    df_list.append(df_info)
    
    print(j,"of",len(ber_plz))
    j = j + 1

df_all = pd.concat(df_list,ignore_index=True)
df_all.to_csv("df_all.csv")
