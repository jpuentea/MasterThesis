# Master Thesis
# Author: Javier Puente Aller
# Title: Obtention of Advertiser tags in its Facebook page.
# Resume: The objective of this script is to navigate through the advertiser's Facebook page detecting the tags it belongs to.
# Date

import time
import sqlite3
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from bs4 import BeautifulSoup as BS
from selenium.webdriver.common.action_chains import ActionChains
import re
from googletrans import Translator

# Selenium configuration to use the desired user's account.
options = webdriver.ChromeOptions()
options.add_argument(r"user-data-dir=C:\Users\javie\AppData\Local\Google\Chrome\User Data\Default")
chrome_path=r"D:\javier\Documents\TELECO\PyCharm workspace\chromedriver_win32\chromedriver.exe"
driver = webdriver.Chrome(chrome_path,options=options)

def Translate(textToTranslate):
    textToTranslate = translator.translate(textToTranslate, dest='en').text
    return textToTranslate

#creation of the db
conn = sqlite3.connect('interestsBrowsingAndDetection.db')
#cursor to use the db
c = conn.cursor()
c.execute("CREATE TABLE IF NOT EXISTS advertiserInfoFinal(TimeStamp Text,Advertiser Text,Mission Text, About Text, CompanyOverview Text, Products Text, Tags Text, ExternalPage Text)")



pages = c.execute("SELECT AdvertiserFbPage FROM adsFinal WHERE AdvertiserFbPage != 'NULL'").fetchall()
translator = Translator()

for i in range(0,len(pages),1):

    # Navigate to the advertiser page detected when the ad was shown to the user.


    page = pages[i][0]

    anunciante = c.execute("SELECT Advertiser FROM adsFinal Where AdvertiserFbPage == \"" + page + "\"").fetchone()[0]
    print(anunciante)
    page = page.rsplit('?')[0]
    if (c.execute("SELECT Advertiser FROM advertiserInfoFinal WHERE Advertiser == \"" + anunciante + "\"").fetchall().__len__() == 0):
        page += str("about")
        print(page)
        driver.get(page)
        # Selection of the About tab. The class is _2yau and the label is in the second place.
        time.sleep(1)
        Advertiser = driver.find_element_by_xpath("//*[@class='_64-f']").text
        externalAdvertiserPage = ""
        MultipleOptions = driver.find_elements_by_xpath("//*[@class='_50f4']")
        for multiplex in MultipleOptions:
            multipletext = str(multiplex.text)

            if(multipletext.find("http") != -1):
                externalAdvertiserPage = multipletext
                print("External page: " +externalAdvertiserPage)

        print("Advertiser: "+Advertiser)


        #Detection of the Tags of the advertiser:
        Tags = driver.find_element_by_xpath("//*[@class='_4bl9 _5m_o']")
        Tags = Tags.text
        print(Tags)

        MoreInfo = driver.find_elements_by_xpath("//*[@class='_4bl9']")
        Mission = ""
        About = ""
        CompanyOverview = ""
        Products = ""
        for MoreInfox in MoreInfo:
            HtmlInfo = MoreInfox.get_attribute('outerHTML')
            soup = BS(HtmlInfo)
            try:
                getTitle = soup.find('div',{'class':'_50f4'}).text
                time.sleep(1)
                if(getTitle == "Mission"):
                    Mission = soup.find('div',{'class':'_3-8w'}).text
                    Mission = Translate(Mission)
                    Mission = Mission.replace('"',"")
                    print("Mission: "+Mission)
                elif(getTitle == "About"):
                    About = soup.find('div', {'class': '_3-8w'}).text
                    About = Translate(About)
                    About = About.replace('"',"")
                    print("About: "+About)
                elif(getTitle == "Company Overview"):
                    CompanyOverview = soup.find('div', {'class': '_3-8w'}).text
                    CompanyOverview = Translate(CompanyOverview).replace('"',"")
                    print("Company Overview: "+CompanyOverview)
                elif(getTitle == "Products"):
                    Products = soup.find('div', {'class': '_3-8w'}).text
                    Products = Translate(Products)
                    print("Products: "+Products)
            except:
                pass
        c.execute("INSERT INTO advertiserInfoFinal (TimeStamp,Advertiser ,Mission , About , CompanyOverview , Products , Tags, ExternalPage) VALUES ( datetime('now', 'localtime'),\"" + Advertiser + "\",\"" + Mission + "\",\"" + About + "\",\"" + CompanyOverview + "\", \"" + Products + "\", \"" + Tags + "\", \"" + externalAdvertiserPage + "\")")
        conn.commit()

