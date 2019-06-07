from urllib.request import urlopen, Request
import urllib.request
import urllib.parse
from bs4 import BeautifulSoup
from googletrans import Translator
import sqlite3

def ObtainHtmlFromUrl(url):
    try:
        headers = {}
        headers['User-Agent'] = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)     Chrome/37.0.2049.0 Safari/537.36"

        try:
            req = urllib.request.Request(url,headers=headers)
            resp = urllib.request.urlopen(req)
            resp_data = resp.read()

            soup = BeautifulSoup(resp_data)
        except:
            pass
        try:
            title = Translate(soup.title.text)
            print(title)
            description = soup.find('meta',{'name':'description'}).get('content')
            description = Translate(description)
            print("Description: "+description)

            keywords = soup.find('meta',{'name':'keywords'}).get('content')
            keywords = Translate(keywords)
            print("keywords: "+keywords)
        except:
            print("no keywords found")
        print(Advertiser)
        try:
            c.execute("UPDATE advertiserInfoFinal SET Keywords = \"" + keywords + "\" WHERE Advertiser == \"" + Advertiser + "\"")
            conn.commit()

        except:
            print("error 1 found")

        try:
            c.execute(
                "UPDATE advertiserInfoFinal SET  Description = \"" + description + "\" WHERE Advertiser == \"" + Advertiser + "\"")
            conn.commit()
        except:
            print("error 2 found")
    except:
        print("error 3 found")
def Translate(textToTranslate):
    textToTranslate = translator.translate(textToTranslate, dest='en').text
    return textToTranslate





#creation of the db
conn = sqlite3.connect('interestsBrowsingAndDetection.db')
#cursor to use the db
c = conn.cursor()
try:
    c.execute("ALTER TABLE advertiserInfoFinal ADD Keywords Text")

except:
    print("column already created")
try:

    c.execute("ALTER TABLE advertiserInfoFinal ADD Description Text")
except:
    print("description already created")

translator = Translator()
pages = c.execute("SELECT ExternalPage FROM advertiserInfoFinal").fetchall()
for i in range(0,len(pages),1):
    print(pages[i][0])
    Advertiser = c.execute("SELECT Advertiser FROM advertiserInfoFinal WHERE ExternalPage == \"" + pages[i][0] + "\"").fetchall()[0][0]
    print(Advertiser)
    ObtainHtmlFromUrl(pages[i][0])