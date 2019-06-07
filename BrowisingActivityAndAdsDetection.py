# Master Thesis
# Author: Javier Puente Aller
# Title: Obtention of advertisements and Browsing activity
# Resume: Script that allows to automate the browsing activity and the detection and store all the advertisements each user receives.
# Date

import time
import sqlite3
import re
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from bs4 import BeautifulSoup as BS
from selenium.webdriver.common.action_chains import ActionChains
from random import randint
from urllib.parse import  urlparse
from googletrans import Translator

def browsingActivity():
    # creation of the db
    conn = sqlite3.connect('pagesvisited.db')
    # cursor of the db
    c = conn.cursor()

    c.execute("CREATE TABLE IF NOT EXISTS visited (Id, TimeStamp Text, PageVisited Text)")

    man_archivo = open(usersite)

    for linea in man_archivo:
        try:
            driver.get(linea)
        except:
            print("link failed")
        c.execute(
            "INSERT INTO visited (Id, TimeStamp, PageVisited) VALUES (\"" + profile + "\", datetime('now','localtime'), \"" + linea + "\")")
        conn.commit()
        counter = 0
        error = 0
        linea = linea.replace("\n","")
        while (counter <= 2 and error <= 5):
            try:

                buttons = driver.find_elements_by_css_selector('a[href]')
                random = randint(0, buttons.__len__())

                if (buttons.__len__() > 0):
                    try:
                        element = buttons.__getitem__(random.__round__())
                    except:
                        pass
                    Href = element.get_attribute("href")
                    parsedHref = urlparse(Href)
                    result = '{uri.scheme}://{uri.netloc}'.format(uri=parsedHref)

                    #Check that the href remains in the same domain.
                    if(result == linea):

                        try:
                            driver.execute_script('arguments[0].click();', element)
                        except:
                            print("wrong click")
                        time.sleep(1)
                counter += 1
                Tabs = driver.window_handles

                if(Tabs.__len__() > 1):
                    for i in range(1, Tabs.__len__(), 1):
                        driver.switch_to.window(Tabs[i])
                        driver.close()
                    driver.switch_to.window(Tabs[0])

            except:
                error +=1
                pass

def AdsDetection():
    conn2 = sqlite3.connect('interests.db')
    c2 = conn2.cursor()
    c2.execute("CREATE TABLE IF NOT EXISTS adsFinal(TimeStamp Text, UserId Text, Advertiser Text,AdText Text, Explanation Text, AdvertiserFbPage Text, AdvertImg Text, Info Text)")
    driver.get("https://www.facebook.com/")
    actions = ActionChains(driver)
    # Detection of the user's facebook name.
    UserId = driver.find_element_by_class_name('_5afe').text
    start_time_is = time.time()
    idPostsVisitedArray = []

    # Counter to see number of ads detected
    adsDetected = 0

    lateralPosts = driver.find_elements_by_xpath("//*[@class='_3653']")

    for lateralPostx in lateralPosts:
        if(lateralPostx.text == "Sponsored"):
            print("lateral ad detected")

            optionsbutton = driver.find_element_by_xpath("//*[@class='uiSelectorButton uiCloseButton']")

            driver.execute_script('arguments[0].scrollIntoView(true);', optionsbutton)
            time.sleep(2)
            driver.execute_script('arguments[0].scrollIntoView(true);', optionsbutton)
            driver.execute_script('arguments[0].scrollIntoView(true);', optionsbutton)
            driver.execute_script('arguments[0].click();', optionsbutton)
            labels = driver.find_elements_by_xpath("//*[@class='itemLabel fsm']")
            if(labels.__len__() > 1):
                driver.execute_script('arguments[0].click();',labels[1])
                explanation = driver.find_element_by_class_name('_4v6n')
                advertiser = driver.find_element_by_id('ad_prefs_advertiser').text
                print("Advertiser: " + advertiser)
                text = explanation.find_element_by_css_selector('div').text
                print("Text: " + text)
                print("AdText: "+adText)
                c2.execute(
                    "INSERT INTO adsFinal (TimeStamp, UserId, Advertiser, Explanation,Info) VALUES ( datetime('now','localtime'),\"" + UserId + "\",\"" + advertiser + "\", \"" + text + "\",'Lateral ad detected')")
                conn2.commit()
                # Commit into the database finished correctly.
                actions.move_by_offset(10, 10).click().perform()
                time.sleep(2)

    while ((time.time() - start_time_is) < 60):

        # Selection of each post appearing in the news feed..
        post = driver.find_elements_by_xpath("//*[@class='_1dwg _1w_m _q7o']")

        # Counter to see which post I am in
        counter = 0

        for postx in post:
            counter += 1
            # obtention of the HTML source code as string.
            HtmlPost = postx.get_attribute('innerHTML')
            # obtention of the HTML source code.
            Html = postx.get_attribute('outerHTML')
            soup = BS(Html)
            adImg = ""
            adText = ""
            advertiserFacebookPage = ""
            advertiser = ""

            try:

                #isad = soup.find('a', {'class': '_5pcq'}).text
                isad = soup.find('span', {'class': 'fsm fwn fcg'}).text

                #print(isad)
                #print(len(isad))
            except:
                #print("error in isad")
                isad = ""
                pass
            try:
                try:
                    idPostVisited = soup.find('div', {'class': '_4r_y'}).get('id')
                except:
                    pass

                if (not idPostVisited in idPostsVisitedArray):
                    idPostsVisitedArray.append(idPostVisited)
                    if(re.findall(r'\d',isad).__len__() ==0):
                        try:
                            # Obtention of the name of the advertiser present in the label span in class fwb fcg.
                            a = soup.find('span', {'class':'fwb fcg'})
                            b = str(a)
                            advertiserFacebookPage = re.findall(r'href=\"(.*)\"',b)[0]
                            advertiserFacebookPage = advertiserFacebookPage.rsplit('?')[0]


                        except:
                            print("error 1")

                        #detection of the advertiser image, video link or fist link of multiple images.

                        try:
                            adImg = soup.find('img', {'class': 'scaledImageFitWidth img'}).get('src')
                        except:
                            pass
                        try:
                            adImg = soup.find('img', {'class': '_4lpf'}).get('src')
                        except:
                            pass
                        try:
                            adImg = soup.find('img', {'class': '_kvn img'}).get('src')
                        except:
                            pass


                        try:
                            adText = soup.find('div', {'class': '_5pbx userContent _3576'}).text.replace('"',"")
                            adText = deEmoji(adText)
                            adText = Translate(adText)
                        except:
                            print("Error 2")



                        # Obtention of the button id to expand the option why am I seeing this ad.
                        id = soup.find('a', {'class': '_4xev _p'}).get('id')
                        button = driver.find_element_by_id(id)
                        # button.click()

                        # Scroll into the element to avoid errors.
                        driver.execute_script('arguments[0].scrollIntoView(true);', button)
                        time.sleep(2)
                        driver.execute_script('arguments[0].scrollIntoView(true);', button)
                        driver.execute_script('arguments[0].scrollIntoView(true);', button)
                        driver.execute_script('arguments[0].scrollIntoView(true);', button)
                        driver.execute_script("window.scrollBy(0,-200 );")

                        time.sleep(2)

                        button.click()
                        time.sleep(1)
                        # Selection of the menu where the option why am I seeing this ad appears.
                        menu = driver.find_elements_by_xpath("//*[@class='_54ng']")
                        # Interested in the last element found
                        position = menu.__len__() - 1
                        postOptions = menu[position]
                        options = postOptions.find_elements_by_class_name('_54nc')
                        if (options.__len__() > 2 & options.__len__() < 6):
                            why = options[3].find_element_by_class_name('_2ezy').text
                            why2 = options[2].find_element_by_class_name('_2ezy').text
                            try:
                                why3 = options[4].find_element_by_class_name('_2ezy').text
                            except:
                                why3 = ""
                                print("Error why3")

                            if (why == "Why am I seeing this ad?"):
                                adsDetected += 1
                                print("Ad detected")
                                time.sleep(2)
                                driver.execute_script('arguments[0].click();', options[3])
                                time.sleep(2)
                                explanation = driver.find_element_by_class_name('_4v6n')
                                text = explanation.find_element_by_css_selector('div').text

                                try:
                                    explanation2 = driver.find_element_by_class_name('_4hcd')
                                    text2 = explanation2.find_element_by_css_selector('span').text
                                    totaltext = text + '' + text2

                                except:
                                    totaltext = text

                                advertiser = driver.find_element_by_id('ad_prefs_advertiser').text
                                print("Advertiser: " + advertiser)
                                totaltext = Translate(totaltext).replace('"',"")

                                print("Text: " + totaltext)
                                print("ADText: " + adText)
                                print("UserId" + UserId)
                                print("AdvertiserFacebookPage: " + advertiserFacebookPage)
                                print("AdvertiserImage: " + adImg)
                                c2.execute(
                                    "INSERT INTO adsFinal(TimeStamp, UserId, Advertiser, AdText, Explanation, AdvertiserFbPage, AdvertImg, Info) VALUES (datetime('now','localtime'), \"" + UserId + "\", \"" + advertiser + "\", \"" + adText + "\", \"" + totaltext + "\", \"" + advertiserFacebookPage + "\", \"" + adImg + "\",'Regular ad detected')")
                                print("Number of ads detected: ")
                                print(adsDetected)

                                conn2.commit()
                                # Commit into the database finished correctly.
                                time.sleep(1)
                                # actions.move_by_offset(0, 20).click().perform()
                                driver.find_element_by_css_selector(
                                    "body > div._10.uiLayer._4-hy._3qw > div._59s7 > div > div > div > div._4-i0._26c5 > div > div._51-u.rfloat._ohf > a").click()

                                # driver.execute_script('arguments[0].click();', close_button)

                                adText = ""
                                why = ""

                                time.sleep(2)
                            if (why2 == "Why am I seeing this ad?"):
                                adsDetected += 1
                                print("Ad detected")
                                time.sleep(2)
                                driver.execute_script('arguments[0].click();', options[2])
                                time.sleep(2)
                                explanation = driver.find_element_by_class_name('_4v6n')
                                text = explanation.find_element_by_css_selector('div').text

                                try:
                                    explanation2 = driver.find_element_by_class_name('_4hcd')
                                    text2 = explanation2.find_element_by_css_selector('span').text
                                    totaltext = text + '' + text2

                                except:
                                    totaltext = text

                                advertiser = driver.find_element_by_id('ad_prefs_advertiser').text
                                print("Advertiser: " + advertiser)
                                totaltext = Translate(totaltext).replace('"',"")

                                print("Text: " + totaltext)
                                print("ADText: " + adText)
                                print("UserId" + UserId)
                                print("AdvertiserFacebookPage: " + advertiserFacebookPage)
                                print("AdvertiserImage: " + adImg)
                                c2.execute(
                                    "INSERT INTO adsFinal(TimeStamp, UserId, Advertiser, AdText, Explanation, AdvertiserFbPage, AdvertImg, Info) VALUES (datetime('now','localtime'), \"" + UserId + "\", \"" + advertiser + "\", \"" + adText + "\", \"" + totaltext + "\", \"" + advertiserFacebookPage + "\", \"" + adImg + "\",'Regular ad detected')")
                                print("Insertado correctamente")
                                print("Numero de anuncios detectados: ")
                                print(adsDetected)

                                conn2.commit()
                                # Commit into the database finished correctly.
                                time.sleep(1)
                                # actions.move_by_offset(0, 20).click().perform()
                                driver.find_element_by_css_selector(
                                    "body > div._10.uiLayer._4-hy._3qw > div._59s7 > div > div > div > div._4-i0._26c5 > div > div._51-u.rfloat._ohf > a").click()

                                # driver.execute_script('arguments[0].click();', close_button)

                                adText = ""
                                why2 = ""

                                time.sleep(2)
                            if (why3 == "Why am I seeing this ad?"):
                                adsDetected += 1
                                print("Ad detected")
                                time.sleep(2)
                                driver.execute_script('arguments[0].click();', options[4])
                                time.sleep(2)
                                explanation = driver.find_element_by_class_name('_4v6n')
                                text = explanation.find_element_by_css_selector('div').text

                                try:
                                    explanation2 = driver.find_element_by_class_name('_4hcd')
                                    text2 = explanation2.find_element_by_css_selector('span').text
                                    totaltext = text + '' + text2

                                except:
                                    totaltext = text

                                advertiser = driver.find_element_by_id('ad_prefs_advertiser').text
                                print("Advertiser: " + advertiser)
                                totaltext = Translate(totaltext).replace('"',"")

                                print("Text: " + totaltext)
                                print("ADText: " + adText)
                                print("UserId" + UserId)
                                print("AdvertiserFacebookPage: " + advertiserFacebookPage)
                                print("AdvertiserImage: " + adImg)
                                c2.execute(
                                    "INSERT INTO adsFinal(TimeStamp, UserId, Advertiser, AdText, Explanation, AdvertiserFbPage, AdvertImg, Info) VALUES (datetime('now','localtime'), \"" + UserId + "\", \"" + advertiser + "\", \"" + adText + "\", \"" + totaltext + "\", \"" + advertiserFacebookPage + "\", \"" + adImg + "\",'Regular ad detected')")
                                print("Insertado correctamente")
                                print("Numero de anuncios detectados: ")
                                print(adsDetected)

                                conn2.commit()
                                # Commit into the database finished correctly.
                                time.sleep(1)
                                # actions.move_by_offset(0, 20).click().perform()
                                driver.find_element_by_css_selector(
                                    "body > div._10.uiLayer._4-hy._3qw > div._59s7 > div > div > div > div._4-i0._26c5 > div > div._51-u.rfloat._ohf > a").click()

                                # driver.execute_script('arguments[0].click();', close_button)

                                adText = ""
                                why3 = ""

                                # close_button = driver.find_element_by_xpath("//*[@class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-']").click()
                                # driver.execute_script('arguments[0].click();', close_button)
                                time.sleep(2)

            except:
                pass

        driver.execute_script("window.scrollBy(0,document.body.scrollHeight );")

    if(adsDetected == 0):
        c2.execute("INSERT INTO adsFinal(TimeStamp, UserId, Info) VALUES (datetime('now','localtime'), \""+UserId+"\",'No ads detected at this Timestamp')")
        conn2.commit()


def Translate(textToTranslate):
    textToTranslate = translator.translate(textToTranslate, dest='en').text
    return textToTranslate

def deEmoji(input):
    return input.encode('ascii','ignore').decode('ascii')


profiles = ["Mark","Lisa"]


for profile in profiles:
    options = webdriver.ChromeOptions()
    options.add_argument(r"user-data-dir=/home/experiment/.config/google-chrome/"+profile+"")
    chrome_path=r"/home/experiment/Downloads/chromedriver"
    driver = webdriver.Chrome(chrome_path,options = options)
    translator = Translator()
    driver.get("https://www.facebook.com")
    usersite=""+profile+"sites.txt"
    print(usersite)

    browsingActivity()
    AdsDetection()
