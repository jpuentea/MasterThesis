# Master Thesis
# Author: Javier Puente Aller
# Title: Getting Interests
# Resume: Script that allows to detect and store all the interests a user has been tagged with.

import time
import sqlite3
import datetime
from selenium import webdriver
from selenium.webdriver import ActionChains
from bs4 import BeautifulSoup as BS


# Selenium configuration to use the desired user's account.
# Insert the profile name from which to obtain information
profiles = ["Sam Smith"]

for profile in profiles:

    options = webdriver.ChromeOptions()
    options.add_argument(r"user-data-dir=/home/experiment/.config/google-chrome/"+profile+"")
    chrome_path=r"/home/experiment/Downloads/chromedriver"
    driver = webdriver.Chrome(chrome_path,options = options)
    driver.get('https://www.facebook.com/ads/preferences/?entry_product=ad_settings_screen')
    #creation of the db
    conn = sqlite3.connect('interestsTagged.db')
    # #cursor to use the db
    c = conn.cursor()

    # Storage of the UserId.
    UserId = profile

    time.sleep(3)
    #Click on Your Interests option.
    Interests = driver.find_elements_by_xpath("//div[@class='_2qo6']")
    driver.execute_script('arguments[0].click();',Interests[0])

    Interest = driver.find_elements_by_xpath("//*[@class='_3m1v _468f']")

    # Table creation. Columns Timestamp UserId  Category Value.
    c.execute("CREATE TABLE IF NOT EXISTS interests(Inserted Text, TimeStamp Text, UserId Text, Category Text, Value Text, Explanation Text)")

    for Interestx in Interest:

        driver.execute_script('arguments[0].click();', Interestx)
        # Name of the section (Business and Industry, News and entertainment...)
        try:
            more = driver.find_element_by_xpath("//*[@class='_45yq _5946']")
            while(True):
                driver.execute_script('arguments[0].click();', more)

        except:
            print("not see more")
        try:
            a = Interestx.find_element_by_class_name('_4xjz')
            ClassInterest = a.text.replace("'"," ")
            print("ClassInterest:")
            print(ClassInterest)

            Hovers = driver.find_elements_by_xpath("//*[@class='_2b2p _4jy0 _4jy3 _517h _51sy _42ft']")
            time.sleep(1)
            for Hover in Hovers:
                ActionChains(driver).move_to_element(Hover).perform()
                time.sleep(0.8)

            Tags = driver.find_elements_by_xpath("//div[@class='_2b2e']//img")
            explanations = driver.find_elements_by_xpath("//*[@class='_3d6x']")

            for i in range(0, len(Tags), 1):
                Tagged = Tags[i].get_attribute('alt')
                htmlexplanation = explanations[i].get_attribute('outerHTML')
                soup = BS(htmlexplanation)
                explanation = soup.find('div', {'class': '_3d6x'}).text
                print("Explanation: "+explanation)


                #hover = driver.find_element_by_xpath("//*[@class='_2b2p _4jy0 _4jy3 _517h _51sy _42ft']")

                print(Tagged)

                try:
                    # We check if we have to update or insert the element for the first time
                    exists = c.execute(
                        "SELECT UserId FROM interests WHERE Category = \"" + ClassInterest + "\" and Value=\"" + Tagged + "\"").fetchall()
                    print("USERID")
                    print(UserId)
                except:
                    exists = []

                empty = []
                if (exists == empty):
                    c.execute(
                        "INSERT INTO interests (Inserted, TimeStamp, UserId, Category, Value, Explanation) VALUES ( datetime('now','localtime'), datetime('now','localtime'),\"" + UserId + "\",\"" + ClassInterest + "\", \"" + Tagged + "\", \"" + explanation + "\")")
                    conn.commit()
                else:
                    c.execute(
                        "UPDATE interests SET TimeStamp = datetime('now','localtime') WHERE Category = \"" + ClassInterest + "\" and Value = \"" + Tagged + "\"")
                    conn.commit()



        except:
            pass
    driver.close()
