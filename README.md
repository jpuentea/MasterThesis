# MasterThesis

In this repository we can find the different scripts used for the development of my Master's Thesis "Exploring the influence of personal data and user activity in Facebook ad system"

- `BrowisingActivityAndAdsDetection.py`: Is a Python script that allows to automate the browsing activity in a Google Chrome account associated with an user and detect the ads received by the user in Facebook. When detecting an ad, we store in a database along with the user name and timestamp, the advertiser's name, the text of the ad, the url of the ad's image and the explanation provided by Facebook to justify the ad delivery. 
- `GetAdvertiserInfoFromFBPage.py`: Is a Python script that allows to obtain information from the advertiser's Facebook page. We automatically obtain the external website of the advertiser, its Facebook categories as well as the information present in the sections Mission, About, Company Overview and Products.
- `GetAdvertiserInfoFromExternalPage.py`: Is a Python script that allows to obtain extra information from the advertiser's official web page. We obtain information present in the web page's metadata such as the title, the keywords and the description. 
