#####################################################################
# To quickly download all MICS Data, it is necessary to 
# automate a web browser with selenium
# This may require some set up:

from selenium import webdriver
import time

################################
#Download data
################################

driver = webdriver.Firefox()

driver.get('https://mics.unicef.org/surveys')

#Now click on one of the surveys that say "Available" under "Datasets" in order to prompt a login

pages = ["1", "2", "3", "4", "5", "..."]  #As of 01/15/2021, there are six pages, and the way to get to the sixth is to click "...".  This will need to be updated to paginate better if MICS ever has more than six pages.

def clickpage(page):    
    pagebutton = driver.find_element_by_link_text(page)
    
    pagebutton.click()
    
    time.sleep(2)

def downloadDatasets():
    elements = driver.find_elements_by_partial_link_text('Available')
    
    for elem in elements:
        elem.click()
        time.sleep(1)

for page in pages:
    clickpage(page)
    downloadDatasets()

# cd ~/mortalityblob/micszip
# for f in *.zip; do unzip -d "../micsraw/${f%*.zip}" "$f"; done

