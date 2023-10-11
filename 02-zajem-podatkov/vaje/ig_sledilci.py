import os
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL
followers_url = 'https://www.instagram.com/timdolenc/followers/'
following_url = "https://www.instagram.com/timdolenc/following/"
# mapa, v katero bomo shranili podatke
directory = "/Users/timdolenc/Desktop/PROGRAMIRANJE/programiranje-1/02-zajem-podatkov/vaje/izvozne"
# ime datoteke v katero bomo shranili glavno stran
followers_ime_datoteke = 'followers.html'
following_ime_datoteke = "following.html"
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'TODO'




import os
import re


#SELENIUM##########################################
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
DRIVER_PATH = '/path/to/chromedriver'
options = Options()
options.headless = True
options.add_argument("--window-size=1920,1200")
driver = webdriver.Chrome(options=options, executable_path=DRIVER_PATH)
####################################################

# LINKI#############################################
directory_frontpages = "/Users/timdolenc/Desktop/PROGRAMIRANJE/programiranje-1/PROJEKT_ANALIZA_PODATKOV/frontpages"
directory_tabs = "/Users/timdolenc/Desktop/PROGRAMIRANJE/programiranje-1/PROJEKT_ANALIZA_PODATKOV/tabs"
####################################################



def download_url_to_string_selenium(url):
    driver.get(url)
    vsebina = driver.page_source
    return vsebina



def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename) #združiš direktorij z imenom datoteke da nerabiš posebi lepit
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None




# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    vsebina = download_url_to_string_selenium(page)
    save_string_to_file(vsebina, directory, filename)
    print("datoteka shranjena")


save_frontpage(followers_url, directory, followers_ime_datoteke)
    


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    path = os.path.join(directory, filename)
    with open(path, "r") as f:
        vsebina = f.read()
    return vsebina

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran
    #save_frontpage(followers_url, directory, followers_ime_datoteke)
    #save_frontpage(following_url, directory, following_ime_datoteke)
    # Iz lokalne (html) datoteke preberemo podatke
    followers_vsebina = read_file_to_string(directory, followers_ime_datoteke)
    following_vsebina = read_file_to_string(directory, following_ime_datoteke)



if __name__ == '__main__': #
    main()