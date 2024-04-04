from bs4 import BeautifulSoup
from IPython.display import HTML
import pandas as pd
import requests
import sys
import time
#import os
#os.makedirs("python/resultados/")
import csv
#sys.path.insert(0,'/usr/lib/chromium-browser/chromedriver')

from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait 
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
url_base = 'https://www.indec.gob.ar/indec/web/Calendario-Fecha-0'

service = Service(executable_path=r'/usr/bin/chromedriver')
options = webdriver.ChromeOptions() # Usamos chrome, se podria usar otro.
options.add_argument('--headless') # Chromium sin interfaz grafica
options.add_argument('--no-sandbox') # Seguridad
options.add_argument('--disable-dev-shm-usage') # configuracion de linux
options.add_argument('--user-agent=""Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.157 Safari/537.36""') # user agent
#driver = webdriver.Chrome(service=service,options=options)

driver = webdriver.Chrome()
driver.get('https://www.indec.gob.ar/indec/web/Calendario-Fecha-0')
# Busco el boton del mes en español
#boton_mes = driver.find_element(By.ID, 'btn-mes')

# Aca lo que hacemos es usar las funciones de `expected_conditions` para no clickear hasta que se haya cargado el elemento
WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, "//*[@id='btn-mes']")))

boton_mes= driver.find_element(By.XPATH, "//*[@id='btn-mes']")
# Le hago click
boton_mes.click()

#Screenhot para chequear (hecho rapido, mejorar) 
#driver.save_screenshot('screenshot_indec.png')


##Datos calendario en texto (hecho asi nomas, mejorar) 
time.sleep(4)
datos_calendario = driver.find_element(By.ID,'DatosCalendario')
calendario_texto = datos_calendario.text

driver.quit()
print(f'Datos:\n {calendario_texto}')

with open('python/resultados/calendario.csv', 'w') as out_file:
        writer = csv.writer(out_file)
        writer.writerow(calendario_texto)
                            
                            
