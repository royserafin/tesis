from urllib.request import Request, urlopen
from bs4 import BeautifulSoup as soup
import re
import time
import mmap

# Archivo resultado
filename = 'terrenos.csv'

#Contador de requests
requests = 0

t0 = 0
for pag in range(1,1998):

    # Creando string de página de búsqueda
    urlstr = f'https://www.lamudi.com.mx/terreno/for-sale/?page={pag}'
    print(urlstr)
    requests += 1

    t_pag =time.time()-t0
    print("Tiempo entre páginas" + str(t_pag))
    if (time.time()-t0)<1:
        print("Esperando (pag)")
        time.sleep(100)
    t0=time.time()
    # Obteniendo html
    req = Request(urlstr, headers={'User-Agent': 'Mozilla/5.0'})
    client = urlopen(req)
    html_byte = client.read()
    client.close()


    # Decoding a utf-8
    html = html_byte.decode('utf-8')

    # Creando objeto BeautifulSoup
    html = soup(html, 'html.parser')
    i = 0
    t1=0
    # Iteración sobre terrenos encontrados
    for terreno in html.html.body.findAll('div', {'class':'row ListingCell-row'}):

        # Obteniendo variables título, dirección, descripción, precio y tamaño
        try:
            title = terreno.find('a', {'class':'js-listing-link'})['title']
            title = title.replace("  ","")
            title = title.replace(",", ";")
            title = title.replace("\r\n", "||")
            title = title.replace("\n", "||")
        except:
            title = 'NA'
            print('No se pudo obtener título' + str(i))


        try:
            address = (terreno.
                find('div', {'class':'ListingCell-KeyInfo-address ellipsis'}).
                select('a > span')[1].get_text(strip = True))
            address = address.replace("  ","")
            address = address.replace(",",";" )
            address = address.replace("\r\n", "||")
            address = address.replace("\n", "||")
        except:
            address = 'NA'
            print('No se pudo obtener dirección' + str(i))

        try:
            regex = re.compile('ListingCell-shortDescription.*')
            descripcion = (terreno.
                find('div', {'class': regex}))
            descripcion = descripcion.find('a', {'class':'js-listing-link'}).get_text(strip = True)
            descripcion = descripcion.replace("  ","")
            descripcion = descripcion.replace(",",";" )
            descripcion = descripcion.replace("\r\n", "||")
            descripcion = descripcion.replace("\n", "||")
        except:
            descripcion = 'NA'
            print('No se pudo obtener descripción' + str(i))

        try:
            regex = re.compile('PriceSection.*')
            precio = (terreno.
                find('span', {'class': regex}).get_text())
            precio = precio.replace("  ","")
            precio = precio.replace(",",";" )
            precio = precio.replace("\r\n", "||")
            precio = precio.replace("\n", "||")
        except:
            precio = 'NA'
            print('No se pudo obtener precio' + str(i))

        try:
            regex = re.compile('KeyInformation-value.*')
            tamaño = (terreno.
                find('span', {'class': regex}).get_text(strip=True))
            tamaño = tamaño.replace("  ","")
            tamaño = tamaño.replace(",",";" )
            tamaño = tamaño.replace("\r\n", "||")
            tamaño = tamaño.replace("\n", "||")
            #print(tamaño)
        except:
            tamaño = 'NA'
            print('No se pudo obtener tamaño' + str(i))

        with open(filename,'rb',0) as f, mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ) as s:
            if s.find(bytes(title, 'utf-8')) != -1:
                print("Terreno repetido")
                continue
        # Obteniendo latitud y longitud
        try:

            url_terreno = terreno.find('a', {'class':'js-listing-link'})['href']
            #print(url_terreno)
            requests += 1

            t_ubic =time.time()-t1
            print("Tiempo entre terrenos" + str(t_ubic))
            if (t_ubic)<1:
                print("Esperando (loc)")
                time.sleep(100)
            t1 = time.time()
            # Obteniendo html
            req = Request(url_terreno, headers={'User-Agent': 'Mozilla/5.0'})
            client = urlopen(req)
            html_byte = client.read()
            client.close()


            # Decoding a utf-8
            html = html_byte.decode('utf-8')

            # Creando objeto BeautifulSoup
            html = soup(html, 'html.parser')

            coords = html.find('div', {'id':'js-developmentMap'})
            #print(coords)
            latitud = coords['data-lat']
            longitud = coords['data-lng']

        except:
            latitud = 'NA'
            longitud = 'NA'
            print('No se pudo obtener coordenadas' + str(i))

        with open(filename, "a", encoding="utf-8") as file:
            file.write(f'{title},{address},{descripcion},{precio},{tamaño},{latitud},{longitud}\n')


        i+=1
print('Requests totales' + str(requests))
