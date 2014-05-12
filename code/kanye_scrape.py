#######################################################
#		Parsing rapgenius.com with Python. 
#		Example code for 'pyong' 
#	
#	<div class="pyong_button_helper">
#	  <span class="pyongs_count" tooltip="/songs/330261/show_pyongers">
#		0
#	  </span>
#	</div>
#
#
#
#			Maintained by Nelson Auner (2014) 
#			To do: 
#				1) cache results before parsing
#
########################################################

import csv, string
from bs4 import BeautifulSoup
from urllib2 import urlopen

kanye_url = "http://rapgenius.com/artists/Kanye-west"
BASE_URL = "http://rapgenius.com"
html = urlopen(base_url).read()

import collections

def flat_gen(x):
    def iselement(e):
        return not(isinstance(e, collections.Iterable) and not isinstance(e, str))
    for el in x:
        if iselement(el):
            yield el
        else:
            for sub in flat_gen(el): yield sub

def get_album_links(section_url,base_url):
    html = urlopen(section_url).read()
    soup = BeautifulSoup(html, "lxml")
    album = soup.find("ul", "album_list")
    category_links = [base_url + li.a["href"] for li in album.findAll("li")]
    return category_links
	
	
albums = get_album_links(kanye_url,BASE_URL)	
	
def get_songs(album_url, BASE_URL):
	html = urlopen(album_url).read()
	soup = BeautifulSoup(html, "lxml")
	songs = soup.find("ul", "song_list")
	song_links = [BASE_URL  + a.get('href') for a in songs.findAll("a")]
	return song_links

#TEST:	
#get_songs(albums[1],BASE_URL)

all_songs = list(flat_gen([get_songs(album,BASE_URL) for album in albums]))


def parse_lyrics(messy_string):
	#TODO: remove (..) e.g. (2X) 
	#remove unicdoe
	messy_string.encode('ascii','ignore') 
	#use re to remove [...]
	just_lyrics = re.sub(r'\[.+?\]', '', messy_string)
	#remove \n
	one_line = just_lyrics.replace('\n',' ')
	one_line = one_line.translate(string.maketrans("",""), string.punctuation)
	return(one_line)


def get_lyrics(song_url):
	html = urlopen(song_url).read()
	soup = BeautifulSoup(html, "lxml")
	#Now get pyong data
	pyong = soup.find("span", "pyongs_count").contents[0].strip()
	#Now get lyrics data: 
	lyrics = parse_lyrics(soup.find('div','lyrics').text) #remove \n, [..] 
	return([lyrics,int(pyong)])
	

all_data = [get_lyrics(song_url) for song_url in all_songs]
all_lyrics = [[data[0].encode('ascii','ignore'),data[1]] for data in all_data] #This shouldn't be necessary twice, w/e. 



with open("C:/Users/nauner/SkyDrive/4year/Spring/Spanish/KANYE_LYRICS.csv", 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(all_lyrics)


