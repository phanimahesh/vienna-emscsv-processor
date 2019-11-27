#!/usr/bin/env python3
import sys
from bs4 import BeautifulSoup

index_file = sys.argv[1]
urls_file = sys.argv[2]

with open(index_file) as f:
    data = f.read()

soup=BeautifulSoup(data, "lxml")

all_links = soup.find_all("a")
file_links=all_links[1:]

urls = map(lambda t: t['href'], file_links)

with open(urls_file, 'w') as f:
    f.writelines("%s\n" % u for u in urls)
