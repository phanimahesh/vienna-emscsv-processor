#!/bin/bash

CERT=certs/cert.crt
KEY=certs/cert.key
CA=certs/stp.wien.gv.at.crt

DATE=$(date +%Y-%m-%d)

CURL="curl --cert ${CERT} --key ${KEY} --cacert ${CA}"

mkdir -p file-lists/$DATE
HTML_FILE="file-lists/$DATE/files.html"
URLS_FILE="file-lists/$DATE/urls.txt"
BASE_URL="https://stp.wien.gv.at:4543"

echo "Initializing. Reading ${BASE_URL}/emscsv/export"
$CURL ${BASE_URL}/emscsv/EXPORT/ -o ${HTML_FILE}

echo "Extracting links"
python3 extract-links.py ${HTML_FILE} ${URLS_FILE}
TOTAL=$(wc -l ${URLS_FILE} | cut -d' ' -f1)
echo "Found ${TOTAL} files, entire list available in ${URLS_FILE}"

COUNT=0
for link in $(cat ${URLS_FILE}); do
  echo "[${COUNT}/${TOTAL}] Downloading ${link}"
  $CURL --silent --show-error --create-dirs --fail ${BASE_URL}/${link} -o csvs/${DATE}/${link};
  COUNT=$(expr ${COUNT} + 1)
done
