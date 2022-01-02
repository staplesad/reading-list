import csv
import json
from glob import glob
from pathlib import Path

import requests

def load_csv(csv_filename):
    book_titles = []
    with open(csv_filename) as fp:
        rdr = csv.reader(fp, delimiter=',')
        for i, row in enumerate(rdr):
            if i == 0:
                continue
            book_titles.append(row[0].lower())
    return book_titles

def search_book_title(title, limit=10):
    outfile = f'{title}_search_response.json'
    if Path(outfile).is_file():
        print('Already searched')
        return
    request_stem = 'http://openlibrary.org/search.json?q'
    words = title.strip().split(' ')
    query_string = '+'.join(words)
    req_str = f'{request_stem}={query_string}&limit={limit}'
    print(req_str)
    r = requests.get(req_str)
    try:
        json_res = r.json()
    except requests.exceptions.JSONDecodeError as e:
        print(e)
        print(title)
        return

    with open(outfile, 'w') as fp:
        json.dump(json_res, fp)

if __name__ == '__main__':
    book_titles = []
    for file in glob('../data/*.csv'):
        book_titles.extend(load_csv(file))
    book_titles = list(set(book_titles))
    print(len(book_titles))
    for title in book_titles:
        search_book_title(title)


