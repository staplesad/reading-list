import json
from glob import glob


def extract_work_id(search_res, title):
    found = search_res['docs']
    for item in found:
        if item['type'] == 'work':
            return item['key']

    print(f'No works found for {title}')
    return None

def format_json(titles, keys):
    json_obj = {}
    for t, k in zip(titles, keys):
        json_obj.update({t:k})
    with open("../title_to_olid.json", 'w') as fp:
        json.dump(json_obj, fp)

if __name__ == '__main__':
    titles = []
    keys = []
    for filename in glob('*.json'):
        t = filename.split('_')[0]
        titles.append(t)
        with open(filename) as fp:
            search_res = json.load(fp)
        keys.append(extract_work_id(search_res, t))

    format_json(titles, keys)
