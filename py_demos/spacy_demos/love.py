import spacy as sp
import pandas as pd

nlp = sp.load('en')


def read_file(file_name):
    return open(file_name, 'r').read()

beginners0 = read_file('../../data/texts_raw/carver/beginners.txt')
beginners = nlp(beginners0)

#for word in beginners:
#    print(word.text, word.lemma, word.lemma_, word.tag, word.tag_, word.pos, word.pos_)

for word in beginners:
    print(word.text, word.pos, word.pos_)


entities = list(beginners.ents)
print("There were {} entities found".format(len(entities)))
pd.DataFrame(entities)[:10]

orgs_and_people = [entity.orth_ for entity in entities if entity.label_ in ['ORG','PERSON']]
pd.DataFrame(orgs_and_people)
