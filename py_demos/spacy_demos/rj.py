import spacy as sp

nlp = sp.load('en')


def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read()


rj0 = read_file('../../data/texts_raw/shakes/moby/Romeo_and_Juliet.txt')

rj = nlp(rj0)

next(rj.sents)