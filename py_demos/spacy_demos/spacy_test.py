import spacy as sp

nlp = sp.load('en')

doc = nlp(u'They told us to duck.')
for word in doc:
    print(word.text, word.lemma, word.lemma_, word.tag, word.tag_, word.pos, word.pos_)
    
    
def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read()


baby0 = read_file('../../data/texts_raw/barthelme/baby.txt')
baby = nlp(baby0)

#for word in baby:
#    print(word.text, word.lemma, word.lemma_, word.tag, word.tag_, word.pos, word.pos_)

baby_sentences = [s for s in baby.sents ]
len(baby_sentences)


baby_sentiment = [s.sentiment for s in baby.sents ]


from collections import Counter, defaultdict

keywords = Counter()
for chunk in baby.noun_chunks:
    if nlp.vocab[chunk.lemma_].prob < - 8: # probablity value -8 is arbitrarily selected threshold
        keywords[chunk.lemma_] += 1

keywords.most_common
nlp.vocab[baby.noun_chunks]


def count_entity_sentiment(nlp, texts):
    '''Compute the net document sentiment for each entity in the text.'''
    entity_sentiments = Counter()
    for doc in nlp.pipe(texts, batch_size=1000, n_threads=4):
        for ent in doc.ents:
            entity_sentiments[ent.text] += doc.sentiment
    return entity_sentiments

count_entity_sentiment(nlp, baby)