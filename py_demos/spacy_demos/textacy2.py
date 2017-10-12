import spacy as sp
import pandas as pd

nlp = sp.load('en')


def read_file(file_name):
    return open(file_name, 'r').read()

beginners0 = read_file('../../data/texts_raw/carver/beginners.txt')
beginners = nlp(beginners0)

#for word in beginners:
#    print(word.text, word.lemma, word.lemma_, word.tag, word.tag_, word.pos, word.pos_)

#for word in beginners:
#    print(word.text, word.pos, word.pos_)


entities = list(beginners.ents)
print("There were {} entities found".format(len(entities)))
pd.DataFrame(entities)[:10]

org_people = [[entity.orth_,  entity.label_] for entity in entities if entity.label_ in ['ORG','PERSON']]
pd.DataFrame(org_people).drop_duplicates()


from gensim import models
lda = models.ldamodel.LdaModel

import textacy
import textacy.datasets  # note that this is now textacy.datasets if conda ever updates

cw = textacy.datasets.CapitolWords()
cw.download()
records = cw.records(speaker_name={'Hillary Clinton', 'Barack Obama'})
text_stream, metadata_stream = textacy.fileio.split_record_fields(records, 'text')
corpus = textacy.Corpus('en', texts=text_stream, metadatas=metadata_stream)
corpus

corpus_processed =  (textacy.preprocess_text(doc.text, lowercase=True, no_punct=True) for doc in corpus)
vectorizer = textacy.Vectorizer(weighting='tf', normalize=True, smooth_idf=True, min_df=2, max_df=0.95)
doc_term_matrix = vectorizer.fit_transform((doc.to_terms_list(ngrams=1, named_entities=True, as_strings=True)
  for doc in corpus))
print(repr(doc_term_matrix))

model = textacy.TopicModel('lda', n_topics=10)
model.fit(doc_term_matrix)
doc_topic_matrix = model.transform(doc_term_matrix)
doc_topic_matrix.shape
for topic_idx, top_terms in model.top_topic_terms(vectorizer.id_to_term, top_n=10):
    print('topic', topic_idx, ':', '   '.join(top_terms))


model.termite_plot(doc_term_matrix, vectorizer.id_to_term, topics=-1,  n_terms=25, sort_terms_by='seriation')
