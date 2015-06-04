""" 
    word2vec implementation via Gensim, training over our LiveJournal Corpus
    Following guideline of code from the forum at: https://groups.google.com/forum/#!topic/gensim/MJWrDw_IvXw 

    Currently training over 1130s] separate text files.
"""

import logging
import os.path
import sys
import multiprocessing
from gensim.models import Word2Vec
from gensim.models.word2vec import LineSentence

if __name__ == '__main__':
    program = os.path.basename(sys.argv[0])
    logger = logging.getLogger(program)
    logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s')
    logging.root.setLevel(level=logging.INFO)
    logger.info("running %s" % ' '.join(sys.argv))

    sentDirectory = '/var/local/destress/text_sent_ids/'
    outputModel = '/var/local/destress/LJ_word2vec/word2vecLJ.txt'
    outputModelOG = '/var/local/destress/LJ_word2vec/word2vecLJGoogle.bin'

    fileName = 'sents_1.txt'
    sentences = LineSentence(sentDirectory+fileName)

    model = Word2Vec(sentences, size=300, window=10, min_count=5, workers=multiprocessing.cpu_count())

    for i in range(2, 1131):
        print('TEXT FILE NUMBER : ', str(i))
    fileName = 'sents_' + str(i)+'.txt'
        sentences = LineSentence(sentDirectory+fileName)
        model.train(sentences)
        #model = Word2Vec(sentences, size=400, window=5, min_count=5, workers=multiprocessing.cpu_count())

    model.save(outputModel)      # save in gensim format
    model.save_word2vec_format(outputModelOG, binary=True)     #save in original google's C binary format

""" 
    When we finish training w/ negative sampling (after we pick out the "bad queries") 
    we can save a lot of Ram with the following:
    model.init_sims(replace=True) 
"""
