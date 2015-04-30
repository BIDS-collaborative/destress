""" 
    word2vec implementation via Gensim, training over our LiveJournal Corpus
    Following guideline of code from the forum at: https://groups.google.com/forum/#!topic/gensim/MJWrDw_IvXw 

    Currently trying to train over 1131 separate text files.
    If this is too slow and/or memory inefficient, we will probably just concatentate all text files into one
    and rerun.
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
    outputModel = '/var/local/destress/word2vecLJ.model'


    fileName = 'sents_1'
    sentences = LineSentence(sentDirectory+fileName)
    model = Word2Vec(LineSentence(inp), size=400, window=5, min_count=5, workers=multiprocessing.cpu_count())
    model.save(outputModel)


    #This feels very inefficient... but AFAIK, gensim requires you to load/save each time for multiple files

    for i in range(2, 1132):
        #load the previous model
        model = Word2Vec.load(outputModel)
        fileName = 'sents_' + str(i)
        sentences = LineSentence(sentDirectory+fileName)
        model = Word2Vec(LineSentence(inp), size=400, window=5, min_count=5, workers=multiprocessing.cpu_count())
        model.save(outputModel)

""" 
    When we finish training w/ negative sampling (after we pick out the "bad queries") 
    we can save a LOT of Ram with the following:
    model.init_sims(replace=True) 

"""
