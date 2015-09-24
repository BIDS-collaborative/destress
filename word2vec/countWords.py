"""  
Simply counts the words in each sentence (newline) 
and outputs the counts to a text file

"""
import logging
import os.path
import sys
import multiprocessing

if __name__ == '__main__':
    program = os.path.basename(sys.argv[0])
    logger = logging.getLogger(program)
    logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s')
    logging.root.setLevel(level=logging.INFO)
    logger.info("running %s" % ' '.join(sys.argv))

    outputCount = open('/var/local/destress/text_sent/counts.txt', 'r+')
    wordCount = {}
    sentDirectory = '/var/local/destress/text_sent/'

    # fileName = 'sents_1.txt'

    for i in range(1, 1131):
        print('TEXT FILE NUMBER : ', str(i))
        fileName = 'sents_' + str(i) +'.txt'
        textFile = open('/var/local/destress/text_sent/'+fileName)
        sentences = textFile.readlines() # list of list of words
        textFile.close()

        for sent in sentences:
            count = len(sent.split())
            if count in wordCount:
                wordCount[count] += 1
            else:
                wordCount[count] = 1

    for k, v in wordCount.iteritems():
        statStr = str(k) + ' : ' + str(v)   
        outputCount.write(statStr + '\n')
    # write the count of the output to the dictionary
    outputCount.close()

