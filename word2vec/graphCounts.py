"""  
graphCounts plots the histogram
of the counts (by reading in the outputed text file from countWords.py)
"""
from matplotlib import pyplot as plt
import numpy as np

if __name__ == '__main__':

    outputCount = open('/Users/gyoo/BIDS/destress/word2vec/word_counts.txt', 'r')
    # outputCount = open('/var/local/destress/text_sent_ids/word_counts.txt', 'r')
    counts = outputCount.readlines()
    outputCount.close()
    wordCount = {}

    for count in counts:
        c = count.split()
        cnt = int(c[0])
        numSents = int(c[2])
        wordCount[cnt] = numSents

    points = []
    for k, v in wordCount.iteritems():
        print(k)
        for i in range(v):
            points.append(k)
    plt.title("Sentence Length of Live Journal Corpus")
    plt.xlabel("Length of sentence (in words)")
    plt.ylabel("Frequency")
    plt.hist(points, bins=50)
    plt.show()