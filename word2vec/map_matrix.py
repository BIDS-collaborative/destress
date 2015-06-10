""" 
Maps the LiveJournal Matrix to the order of the MasterDict Matrix.
This is so we can get the correct weights multiplied at each word (or else there is a mismatch)
"""


if __name__ == '__main__':
  masterDict = open('/var/local/destress/LJ_word2vec/list_Dictionary.txt', 'r')
  liveJournal = open('/var/local/destress/LJ_word2vec/word2vecTEXT_copy.txt', 'r')
  outputMatrix = open('/var/local/destress/LJ_word2vec/matrix_ordered1.txt', 'r+')

  
  #have to load in text files as lists
  masterList = masterDict.readlines()
  ljList = liveJournal.readlines()
  masterDict.close()
  liveJournal.close()
  
  ljMap = {}

  zeroes = '0.000000 ' * 300
  zeroes = zeroes.strip() 

  
  for line in ljList:
    word, sep, weight = line.partition(" ")
    ljMap[word] = weight

  #for k,v in ljMap.items():
  #  outputMatrix.write(k + v + '\n')
   
  for word in masterList:
    weight = ljMap.get(word.rstrip())
    outputMatrix.write(word + ' ')
    if weight is not None:
      print word, 'NOT NONE'
      outputMatrix.write(weight + '\n')
    else: 
      print word, 'IS NOT IN WORD2VECLIST'
      outputMatrix.write(zeroes + '\n')
  
  masterDict.close()
  liveJournal.close()
  outputMatrix.close()

