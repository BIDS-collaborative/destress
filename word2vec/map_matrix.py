""" 
Maps the LiveJournal Matrix to the order of the MasterDict Matrix.
This is so we can get the correct weights multiplied at each word (or else there is a mismatch)
"""


if __name__ == '__main__':
  masterDict = open('/var/local/destress/LJ_word2vec/list_Dictionary.txt', 'r')
  liveJournal = open('/var/local/destress/LJ_word2vec/word2vecTEXT_copy.txt', 'r')
  outputMatrix = open('/var/local/destress/LJ_word2vec/matrix_ordered.txt', 'r+')

<<<<<<< HEAD
	#have to load in text files as lists
	masterList = masterDict.readlines()
	ljList = liveJournal.readlines()
	
	ljMap = {}
=======
  #have to load in text files as lists
  masterList = masterDict.readlines()
  ljList = liveJournal.readlines()
  masterDict.close()
  liveJournal.close()
  
  ljMap = {}
>>>>>>> 381c1359f74a02e64c2476da9990fff9f18e276b

  zeroes = '0.000000 ' * 300
  zeroes = zeroes.strip() 

<<<<<<< HEAD
	for line in ljList:
		word, sep, weight = line.partition(" ")
		ljMap[word] = weight
	#for k,v in ljMap.items():
	#	print k, v	
	
	count = 1
	for word in masterList:
		print count
		weight = ljMap.get(word)
		outputMatrix.write(word + ' ')
		
		if weight is not None:
			outputMatrix.write(weight + '\n')
		else:
			outputMatrix.write(zeroes + '\n')
		count += 1
	masterDict.close()
	liveJournal.close()
	outputMatrix.close()
=======
  for line in ljList:
    word, sep, weight = line.partition(" ")
    ljMap[word] = weight

  for word in masterList:
    weight = ljMap.get(word)
    outputMatrix.write(word + ' ')
    if weight is not None:
      outputMatrix.write(weight + '\n')
    else:
      outputMatrix.write(zeroes + '\n')

  outputMatrix.close()
>>>>>>> 381c1359f74a02e64c2476da9990fff9f18e276b
