from gensim.models import Word2Vec

if __name__ == '__main__':
	model = Word2Vec.load('/var/local/destress/LJ_word2vec/word2vecLJ_2.txt')
	model.save_word2vec_format('/var/local/destress/LJ_word2vec/word2vecTEXT.txt')
