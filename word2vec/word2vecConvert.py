from gensim.models import Word2Vec

if __name__ == '__main__':
	model = Word2Vec.load('/var/local/destress/word2vecLJ.model')
	model.save_word2vec_format('/var/local/destress/word2vecLJGoogle1.bin', binary=True)

