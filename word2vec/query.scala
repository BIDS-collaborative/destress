import utils._

var query = "cancer"; 

var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
var data = loadSMat("/var/local/destress/featurized_sent/data1.smat.lz4");
var googleVecs = loadFMat("/var/local/destress/wordvec_google.fmat")

var query_vec = googleVecs(dict(query), ?);
query_vec = query_vec / sqrt(sum(query_vec^2));

var magic = data.t * googleVecs;
var n = sum(magic^2, 2);
var nmagic = magic / sqrt(n);

res = nmagit * query_vec.t;

res(n == 0) = -1;
