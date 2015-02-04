#!/usr/bin/env python

'''Classes to Parse LiveJournal XML feed'''

from lxml import etree, html

class ParsedPost:
    '''Helper class used by ParsedLJ below'''

    def __init__(self, xml_post):
        self.xml = xml_post
        # `u0,` is a trick to unpack an exactly 1-item list
        # We'll get an error if the len is != 1
        u0, = xml_post.xpath('user')
        self.user = u0.text
        t0, = xml_post.xpath('event/string')
        # lxml will decode the XML elements (like &gt;) and we end up with a
        # proper HTML string
        self.event = html.fromstring(t0.text)
        self.event_text = self.event.text_content()

class ParsedLJ:
    '''The useful (to us) information in the LiveJournal XML'''

    def __init__(self, fname):
        '''Parse the XML contained in fname'''
        self.parsed = etree.parse(fname)
        # I've made this a bit strict: it'll only find post elements directly
        # under the root node (which is `posts`)
        self.posts = [ParsedPost(el) for el in self.parsed.xpath('post')]

if __name__ == '__main__':
    from sys import argv, exit
    try:
        # Unpack exactly 1 argument
        _, fname = argv
    except ValueError:
        print 'Usage: python parsed_lj.py <lj_file.xml>'
        exit(1)

    parsed = ParsedLJ(fname)
    numposts = len(parsed.posts)
    print 'Found {} events'.format(numposts)
    if numposts > 0:
        print '(first) username: {}'.format(parsed.posts[0].user)
