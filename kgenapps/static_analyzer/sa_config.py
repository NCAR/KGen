# cover_config.py

from collections import OrderedDict

class SAConfig(object):

    def __init__(self, homedir):
        self.home = homedir
        self.attrs = OrderedDict()
        self.options = []

