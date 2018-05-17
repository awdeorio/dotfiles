import pdb
import rlcompleter

# Tab completion
pdb.Pdb.complete = rlcompleter.Completer(locals()).complete

# Start sticky mode automatically when pdbpp is installed
class Config(pdb.DefaultConfig):
    sticky_by_default = True
