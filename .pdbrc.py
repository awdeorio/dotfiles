"""
Configuration for pdb and pdbpp

"""
import pdb

class Config(pdb.DefaultConfig):
    """
    Configuration for pdbpp.

    https://pypi.org/project/pdbpp/#description
    """

    # Start sticky mode automatically when pdbpp is installed
    sticky_by_default = True

    # Colors: https://gist.github.com/chrisopedia/8754917#background
    current_line_color = 7
