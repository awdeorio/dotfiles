# Local support for STL pretty printing
python
import os
import sys
sys.path.insert(0, os.path.expanduser('~/.gdb.d/python'))
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
end

# C++ related beautifiers (optional)
set print pretty on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3
set print sevenbit-strings off

# GDB on OSX
set startup-with-shell off
