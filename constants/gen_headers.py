#!/usr/bin/env python

import os
import re
import sys


DEFINE_RE = re.compile("(\w+)\s+(\S+)$")


def get_defines():
    dname = os.path.dirname(__file__)
    fname = os.path.join(dname, "defines.txt")
    ret = []
    with open(fname, "r") as handle:
        for line in handle:
            if line.lstrip().startswith("#"):
                continue
            if not line.strip():
                ret.append(None)
                continue
            m = DEFINE_RE.match(line)
            ret.append(("EASTON_" + m.group(1), m.group(2)))
    return ret


def write_c_header(defines):
    dname = os.path.dirname(__file__)
    fname = os.path.join(dname, "..", "c_src", "easton_index", "config.hh")
    with open(fname, "w") as handle:
        handle.write("#ifndef EASTON_CONSTANTS_H\n")
        handle.write("#define EASTON_CONSTANTS_H\n\n")
        for entry in defines:
            if entry is None:
                handle.write("\n")
            else:
                handle.write("#define {} {}\n".format(*entry))
        handle.write("\n\n#endif\n")


def write_erl_header(defines):
    dname = os.path.dirname(__file__)
    fname = os.path.join(dname, "..", "src", "easton_constants.hrl")
    with open(fname, "w") as handle:
        for entry in defines:
            if entry is None:
                handle.write("\n")
            else:
                handle.write("-define({}, {}).\n".format(*entry))


def main():
    defines = get_defines()
    write_c_header(defines)
    write_erl_header(defines)


if __name__ == '__main__':
    main()
