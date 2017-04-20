#!/usr/bin/env python
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

import os
import sys


LOOKUP_FUNCTION = """\

const char*
easton_epsg_lookup(uint64_t code)
{
    if(code < EASTON_EPSG_MIN || code > EASTON_EPSG_MAX) {
        return NULL;
    }

    return EASTON_EPSG_TABLE[code - EASTON_EPSG_MIN];
}

"""



def get_codes():
    dname = os.path.dirname(__file__)
    fname = os.path.join(dname, "epsg-codes.txt")
    codes = [];
    with open(fname) as handle:
        for line in handle:
            line = line.strip()
            if not line or line[0] == "#":
                continue
            bits = line.split(None, 1)
            if len(bits) != 2:
                print "Ignoring: %r" % line
                continue
            try:
                code = int(bits[0])
            except:
                print "Ignoring: %r" % line
                continue
            codes.append((code, bits[1]))
    codes.sort()
    return codes


def write_table(codes):
    min_epsg = codes[0][0]
    max_epsg = codes[-1][0]
    size = codes[-1][0] - codes[0][0] + 1
    table = dict(codes)

    dname = os.path.dirname(__file__)
    fname = os.path.join(dname, "..", "c_src", "easton_index", "epsg.hh")
    with open(fname, "w") as handle:
        handle.write("#ifndef EASTON_EPSG_HH\n")
        handle.write("#define EASTON_EPSG_HH\n")
        handle.write("\n\n");
        handle.write("#define EASTON_EPSG_MIN %d\n" % min_epsg)
        handle.write("#define EASTON_EPSG_MAX %d\n" % max_epsg)
        handle.write("#define EASTON_EPSG_COUNT %d\n" % size)
        handle.write("\n\n");
        handle.write("const char* easton_epsg_lookup(uint64_t code);\n")
        handle.write("\n\n")
        handle.write("#endif\n")
    fname = os.path.join(dname, "..", "c_src", "easton_index", "epsg.cc")
    with open(fname, "w") as handle:
        handle.write("#include \"easton.hh\"\n")
        handle.write("#include \"epsg.hh\"\n")
        handle.write("\n\n")
        handle.write("const char* EASTON_EPSG_TABLE[EASTON_EPSG_COUNT] = {\n")
        for i in range(min_epsg, max_epsg+1):
            if i in table:
                handle.write("    \"%s\",\n" % table[i])
            else:
                handle.write("    NULL,\n")
        handle.write("};\n\n")
        handle.write(LOOKUP_FUNCTION)


def main():
    write_table(get_codes())


if __name__ == '__main__':
    main()
