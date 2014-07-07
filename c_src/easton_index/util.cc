

#include <tcutil.h>

#include "util.h"


bool
easton_is_dir(const char* path)
{
    bool is_dir;
    
    if(!tcstatfile(path, &is_dir, NULL, NULL)) {
        return false;
    }
    
    return is_dir;
}