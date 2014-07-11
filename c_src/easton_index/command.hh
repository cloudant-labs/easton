
#ifndef EASTON_COMMAND_HH
#define EASTON_COMMAND_HH

#include "easton.hh"
#include "index.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_CMD_BEGIN


io::Writer::Ptr handle(easton::Index::Ptr idx, io::Reader::Ptr req);


NS_EASTON_CMD_END


#endif
