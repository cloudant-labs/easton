
#ifndef EASTON_IO_HH
#define EASTON_IO_HH


#include <unistd.h>


bool easton_read_data(unsigned char** data, size_t* len);
void easton_send_data(unsigned char* data, size_t len);
void easton_send_ok(unsigned char* data, size_t len);
void easton_send_error(unsigned char* data, size_t len);


#endif