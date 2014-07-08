
#ifndef EASTON_IO_HH
#define EASTON_IO_HH


#include <unistd.h>


bool easton_read_data(uint8_t** data, uint32_t* len);
void easton_send_data(uint8_t* data, uint32_t len);
void easton_send_ok(uint8_t* data, uint32_t len);
void easton_send_ok_uint64_t(uint64_t value);
void easton_send_error(uint8_t* data, uint32_t len);


#endif