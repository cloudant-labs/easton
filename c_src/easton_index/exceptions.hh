
#ifndef EASTON_EXCEPTION_HH
#define EASTON_EXCEPTION_HH


#include <string>
#include <sstream>

#include "easton.hh"


NS_EASTON_BEGIN


class EastonException: public std::exception
{
    public:
        explicit EastonException(const char* msg) {
            this->reason = msg;
        }

        explicit EastonException(std::string msg) {
            this->reason = msg;
        }

        virtual ~EastonException() throw() {}

        virtual const char* what() const throw() {
            return this->reason.c_str();
        };

    protected:
        std::string reason;
};


class EastonExit: public std::exception
{
    public:
        EastonExit(int code) {
            std::stringstream ss;
            ss << "EastonExit: " << this->code;
            this->reason = ss.str();
            this->code = code;
        }

        virtual ~EastonExit() throw() {}

        virtual const char* what() const throw() {
            return this->reason.c_str();
        }

        std::string reason;
        int code;
};


class IndexException: public std::exception
{
    public:
        IndexException(std::string msg) {
            this->msg = msg;
        }

        virtual ~IndexException() throw() {}

        virtual const char*  what() const throw() {
            return this->msg.c_str();
        }

        std::string msg;
};


class GeoException: public std::exception
{
    public:
        GeoException(const char* msg) {
            this->msg = msg;
        }

        virtual ~GeoException() throw() {}

        virtual const char* what() const throw() {
            return this->msg.c_str();
        }

        std::string msg;
};


NS_EASTON_END


#endif
