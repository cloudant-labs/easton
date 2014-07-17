
#include <spatialindex/capi/sidx_api.h>
#include <spatialindex/capi/sidx_impl.h>

// I'm hiding these functions in their own
// compilation unit so that I'm not exposing
// all of index.cc to the private implementation
// of SpatialIndex.


std::string
get_si_index_error()
{
    char* err = Error_GetLastErrorMsg();
    std::string m(err);
    free(err);
    return m;
}


IndexPropertyH
get_si_index_properties(IndexH index)
{
	Index* idx = static_cast<Index*>(index);
	Tools::PropertySet* ps = new Tools::PropertySet;

	*ps = idx->GetProperties();
	return (IndexPropertyH)ps;
}
