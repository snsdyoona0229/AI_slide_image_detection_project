#define _CRT_SECURE_NO_WARNINGS

#ifdef _WIN32
    #include <SDKDDKVer.h>
    #include <tchar.h>
#endif

#include <stdio.h>

///////////////////////////////////////////////////

#ifdef _WIN32
	#define NOMINMAX
	#include "windows.h"
#endif

#include <sstream>
#ifdef _WIN32
	#define _NDPR2_STDCALL_ __stdcall
#else
	#define _NDPR2_STDCALL_
#endif

#if defined(_NDPR2_NARROWSTRINGS_) && defined(_NDPR2_WIDESTRINGS_)

	#error _NDPR2_NARROWSTRINGS_ and _NDPR2_WIDESTRINGS_ are mutually exclusive.

#elif !defined(_NDPR2_NARROWSTRINGS_) &&  !defined(_NDPR2_WIDESTRINGS_)

	#if defined (__APPLE__) || defined (__linux__)
		
		#define _NDPR2_NARROWSTRINGS_
		
	#else
		
		#define _NDPR2_WIDESTRINGS_
		
	#endif

#endif

enum ndprRequestType
{
	ndprRequestType_InitFileImage		= 0,
	ndprRequestType_InitTestImage		= 1,
	ndprRequestType_CloseImage			= 2,
	ndprRequestType_GetImageInfo		= 3,
	ndprRequestType_GetMetadata			= 4,
	ndprRequestType_GetRegion			= 5,
	ndprRequestType_ConvertUnits		= 6,
	ndprRequestType_GetMetaImage		= 7,
	ndprRequestType_InitCallbackImage	= 8
};

enum ndprError
{
	ndprError_Success				=   0,
	ndprError_ExceptionDetected		=  -1,
	ndprError_InvalidRequestType	=  -2,
	ndprError_InvalidImageID		=  -3,
	ndprError_InvalidRequestID		=  -4,
	ndprError_MissingParameter		=  -5,
	ndprError_InvalidParameter		=  -6,
	ndprError_ParameterNotFound		=  -7,
	ndprError_RequestNotExecuted	=  -8,
	ndprError_ImageInUse			=  -9,
	ndprError_ImageNotInUse			= -10,
	ndprError_MetaImageNotPresent	= -11,
	ndprError_UnsupportedFiletype	= -12,
	ndprError_BufferNotFilled		= -13
};

enum ndprOption
{
	ndprOption_TextEncoding			= 0,
	ndprOption_CpuType				= 1
};

enum ndprEncoding
{
	ndprEncoding_UCS2				= 0,
	ndprEncoding_UTF8				= 1,
	ndprEncoding_ANSI				= 2
};

enum ndprCpuType
{
	ndprCpuType_Unknown				= 0x00,
	ndprCpuType_SSE					= 0x40, /* Processor supports Intel(R) Streaming SIMD Extensions (Intel(R) SSE) instruction set					*/
	ndprCpuType_SSE2				= 0x41, /* Processor supports Intel(R) Streaming SIMD Extensions 2 (Intel(R) SSE2) instruction set				*/
	ndprCpuType_SSE3				= 0x42, /* Processor supports Intel(R) Streaming SIMD Extensions 3 (Intel(R) SSE3) instruction set				*/
	ndprCpuType_SSSE3				= 0x43, /* Processor supports Intel(R) Supplemental Streaming SIMD Extension 3 (Intel(R) SSSE3) instruction set	*/
	ndprCpuType_SSE41				= 0x44, /* Processor supports Intel(R) Streaming SIMD Extensions 4.1 (Intel(R) SSE4.1) instruction set			*/
	ndprCpuType_SSE42				= 0x45, /* Processor supports Intel(R) Streaming SIMD Extensions 4.2 (Intel(R) SSE4.2) instruction set			*/
	ndprCpuType_AVX					= 0x46, /* Processor supports Intel(R) Advanced Vector Extensions (Intel(R) AVX) instruction set				*/
	ndprCpuType_AES					= 0x47, /* Processor supports Intel(R) AES New Instructions														*/
	ndprCpuType_SHA					= 0x48, /* Processor supports Intel(R) SHA New Instructions														*/
	ndprCpuType_F16RND				= 0x49, /* Processor supports RDRRAND & Float16 instructions													*/
	ndprCpuType_AVX2				= 0x4a, /* Processor supports Intel(R) Advanced Vector Extensions 2 (Intel(R) AVX2) instruction set				*/
	ndprCpuType_ADCOX				= 0x4b, /* Processor supports ADCX and ADOX instructions														*/
};

struct ndprDataRequest
{
	void	   *pBuffer;
	long long	nFilePos;
	int			nLength;
	bool		bFilled;
};

#if defined(_NDPR2_NARROWSTRINGS_)

	typedef bool(_NDPR2_STDCALL_ *ndprRequestData)	(void *i_pID, const char* i_strSubFilename, int i_nNoRequests, ndprDataRequest *i_pRequests);

#elif defined(_NDPR2_WIDESTRINGS_)

	typedef bool(_NDPR2_STDCALL_ *ndprRequestData)	(void *i_pID, const wchar_t* i_strSubFilename, int i_nNoRequests, ndprDataRequest *i_pRequests);

#endif

#ifdef _WIN32

	typedef int					(__stdcall *ndprSetOptionProc)			(int nOptionID, int nValue);
	typedef int					(__stdcall *ndprCreateRequestProc)		(int nRequestType);
	typedef int					(__stdcall *ndprExecuteRequestProc)		(int nRequestID);
	typedef int					(__stdcall *ndprFreeRequestProc)		(int nRequestID);
	typedef int					(__stdcall *ndprGetLastErrorCodeProc)	(int nRequestID);

	#ifdef _UNICODE

		typedef const wchar_t *	(__stdcall *ndprGetLastErrorStringProc)	(int nRequestID);
		typedef int				(__stdcall *ndprSetRequestIntProc)		(int nRequestID, const wchar_t *strParamID, int nValue);
		typedef int				(__stdcall *ndprSetRequestInt64Proc)	(int nRequestID, const wchar_t *strParamID, long long nValue);
		typedef int				(__stdcall *ndprSetRequestFloatProc)	(int nRequestID, const wchar_t *strParamID, float fValue);
		typedef int				(__stdcall *ndprSetRequestStringProc)	(int nRequestID, const wchar_t *strParamID, const wchar_t *strValue);
		typedef int				(__stdcall *ndprSetRequestPointerProc)	(int nRequestID, const wchar_t *strParamID, void *pValue);
		typedef int				(__stdcall *ndprGetRequestIntProc)		(int nRequestID, const wchar_t *strParamID);
		typedef long long		(__stdcall *ndprGetRequestInt64Proc)	(int nRequestID, const wchar_t *strParamID);
		typedef float			(__stdcall *ndprGetRequestFloatProc)	(int nRequestID, const wchar_t *strParamID);
		typedef const wchar_t *	(__stdcall *ndprGetRequestStringProc)	(int nRequestID, const wchar_t *strParamID);
		typedef const void *	(__stdcall *ndprGetRequestPointerProc)	(int nRequestID, const wchar_t *strParamID);

	#else

		typedef const char *	(__stdcall *ndprGetLastErrorStringProc)	(int nRequestID);
		typedef int				(__stdcall *ndprSetRequestIntProc)		(int nRequestID, const char *strParamID, int nValue);
		typedef int				(__stdcall *ndprSetRequestInt64Proc)	(int nRequestID, const char *strParamID, long long nValue);
		typedef int				(__stdcall *ndprSetRequestFloatProc)	(int nRequestID, const char *strParamID, float fValue);
		typedef int				(__stdcall *ndprSetRequestStringProc)	(int nRequestID, const char *strParamID, const char *strValue);
		typedef int				(__stdcall *ndprSetRequestPointerProc)	(int nRequestID, const char *strParamID, void *pValue);
		typedef int				(__stdcall *ndprGetRequestIntProc)		(int nRequestID, const char *strParamID);
		typedef long long		(__stdcall *ndprGetRequestInt64Proc)	(int nRequestID, const char *strParamID);
		typedef float			(__stdcall *ndprGetRequestFloatProc)	(int nRequestID, const char *strParamID);
		typedef const char *	(__stdcall *ndprGetRequestStringProc)	(int nRequestID, const char *strParamID);
		typedef const void *	(__stdcall *ndprGetRequestPointerProc)	(int nRequestID, const char *strParamID);

	#endif
	
	ndprSetOptionProc			ndprSetOption;
	ndprCreateRequestProc		ndprCreateRequest;
	ndprExecuteRequestProc		ndprExecuteRequest;
	ndprFreeRequestProc			ndprFreeRequest;
	ndprGetLastErrorCodeProc	ndprGetLastErrorCode;
	ndprGetLastErrorStringProc	ndprGetLastErrorString;
	ndprSetRequestIntProc		ndprSetRequestInt;
	ndprSetRequestInt64Proc		ndprSetRequestInt64;
	ndprSetRequestFloatProc		ndprSetRequestFloat;
	ndprSetRequestStringProc	ndprSetRequestString;
	ndprSetRequestPointerProc	ndprSetRequestPointer;
	ndprGetRequestIntProc		ndprGetRequestInt;
	ndprGetRequestInt64Proc		ndprGetRequestInt64;
	ndprGetRequestFloatProc		ndprGetRequestFloat;
	ndprGetRequestStringProc	ndprGetRequestString;
	ndprGetRequestPointerProc	ndprGetRequestPointer;

	HMODULE						hNDPRead;
	
#endif // #ifdef _WIN32





#ifdef _WIN32

	void *LoadProc(const char *strName)
	{
		if (void *pAddress = (void*)GetProcAddress(hNDPRead, strName))
			return pAddress;

		//std::cout << "Failed to load procedure " << strName << std::endl;
		exit(1);
	}

	void LoadNDPReadDLL()
	{
		if (!(hNDPRead = LoadLibraryA("compile_program_callback/NDPread2.dll")))
		{
			//std::cout << "Failed to load NDPread2.dll" << std::endl;
			exit(1);
		}

		ndprSetOption				= (ndprSetOptionProc			)LoadProc("ndprSetOption");
		ndprCreateRequest			= (ndprCreateRequestProc		)LoadProc("ndprCreateRequest");
		ndprExecuteRequest			= (ndprExecuteRequestProc		)LoadProc("ndprExecuteRequest");
		ndprFreeRequest				= (ndprFreeRequestProc			)LoadProc("ndprFreeRequest");
		ndprGetLastErrorCode		= (ndprGetLastErrorCodeProc		)LoadProc("ndprGetLastErrorCode");
		ndprGetLastErrorString		= (ndprGetLastErrorStringProc	)LoadProc("ndprGetLastErrorString");
		ndprSetRequestInt			= (ndprSetRequestIntProc		)LoadProc("ndprSetRequestInt");
		ndprSetRequestInt64			= (ndprSetRequestInt64Proc		)LoadProc("ndprSetRequestInt64");
		ndprSetRequestFloat			= (ndprSetRequestFloatProc		)LoadProc("ndprSetRequestFloat");
		ndprSetRequestString		= (ndprSetRequestStringProc		)LoadProc("ndprSetRequestString");
		ndprSetRequestPointer		= (ndprSetRequestPointerProc	)LoadProc("ndprSetRequestPointer");
		ndprGetRequestInt			= (ndprGetRequestIntProc		)LoadProc("ndprGetRequestInt");
		ndprGetRequestInt64			= (ndprGetRequestInt64Proc		)LoadProc("ndprGetRequestInt64");
		ndprGetRequestFloat			= (ndprGetRequestFloatProc		)LoadProc("ndprGetRequestFloat");
		ndprGetRequestString		= (ndprGetRequestStringProc		)LoadProc("ndprGetRequestString");
		ndprGetRequestPointer		= (ndprGetRequestPointerProc	)LoadProc("ndprGetRequestPointer");
	}



#endif






























#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
RawVector NDPI_jpg_tiling(std::string file_name, std::string location_name, int x, int y, int z) {
  static bool initialization = FALSE;
  static std::string access_file = "start";
  static int	  nImageID;
  int             pyramid_layer;
  int	          x_location;
  int	          y_location;
  std::string 	  save_location = location_name;
  static int      nRequestID;
  
  x_location = x;
  y_location = y;
  pyramid_layer = z;
  
  if (initialization == FALSE) {
    LoadNDPReadDLL();
    initialization = TRUE;
  }
  
  if (access_file != file_name & access_file != "start") {
  	access_file = file_name;
	
	nRequestID = ndprCreateRequest(ndprRequestType_CloseImage);	
	
	ndprSetRequestInt(nRequestID, "imageid", nImageID);	
	ndprExecuteRequest(nRequestID);
	ndprFreeRequest(nRequestID);
	
	ndprSetOption(ndprOption_TextEncoding, ndprEncoding_ANSI);
	nRequestID = ndprCreateRequest(ndprRequestType_InitFileImage);
	ndprSetRequestString(nRequestID, "filename", access_file.c_str());
	nImageID = ndprExecuteRequest(nRequestID);
	ndprFreeRequest(nRequestID);
		 
	const int	nSourceTileSize = (1 << (12 - pyramid_layer)) * 1024;
	const int	nDestTileSize = 1024;
	 
	nRequestID = ndprCreateRequest(ndprRequestType_GetRegion);
	ndprSetRequestString(nRequestID, "format", "JPEG");
	ndprSetRequestInt(nRequestID, "imageid", nImageID);
	ndprSetRequestInt(nRequestID, "sourcewidth", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "sourceheight", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "width", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "height", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "top", y_location * nSourceTileSize);
	ndprSetRequestInt(nRequestID, "left", x_location * nSourceTileSize);
		  
	ndprExecuteRequest(nRequestID);
	const void *pBuffer;
	int			nSize;
	pBuffer = ndprGetRequestPointer(nRequestID, "imagebuffer");
	nSize = ndprGetRequestInt(nRequestID, "buffersize");
		  
	FILE *fp = fopen(save_location.c_str(), "wb");
	fwrite(pBuffer, nSize, 1, fp);
	fclose(fp);
		  
	return 0;
  } else if (access_file == "start") {
  	access_file = file_name;
  	ndprSetOption(ndprOption_TextEncoding, ndprEncoding_ANSI);
	nRequestID = ndprCreateRequest(ndprRequestType_InitFileImage);
	ndprSetRequestString(nRequestID, "filename", access_file.c_str());
	nImageID = ndprExecuteRequest(nRequestID);
	ndprFreeRequest(nRequestID);
		 
	const int	nSourceTileSize = (1 << (12 - pyramid_layer)) * 1024;
	const int	nDestTileSize = 1024;
	 
	nRequestID = ndprCreateRequest(ndprRequestType_GetRegion);
	ndprSetRequestString(nRequestID, "format", "JPEG");
	ndprSetRequestInt(nRequestID, "imageid", nImageID);
	ndprSetRequestInt(nRequestID, "sourcewidth", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "sourceheight", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "width", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "height", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "top", y_location * nSourceTileSize);
	ndprSetRequestInt(nRequestID, "left", x_location * nSourceTileSize);
		  
	ndprExecuteRequest(nRequestID);
	const void *pBuffer;
	int			nSize;
	pBuffer = ndprGetRequestPointer(nRequestID, "imagebuffer");
	nSize = ndprGetRequestInt(nRequestID, "buffersize");
		  
	FILE *fp = fopen(save_location.c_str(), "wb");
	fwrite(pBuffer, nSize, 1, fp);
	fclose(fp);
		  
	return 0;
  } else if	(access_file == file_name) {
  	const int	nSourceTileSize = (1 << (12 - pyramid_layer)) * 1024;
	const int	nDestTileSize = 1024;
	 
	nRequestID = ndprCreateRequest(ndprRequestType_GetRegion);
	ndprSetRequestString(nRequestID, "format", "JPEG");
	ndprSetRequestInt(nRequestID, "imageid", nImageID);
	ndprSetRequestInt(nRequestID, "sourcewidth", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "sourceheight", nSourceTileSize);
	ndprSetRequestInt(nRequestID, "width", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "height", nDestTileSize);	
	ndprSetRequestInt(nRequestID, "top", y_location * nSourceTileSize);
	ndprSetRequestInt(nRequestID, "left", x_location * nSourceTileSize);
	
	ndprExecuteRequest(nRequestID);
	const void *pBuffer;
	int			nSize;
	pBuffer = ndprGetRequestPointer(nRequestID, "imagebuffer");
	nSize = ndprGetRequestInt(nRequestID, "buffersize");
		  
	FILE *fp = fopen(save_location.c_str(), "wb");
	fwrite(pBuffer, nSize, 1, fp);
	fclose(fp);
		  
	return 0;
  }


}
