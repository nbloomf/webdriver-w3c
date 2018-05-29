module Web.Api.Http.Effects.Test.Server
  ( _200_Ok
  , _400_Bad_Request
  , _404_Not_Found
  , _405_Method_Not_Allowed
  , _408_Request_Timeout
  , _500_Internal_Server_Error
  ) where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (createCookieJar)
import Network.HTTP.Types

import Web.Api.Http
import Web.Api.Http.Effects.Test.Mock

_200_Ok :: ByteString -> HttpResponse
_200_Ok body = HttpResponse
  { _responseStatus = status200
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_400_Bad_Request :: ByteString -> HttpResponse
_400_Bad_Request body = HttpResponse
  { _responseStatus = status400
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_404_Not_Found :: ByteString -> HttpResponse
_404_Not_Found body = HttpResponse
  { _responseStatus = status404
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_405_Method_Not_Allowed :: ByteString -> HttpResponse
_405_Method_Not_Allowed body = HttpResponse
  { _responseStatus = status405
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_408_Request_Timeout :: ByteString -> HttpResponse
_408_Request_Timeout body = HttpResponse
  { _responseStatus = status408
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_500_Internal_Server_Error :: ByteString -> HttpResponse
_500_Internal_Server_Error body = HttpResponse
  { _responseStatus = status500
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }
