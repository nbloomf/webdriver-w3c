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
  { __response_status = status200
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }

_400_Bad_Request :: ByteString -> HttpResponse
_400_Bad_Request body = HttpResponse
  { __response_status = status400
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }

_404_Not_Found :: ByteString -> HttpResponse
_404_Not_Found body = HttpResponse
  { __response_status = status404
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }

_405_Method_Not_Allowed :: ByteString -> HttpResponse
_405_Method_Not_Allowed body = HttpResponse
  { __response_status = status405
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }

_408_Request_Timeout :: ByteString -> HttpResponse
_408_Request_Timeout body = HttpResponse
  { __response_status = status408
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }

_500_Internal_Server_Error :: ByteString -> HttpResponse
_500_Internal_Server_Error body = HttpResponse
  { __response_status = status500
  , __response_version = http11
  , __response_headers = []
  , __response_body = body
  , __response_cookie_jar = createCookieJar []
  }
