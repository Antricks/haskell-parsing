-- URL Wikipedia: https://en.wikipedia.org/wiki/URL
-- 1994 URL RFC: https://datatracker.ietf.org/doc/html/rfc1738
-- 1998 URL RFC: https://datatracker.ietf.org/doc/html/rfc2396
-- 2005 URL RFC: https://datatracker.ietf.org/doc/html/rfc3986
-- 2020 URL RFC: https://datatracker.ietf.org/doc/html/rfc8820
-- ... This is a huge rabbit hole for sure ...

-- IPv6 in address literals & uris: https://datatracker.ietf.org/doc/html/rfc6874/
-- telnet: https://datatracker.ietf.org/doc/html/rfc4248/
-- file: https://datatracker.ietf.org/doc/html/rfc8089

-- URLs are extreeemely versatile. Which makes them quite complicated if you want to support a lot of different schemes.
-- I guess I'm going to start implementing a lot here but no one knows how much I'll finish...
-- Also a lot of what I'm currently doing here will probably be fundamentally changed over time as I'm learning more about URLs

module Parsers.Url (module Parsers.Url, module Base.ParsingBase) where

import Base.ParsingBase
import Parsers.Misc

data Scheme = Http | Https | Ftp | Sftp | Mailto | File | Data | Telnet

data Url = Url {getScheme :: Scheme, getInfo :: [UrlInfo]}

data UrlInfo = UrlInfoHost Host | Path String | Query (String, String) | Fragment String

data Host = Host String (Maybe ConnInfo)

data ConnInfo = Port (Maybe ConnInfo) | User (Maybe ConnInfo) | Password (Maybe ConnInfo)

-------------------------------
-- SCHEME IDENTIFIER PARSERS --
-------------------------------

httpSchemeP = caseInsStringP "http" ?+ Http

httpsSchemeP = caseInsStringP "https" ?+ Https

ftpSchemeP = caseInsStringP "ftp" ?+ Ftp

sftpSchemeP = caseInsStringP "sftp" ?+ Sftp

mailtoSchemeP = caseInsStringP "mailto" ?+ Mailto

fileSchemeP = caseInsStringP "file" ?+ File

dataSchemeP = caseInsStringP "data" ?+ Data

telnetSchemeP = caseInsStringP "telnet" ?+ Telnet

schemeParser = httpsSchemeP ||| httpSchemeP ||| sftpSchemeP ||| ftpSchemeP ||| mailtoSchemeP ||| fileSchemeP ||| dataSchemeP ||| telnetSchemeP

----------------------------------
-- SCHEME SPECIFIC PART PARSERS --
----------------------------------

commonInternetSchemeSpecP :: Parser [UrlInfo]
commonInternetSchemeSpecP = stringP "//" |> undefined

ftpSchemeSpecP :: Parser [UrlInfo]
ftpSchemeSpecP = commonInternetSchemeSpecP

httpSchemeSpecP :: Parser [UrlInfo]
httpSchemeSpecP = commonInternetSchemeSpecP ++? queryP ++? fragmentP

queryP :: Parser [UrlInfo]
queryP = undefined

fragmentP :: Parser [UrlInfo]
fragmentP = undefined

mailtoSchemeSpecP :: Parser [UrlInfo]
mailtoSchemeSpecP = undefined

------------------------
-- GENERAL URL PARSER --
------------------------

urlParser :: Parser Url
urlParser = Parser f
  where
    f i = do
      (lastRem, scheme) <- runParser schemeParser i
      (lastRem, _) <- runParser (charP ':') lastRem

      let schemeSpecificParser = case scheme of
            Http -> httpSchemeSpecP
            Https -> httpSchemeSpecP
            Ftp -> ftpSchemeSpecP
            Sftp -> ftpSchemeSpecP
            Mailto -> mailtoSchemeSpecP
            File -> commonInternetSchemeSpecP
            Data -> undefined
            Telnet -> undefined
            :: Parser [UrlInfo]

      (lastRem, urlInfo) <- runParser schemeSpecificParser lastRem
      let resultUrlObj = Url scheme urlInfo

      Just (lastRem, resultUrlObj)