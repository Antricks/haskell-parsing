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
import Parsers.Basic

data Scheme = Http | Https | Ftp | Sftp | Mailto | File | Data | Telnet deriving (Eq, Show)

data Url = Url {getScheme :: Scheme, getInfo :: [UrlInfo]} deriving (Eq, Show)

data UrlInfo = Host [ConnInfo] | Path String | Query (String, String) | Fragment String deriving (Eq, Show)

data ConnInfo = HostAddr String | Port Int | User String | Password String deriving (Eq, Show)

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

commInetSchemeSpecP :: Parser [UrlInfo]
commInetSchemeSpecP = stringP "//" |> commInetHostInfoP ?**? commInetPathInfoP

commInetHostInfoP :: Parser UrlInfo
commInetHostInfoP = Parser f
  where
    userP :: Parser ConnInfo
    userP = Parser f
      where
        f i = do
          (rem, user) <- runParser alphaNumStringP i -- TODO: Maybe this isn't what I actually want.
          Just (rem, User user)

    passP :: Parser ConnInfo
    passP = Parser f
      where
        f i = do
          (rem, pass) <- runParser (oblGreedify (notMultiCharP ['@', ':', '\n', '/'])) i -- TODO: This is very likely not what I want.
          Just (rem, Password pass)

    hostP :: Parser ConnInfo
    hostP = Parser f
      where
        f i = do
          (rem, hostAddr) <- runParser (oblGreedify (alphaNumCharP ||| multiCharP ['.', '-'])) i -- TODO: Maybe this isn't what I actually want.
          Just (rem, HostAddr hostAddr)

    portP :: Parser ConnInfo
    portP = Parser f
      where
        f i = do
          (rem, port) <- runParser posIntP i -- NOTICE: this allows for a '+' before the port number. Theoretically this could be unwanted.
          Just (rem, Port port)

    f i = do
      (rem, connInfo) <- runParser (((userP ?**? (charP ':' |> passP)) <| charP '@') ?++? (hostP ?**? (charP ':' |> portP))) i
      Just (rem, Host connInfo)

commInetPathInfoP :: Parser UrlInfo
commInetPathInfoP = Parser f
  where
    f i = do
      (rem, path) <- runParser (greedifyStr (charP '/' ?*+? alphaNumStringP ?+*? charP '/')) i -- TODO: Maybe this isn't what I actually want.
      Just (rem, Path path)

ftpSchemeSpecP :: Parser [UrlInfo]
ftpSchemeSpecP = commInetSchemeSpecP

httpSchemeSpecP :: Parser [UrlInfo]
httpSchemeSpecP = commInetSchemeSpecP ++? queryP ++? fragmentP

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
            File -> commInetSchemeSpecP
            Data -> undefined
            Telnet -> undefined
            :: Parser [UrlInfo]

      (lastRem, urlInfo) <- runParser schemeSpecificParser lastRem
      let resultUrlObj = Url scheme urlInfo

      Just (lastRem, resultUrlObj)