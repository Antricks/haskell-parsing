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
import Parsers.BasicInternet

data Scheme = Http | Https | Ftp | Sftp | Mailto | File | Data | Telnet deriving (Eq, Show)

data Url = Url {getScheme :: Scheme, getInfo :: [UrlInfo]} deriving (Eq, Show)

data UrlInfo = Host [ConnInfo] | Path String | Query (String, String) | Fragment String | Email String deriving (Eq, Show)

data ConnInfo = HostLocation HostAddr | Port Int | User String | Password String deriving (Eq, Show) -- maybe I'll merge this into UrlInfo later...

data HostAddr = DomainName String | Ipv4 String | Ipv6 String deriving (Eq, Show)

getHost :: [UrlInfo] -> Maybe UrlInfo
getHost [] = Nothing
getHost (r@(Host _):urlInfo) = Just r
getHost (_:urlInfo) = getHost urlInfo

getPath :: [UrlInfo] -> Maybe UrlInfo
getPath [] = Nothing
getPath (r@(Path _):urlInfo) = Just r
getPath (_:urlInfo) = getPath urlInfo

getQuery :: [UrlInfo] -> Maybe UrlInfo
getQuery [] = Nothing
getQuery (r@(Query _):urlInfo) = Just r 
getQuery (_:urlInfo) = getQuery urlInfo

getFragment :: [UrlInfo] -> Maybe UrlInfo
getFragment [] = Nothing
getFragment (r@(Fragment _):urlInfo) = Just r
getFragment (_:urlInfo) = getFragment urlInfo

getEmail :: [UrlInfo] -> Maybe UrlInfo
getEmail [] = Nothing
getEmail (r@(Email _):urlInfo) = Just r
getEmail (_:urlInfo) = getEmail urlInfo

-------------------------------
-- SCHEME IDENTIFIER PARSERS --
-------------------------------

httpSchemeP :: Parser Scheme
httpSchemeP = caseInsStringP "http" ?~ Http

httpsSchemeP :: Parser Scheme
httpsSchemeP = caseInsStringP "https" ?~ Https

ftpSchemeP :: Parser Scheme
ftpSchemeP = caseInsStringP "ftp" ?~ Ftp

sftpSchemeP :: Parser Scheme
sftpSchemeP = caseInsStringP "sftp" ?~ Sftp

mailtoSchemeP :: Parser Scheme
mailtoSchemeP = caseInsStringP "mailto" ?~ Mailto

fileSchemeP :: Parser Scheme
fileSchemeP = caseInsStringP "file" ?~ File

dataSchemeP :: Parser Scheme
dataSchemeP = caseInsStringP "data" ?~ Data

telnetSchemeP :: Parser Scheme
telnetSchemeP = caseInsStringP "telnet" ?~ Telnet

-- NOTICE: I have to watch out for overlapping identifiers like "http" and "https" here. Maybe I'll find a more elegant version later. 
schemeP :: Parser Scheme
schemeP = httpsSchemeP ||| httpSchemeP ||| sftpSchemeP ||| ftpSchemeP ||| mailtoSchemeP ||| fileSchemeP ||| dataSchemeP ||| telnetSchemeP

----------------------------------
-- SCHEME SPECIFIC PART PARSERS --
----------------------------------

commInetSchemeSpecP :: Parser [UrlInfo]
commInetSchemeSpecP = matchStringP "//" |> commInetHostInfoP ?**? commInetPathInfoP

commInetHostInfoP :: Parser UrlInfo
commInetHostInfoP = Parser f
  where
    userP :: Parser ConnInfo
    userP = wrap User (oblGreedify (notMultiCharP ['@', ':', '\n', '/'])) -- TODO: Maybe this isn't what I actually want.

    passP :: Parser ConnInfo
    passP = wrap Password (oblGreedify (notMultiCharP ['@', ':', '\n', '/'])) -- TODO: This is very likely not what I want.

    hostP :: Parser ConnInfo
    hostP = wrap HostLocation (wrap DomainName domainNameP ||| wrap Ipv4 ipv4AddrP) -- TODO: support IPv6

    portP :: Parser ConnInfo
    portP = wrap Port (between 0 (2^16-1) unsignedIntP)
    
    f i = do
      (rem, connInfo) <- runParser (obligatoryListContent (((userP ?**? (charP ':' |> passP)) <| charP '@') ?++? (hostP ?**? (charP ':' |> portP)))) i
      Just (rem, Host connInfo)

commInetPathInfoP :: Parser UrlInfo
commInetPathInfoP = wrap Path (greedifyStr (charP '/' ?*+? alphaNumStringP ?+*? charP '/')) -- TODO: Maybe this isn't what I actually want.

ftpSchemeSpecP :: Parser [UrlInfo]
ftpSchemeSpecP = commInetSchemeSpecP

httpSchemeSpecP :: Parser [UrlInfo]
httpSchemeSpecP = commInetSchemeSpecP ++? queriesP ++*? fragmentP

queriesP :: Parser [UrlInfo]
queriesP = (charP '?' |> queryP) ?*+? greedify (charP '&' |> queryP)

queryP :: Parser UrlInfo
queryP = Parser f
  where
    f i = do
      (lastRem, key) <- runParser (alphaNumStringP <| charP '=') i
      (lastRem, val) <- runParser alphaNumStringP lastRem
      Just (lastRem, Query (key, val))

fragmentP :: Parser UrlInfo
fragmentP = wrap Fragment (charP '#' |> alphaNumStringP)

mailtoSchemeSpecP :: Parser [UrlInfo]
mailtoSchemeSpecP = listify (wrap Email emailAddrP)

------------------------
-- GENERAL URL PARSER --
------------------------

urlP :: Parser Url -- TODO support URL encoding 
urlP = Parser f
  where
    f i = do
      (lastRem, scheme) <- runParser schemeP i
      (lastRem, _) <- runParser (charP ':') lastRem

      let schemeSpecificP = case scheme of
            Http -> httpSchemeSpecP
            Https -> httpSchemeSpecP
            Ftp -> ftpSchemeSpecP
            Sftp -> ftpSchemeSpecP
            Mailto -> mailtoSchemeSpecP
            File -> commInetSchemeSpecP
            Data -> undefined
            Telnet -> matchStringP "//" |> listify commInetHostInfoP <|? charP '/'
            :: Parser [UrlInfo]

      (lastRem, resultUrlObj) <- runParser (wrap (Url scheme) schemeSpecificP) lastRem

      Just (lastRem, resultUrlObj)