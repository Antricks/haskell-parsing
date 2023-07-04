module Parsers.BasicInternet where

import Base.CharSets
import Base.ParsingBase
import Parsers.Basic (alphaNumStringP, digitCharP)

newtype EmailAddr = EmailAddr String

newtype Ipv4Addr = Ipv4Addr String

newtype Ipv6Addr = Ipv6Addr String

ipv4AddrP :: Parser String
ipv4AddrP = octet ++* charP '.' +++ octet ++* charP '.' +++ octet ++* charP '.' +++ octet
  where
    octet :: Parser String
    octet = (multiCharP "012" ?**? digitCharP) ?++* digitCharP

ipv6AddrP :: Parser String
ipv6AddrP = undefined -- TODO implement -> https://en.wikipedia.org/wiki/IPv6_address

domainNameP :: Parser String
domainNameP = multiCharStringP ('-' : alphaNumCS) ++? greedifyStr (charP '.' *++ multiCharStringP ('-' : alphaNumCS))

emailAddrP :: Parser String
emailAddrP = localPartP ++* charP '@' +++ (domainNameP ||| (charP '[' *++ ipv4AddrP ++* charP ']')) -- TODO support IPv6
  where
    localPartP = notCharP '.' *++? multiCharStringP ("!#$%&'*+-/=?^_`{|}~." ++ alphaCS) -- NOTICE: this is still not completetly correct -> https://en.wikipedia.org/wiki/Email_address#Local-part
