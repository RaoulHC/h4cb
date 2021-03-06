{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson
import Data.Thread
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import Text.RawString.QQ

thread :: Maybe Value
thread = decode [r|{
"no": 51971506,
"sticky": 1,
"closed": 1,
"now": "12/20/15(Sun)20:03:52",
"name": "Anonymous",
"com": "The /g/ Wiki:<br><a href=\"http://wiki.installgentoo.com/\">http://wiki.installgentoo.com/</a><br><br>\r\n\r\n/g/ is for the discussion of technology and related topics.<br>\r\n/g/ is <b><u>NOT</u></b> your personal tech support team or personal consumer review site.<br><br>\r\nFor tech support/issues with computers, use <a href=\"https://boards.4chan.org/wsr/\">/wsr/ - Worksafe Requests</a> or one of the following:<br>\r\n<a href=\"https://startpage.com/\">https://startpage.com/</a> or <a href=\"https://duckduckgo.com\">https://duckduckgo.com</a> (i.e., fucking google it)<br>\r\n<a href=\"https://stackexchange.com/\">https://stackexchange.com/</a><br>\r\n<a href=\"http://www.logicalincrements.com/\">http://www.logicalincrements.com/</a><br><br>\r\n\r\nYou can also search the catalog for a specific term by using:<br>\r\n<a href=\"https://boards.4chan.org/g/searchword\"><a href=\"https://boards.4chan.org/g/searchword\" target=\"_blank\">https://boards.4chan.org/g/searchwo<wbr>rd</a></a> or by clicking on [Search]<br><br> \r\n\r\nAlways check the catalog before creating a thread:<br><a href=\"https://boards.4chan.org/g/catalog\"><a href=\"/g/catalog\" class=\"quotelink\">&gt;&gt;&gt;/g/catalog</a></a><br><br>\r\n\r\nPlease check the rules before you post:<br><a href=\"https://www.4chan.org/rules\"></a><br>\r\n<i>Begging for cryptocurrency is against the rules!</i><br><br>\r\n\r\nTo use the Code tag, book-end your body of code with: [code] and [/code]",
"filename": "RMS",
"ext": ".png",
"w": 450,
"h": 399,
"tn_w": 250,
"tn_h": 221,
"tim": 1450659832892,
"time": 1450659832,
"md5": "cEeDnXfLWSsu3+A/HIZkuw==",
"fsize": 299699,
"resto": 0,
"capcode": "mod",
"semantic_url": "the-g-wiki",
"replies": 0,
"images": 0,
"last_modified": 1450659844
}|]

gwiki :: T.Text
gwiki = [r|"The /g/ Wiki:<br><a href=\"http://wiki.installgentoo.com/\">http://wiki.installgentoo.com/</a><br><br>\r\n\r\n/g/ is for the discussion of technology and related topics.<br>\r\n/g/ is <b><u>NOT</u></b> your personal tech support team or personal consumer review site.<br><br>\r\nFor tech support/issues with computers, use <a href=\"https://boards.4chan.org/wsr/\">/wsr/ - Worksafe Requests</a> or one of the following:<br>\r\n<a href=\"https://startpage.com/\">https://startpage.com/</a> or <a href=\"https://duckduckgo.com\">https://duckduckgo.com</a> (i.e., fucking google it)<br>\r\n<a href=\"https://stackexchange.com/\">https://stackexchange.com/</a><br>\r\n<a href=\"http://www.logicalincrements.com/\">http://www.logicalincrements.com/</a><br><br>\r\n\r\nYou can also search the catalog for a specific term by using:<br>\r\n<a href=\"https://boards.4chan.org/g/searchword\"><a href=\"https://boards.4chan.org/g/searchword\" target=\"_blank\">https://boards.4chan.org/g/searchwo<wbr>rd</a></a> or by clicking on [Search]<br><br> \r\n\r\nAlways check the catalog before creating a thread:<br><a href=\"https://boards.4chan.org/g/catalog\"><a href=\"/g/catalog\" class=\"quotelink\">&gt;&gt;&gt;/g/catalog</a></a><br><br>\r\n\r\nPlease check the rules before you post:<br><a href=\"https://www.4chan.org/rules\"></a><br>\r\n<i>Begging for cryptocurrency is against the rules!</i><br><br>\r\n\r\nTo use the Code tag, book-end your body of code with: [code] and [/code]"|]

main :: IO ()
main = do
  putStrLn ""
  T.putStrLn $ htmlToANSI gwiki
