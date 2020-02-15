Basado en:
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Modificado por Mauro Jaskelioff

> module Parsing where
> import Data.Char
> import Control.Monad
> import Control.Applicative hiding (many)
> import ReminderTypes

The monad of parsers
--------------------

> newtype Parser a              =  P (String -> [(a,String)])
>
> instance Functor Parser where
>    fmap = liftM
>
> instance Applicative Parser where
>    pure = return
>    (<*>) = ap
> 
> instance Monad Parser where
>    return v                   =  P (\inp -> [(v,inp)])
>    p >>= f                    =  P (\inp -> [ x | (v,out) <- parse p inp, x <- parse (f v) out])
>
> instance Alternative Parser where
>    empty = mzero
>    (<|>) = mplus
> 
> instance MonadPlus Parser where
>    mzero                      =  P (\_	 -> [])
>    p `mplus` q                =  P (\inp -> case parse p inp of
>                                                []        -> parse q inp
>                                                x         -> x)

Basic parsers
-------------

> failure                       :: Parser a
> failure                       =  mzero
>
> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])
> 
> parse                         :: Parser a -> String -> [(a,String)]
> parse (P p) inp               =  p inp

Derived primitives
------------------

> sat                           :: (Char -> Bool) -> Parser Char
> sat p                         =  do x <- item
>                                     if p x then return x else failure
> 
> digit                         :: Parser Char
> digit                         =  sat isDigit
> 
> lower                         :: Parser Char
> lower                         =  sat isLower
> 
> upper                         :: Parser Char
> upper                         =  sat isUpper
> 
> letter                        :: Parser Char
> letter                        =  sat isAlpha
> 
> alphanum                      :: Parser Char
> alphanum                      =  sat isAlphaNum
> 
> char                          :: Char -> Parser Char
> char x                        =  sat (== x)
>
> string                        :: String -> Parser String
> string []                     =  return []
> string (x:xs)                 =  do char x
>                                     string xs
>                                     return (x:xs)
> 
> many                          :: Parser a -> Parser [a]
> many p                        =  many1 p <|> return []
> 
> many1                         :: Parser a -> Parser [a]
> many1 p                       =  do v  <- p
>                                     vs <- many p
>                                     return (v:vs)
> 
> ident                         :: Parser String
> ident                         =  do x  <- lower
>                                     xs <- many alphanum
>                                     return (x:xs)
> 
> nat                           :: Parser Int
> nat                           =  do xs <- many1 digit
>                                     return (read xs)
>
> int                           :: Parser Int
> int                           =  do char '-'
>                                     n <- nat
>                                     return (-n)
>                                   <|> nat
> 
> space                         :: Parser ()
> space                         =  do many (sat isSpace)
>                                     return ()
>	
> sepBy                         :: Parser a -> Parser sep -> Parser [a]
> sepBy p sep                   =  sepBy1 p sep <|> return []
>
> sepBy1                        :: Parser a -> Parser sep -> Parser [a]
> sepBy1 p sep        		    = do{ x <- p
>                         	    ; xs <- many (sep >> p)
>                         	    ; return (x:xs) }	
>
> endBy1                        :: Parser a -> Parser sep -> Parser [a]
> endBy1 p sep                  = many1 (do { x <- p; sep; return x })
>
> endBy                         :: Parser a -> Parser sep -> Parser [a]
> endBy p sep                   = many (do{ x <- p; sep; return x })
>
>

Ignoring spacing
----------------

> token                         :: Parser a -> Parser a
> token p                       =  do space
>                                     v <- p
>                                     space
>                                     return v
> 
> identifier                    :: Parser String
> identifier                    =  token ident
> 
> natural                       :: Parser Int
> natural                       =  token nat
> 
> integer                       :: Parser Int
> integer                       =  token int
>
> symbol                        :: String -> Parser String
> symbol xs                     =  token (string xs)

Specific for reminder commands
------------------------------

> validChars                   :: Parser Char
> validChars                   = alphanum <|> char '_' <|> char '-'
>
> validStr                     :: Parser String
> validStr                     = do x  <- lower
>                                   xs <- many validChars
>                                   return (x:xs)
>
> reminderId                   :: Parser String
> reminderId                   = token validStr
>
> operation                     :: Parser ReminderOp
> operation                     = do symbol "add"
>                                    return RemAdd
>                                  <|> do symbol "del"
>                                         return RemDel
>                                       <|> do symbol "list"
>                                              return RemList
>
> reiterate                     :: Parser Int
> reiterate                     = do symbol "repeat"
>                                    n <- natural
>                                    return n
>                                  <|> do symbol ""
>                                         return 0
>
> time                          :: Parser MyTime
> time                          = do symbol "at"
>                                    hr <- natural
>                                    symbol ":"
>                                    min <- natural
>                                    return (Time hr min)
>
> date                          :: Parser MyDate
> date                          = do symbol "on"
>                                    year <- natural
>                                    symbol "-"
>                                    mon <- natural
>                                    symbol "-"
>                                    day <- natural
>                                    return (Date year mon day)
>
> datetime                      :: Parser MyDateTime
> datetime                      = do d <- date
>                                    t <- time
>                                    return (DateTime d t)
>
> eventualRem                   :: Parser Reminder
> eventualRem                   = do id <- reminderId
>                                    dt <- datetime
>                                    n <- reiterate
>                                    return (Rmd id (Just dt) (Just (Iter (Fyear 1) n)))
>
> fmin                           :: Parser Freq
> fmin                           = do n <- natural
>                                     (symbol "minutes" <|> symbol "minute")
>                                     return (Fmin n)
>
> fhour                          :: Parser Freq
> fhour                          = do n <- natural
>                                     (symbol "hours" <|> symbol "hour")
>                                     return (Fhour n)
>
> fday                           :: Parser Freq
> fday                           = do n <- natural
>                                     (symbol "days" <|> symbol "day")
>                                     return (Fday n)
>
> fmon                           :: Parser Freq
> fmon                           = do n <- natural
>                                     (symbol "months" <|> symbol "month")
>                                     return (Fmon n)
>
> fyear                          :: Parser Freq
> fyear                          = do n <- natural
>                                     (symbol "years" <|> symbol "year")
>                                     return (Fyear n)
>
> frequency                      :: Parser Freq
> frequency                      = do symbol "every"
>                                     f <- (fmin <|> fhour <|> fday <|> fmon <|> fyear)
>                                     return f
>
> dailyRem                      :: Parser Reminder
> dailyRem                      = do id <- reminderId
>                                    (do freq <- frequency
>                                        n <- reiterate
>                                        return (Rmd id Nothing (Just (Iter freq n)))
>                                      <|> do t <- time
>                                             n <- reiterate
>                                             return (Rmd id (Just (DateTime (Date 0 0 0) t)) (Just (Iter (Fday 1) n))))
>
> addOp                         :: Parser ReminderOp
> addOp                         = do symbol "add"
>                                    return RemAdd
>
> delOp                         :: Parser ReminderOp
> delOp                         = do symbol "del"
>                                    return RemDel
>
> lstOp                         :: Parser ReminderOp
> lstOp                         = do symbol "list"
>                                    return RemList
>
> dailyRemAdd                   :: Parser ReminderReq
> dailyRemAdd                   = do op <- addOp
>                                    rem <- dailyRem
>                                    return (RemReq op (Just rem))
>
> dailyRemDel                   :: Parser ReminderReq
> dailyRemDel                   = do op <- delOp
>                                    id <- reminderId
>                                    return (RemReq op (Just (Rmd id Nothing Nothing)))
>
> dailyRemLst                   :: Parser ReminderReq
> dailyRemLst                   = do op <- lstOp
>                                    return (RemReq op Nothing)
>
> daily                         :: Parser ReminderReq
> daily                         = do addRem <- dailyRemAdd
>                                    return addRem
>                                  <|> do delRem <- dailyRemDel
>                                         return delRem
>                                       <|> do lstRem <- dailyRemLst
>                                              return lstRem
>
> eventualRemAdd                :: Parser ReminderReq
> eventualRemAdd                = do op <- addOp
>                                    rem <- eventualRem
>                                    return (RemReq op (Just rem))
>
> eventualRemDel                :: Parser ReminderReq
> eventualRemDel                = do op <- delOp
>                                    id <- reminderId
>                                    return (RemReq op (Just (Rmd id Nothing Nothing)))
>
> eventualRemLst                :: Parser ReminderReq
> eventualRemLst                = do op <- lstOp
>                                    return (RemReq op Nothing)
>
> eventual                      :: Parser ReminderReq
> eventual                      = do addRem <- eventualRemAdd
>                                    return addRem
>                                  <|> do delRem <- eventualRemDel
>                                         return delRem
>                                       <|> do lstRem <- eventualRemLst
>                                              return lstRem