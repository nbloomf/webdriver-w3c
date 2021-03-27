{-# LANGUAGE RecordWildCards #-}
module Web.Api.WebDriver.Monad.Test.Server.Page (
    HtmlTag(..)
  , Attr(..)
  , Document(Text, tag, attrs, children, elementId)
  , Page(url, contents)
  , buildPage
  , node
  , requestPage
  , CssSelector()
  , getElementById
  , cssMatchDocument
  , parseCss
  , tagIsClearable
  , pageAboutBlank
  ) where

import Text.ParserCombinators.Parsec
import Data.Monoid
import Data.List




data HtmlTag
  = Html
  | Head
  | Title
  | Body
  | Div
  | P
  | Ol
  | Ul
  | Li
  | Form
  | Input
  | Button
  deriving Eq

instance Show HtmlTag where
  show t = case t of
    Html -> "html"; Head -> "head"; Title -> "title"; Body -> "body"
    Div -> "div"; P -> "p"; Ol -> "ol"; Ul -> "ul"; _ -> error "Show HtmlTag"

tagIsClearable :: HtmlTag -> Bool
tagIsClearable t = case t of
  Input -> True
  _ -> False

data Attr
  = Id
  | Class
  | Name
  deriving (Eq, Show)

data Document
  = Text String
  | Document
      { elementId :: String
      , tag :: HtmlTag
      , attrs :: [(Attr, Maybe String)]
      , children :: [Document]
      }
  deriving (Eq, Show)

attrHasValue :: Attr -> String -> Document -> Bool
attrHasValue _ _ (Text _) = False
attrHasValue a v Document{..} =
  case lookup a attrs of
    Just (Just val) -> v == val
    _ -> False

data Page = Page
  { contents :: Document
  , url :: String
  } deriving Show

node :: HtmlTag -> [(Attr, Maybe String)] -> [Document] -> Document
node tag attrs children =
  let elementId = "" in
  Document{..}

pageAboutBlank :: Page
pageAboutBlank = Page
  { contents = Text ""
  , url = "about:blank"
  }


assignIds :: String -> Document -> Document
assignIds _ h@(Text str) = Text str
assignIds base h@Document{..} = h
  { elementId = base
  , children = zipWith prefix [1..] children
  }
  where
    prefix i child = assignIds (base ++ "." ++ show i) child

buildPage :: String -> Document -> Page
buildPage url doc =
  let contents = assignIds "" doc
  in Page{..}

test1 :: Page
test1 = buildPage "example.com" $
  node Html []
    [
    ]
    


getElementById :: String -> Page -> Maybe Document
getElementById str Page{..} = getFirst $ get contents
  where
    get :: Document -> First Document
    get (Text _) = First Nothing
    get d@Document{..} = if elementId == str
      then First (Just d)
      else mconcat $ map get children



data CssSelector
  = CssTag HtmlTag
  | CssClass HtmlTag String
  | CssHash HtmlTag String
  | CssAttr HtmlTag Attr String
  deriving Show

pHtmlTag :: Parser HtmlTag
pHtmlTag = choice $ map try
  [ string "html" >> return Html
  , string "head" >> return Head
  , string "title" >> return Title
  , string "body" >> return Body
  , string "div" >> return Div
  , string "p" >> return P
  , string "ol" >> return Ol
  , string "ul" >> return Ul
  , string "li" >> return Li
  , string "form" >> return Form
  , string "input" >> return Input
  , string "button" >> return Button
  ]

pAttr :: Parser Attr
pAttr = choice
  [ string "class" >> return Class
  , string "id" >> return Id
  ]

tokenchar :: Parser String
tokenchar = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-','_']

pCssSelector :: Parser CssSelector
pCssSelector =
  choice
    [ try $ do
        tag <- pHtmlTag
        char '.'
        classname <- tokenchar
        return (CssClass tag classname)

    , try $ do
        tag <- pHtmlTag
        char '#'
        name <- tokenchar
        return (CssHash tag name)

    , try $ do
        tag <- pHtmlTag
        char '['
        attr <- pAttr
        char '='
        char '\''
        value <- tokenchar
        char '\''
        char ']'
        return (CssAttr tag attr value)

    ] <|> do
      tag <- pHtmlTag
      return (CssTag tag)

parseCss :: String -> Either ParseError CssSelector
parseCss str = parse pCssSelector "" str


cssMatchDocument :: CssSelector -> Document -> [Document]
cssMatchDocument _ (Text _) = []
cssMatchDocument selector d@Document{..} =
  let
    match = case selector of
      CssTag t -> t == tag
      CssClass t c -> t == tag && attrHasValue Class c d
      CssHash t h -> t == tag && attrHasValue Id h d
      CssAttr t a v -> t == tag && attrHasValue a v d
  in
    (if match then (d:) else id) $
      concatMap (cssMatchDocument selector) children

requestPage :: String -> [Page] -> Maybe Page
requestPage _ [] = Nothing
requestPage path (p@Page{..}:ps) =
  if url == path
    then Just p
    else requestPage path ps
