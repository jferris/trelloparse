module TrelloParse (parseTrello) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

parseTrello :: String -> Text -> Either ParseError Text
parseTrello name input = parse trelloParser name input

trelloParser :: Parser Text
trelloParser = do
    headers <- T.concat <$> many1 header
    void newline
    void uselessGreeting
    sections <- T.intercalate rule <$> manyTill section (try uselessSignature)
    return $ headers <> "\n" <> sections

rule :: Text
rule = "\n------------------------------------------------------------\n\n"

header :: Parser Text
header = do
    name <- many1 $ noneOf ":\n"
    start <- many1 $ noneOf "\n"
    void newline
    rest <- many headerContinued
    let value = T.pack start <> "\n" <> T.unlines rest
    return $ T.pack name <> value

headerContinued :: Parser Text
headerContinued = do
    char '\t'
    text <- many1 $ noneOf "\n"
    void newline
    return $ "\t" <> T.pack text <> "\n"

section :: Parser Text
section = try commented <|> try moved <|> try changedDueDate <|> try dueSoon <|> try addedYou <|> try mentionedYou <|> try created <|> try archived

commented :: Parser Text
commented = cardAction "commented on the card"

moved :: Parser Text
moved = cardAction "moved the card"

archived :: Parser Text
archived = cardAction "archived the card"

addedYou :: Parser Text
addedYou = cardAction "added you to the card"

mentionedYou :: Parser Text
mentionedYou = cardAction "mentioned you on the card"

created :: Parser Text
created = cardAction "created"

changedDueDate :: Parser Text
changedDueDate = cardAction "changed the due date on the card"

cardAction :: String -> Parser Text
cardAction trigger = do
    action <- manyTill (alphaNum <|> char ' ') (try $ string trigger)
    void space
    card <- cardInfo
    contents <- try body <|> try (uselessReply >> return "")
    return $ T.pack action <> T.pack trigger <> ":\n" <> card <> "\n" <> contents

dueSoon :: Parser Text
dueSoon = do
    void $ string "Due Soon\n\n"
    card <- cardInfo
    return $ "The following card is due soon:\n" <> card <> "\n"

cardInfo :: Parser Text
cardInfo = do
    card <- manyTill anyChar (lookAhead $ try $ parentheticalUrl)
    cardUrl <- parentheticalUrl
    void $ many1 $ noneOf "\n"
    void $ string "\n\n"
    return $ T.pack card <> "\n" <> cardUrl

parentheticalUrl :: Parser Text
parentheticalUrl = do
    string " ("
    cardUrl <- url
    void $ char ')'
    return cardUrl

body :: Parser Text
body = do
    void $ string " \""
    result <- manyTill anyChar (try (string "\"\n\n" >> uselessReply))
    return $ "\n" <> T.pack result <> "\n"

url :: Parser Text
url = try trelloUrl <|> try otherUrl

trelloUrl :: Parser Text
trelloUrl = do
    root <- T.pack <$> string "https://trello.com/c/"
    path <- T.pack <$> many1 (noneOf "-")
    void $ many1 $ noneOf ")"
    return $ root <> path

otherUrl :: Parser Text
otherUrl = do
    protocol <- try (string "http://") <|> string "https://"
    contents <- many1 $ noneOf ")"
    return $ T.pack protocol <> T.pack contents

uselessGreeting :: Parser ()
uselessGreeting = do
    void $ string "Here's what you missed on Trello."
    void newline
    void newline
    return ()

uselessReply :: Parser ()
uselessReply = do
    void $ string " Reply via email: "
    void $ many1 $ noneOf "\n"
    void newline
    void newline
    return ()

uselessSignature :: Parser ()
uselessSignature = do
    void $ string "--"
    return ()
