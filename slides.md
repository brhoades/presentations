---
author: Billy Rhoades
title: Parser Combinators
date: April 13, 2019
---

# Objectives
 * Discuss paser combinator modules available
 * Cover parser combinator usage in [brhoades/marko-hs](https://github.com/brhoades/marko-hs)

   * Basic sentence parsing ([Markov Chains blog](http://blog.brod.es/markov/chain/marko/ruby/bot/irc/2018/07/06/my-first-markov-chain-bot.html))
   * IRC Parsing (subset of [RFC 2812](https://tools.ietf.org/html/rfc2812))


# Parser combinator modules
 * [base's `Text.ParserCombinators.ReadP`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html) (marko-hs IRC parser)
   * Built-in with widely-available examples but slow
 * [parser-combinators](https://github.com/mrkkrp/parser-combinators)
   * Lightweight parser combinators, only depends on base
 * [parsec](https://github.com/haskell/parsec) / [Megaparsec](https://github.com/mrkkrp/megaparsec)
   * Industrial-strength, monad-fed, feature-rich library
 * [attoparsec](https://github.com/bos/attoparsec) (marko-hs sentence parser)
   * Parsec(ish) but with `ByteStrings`
   * Fast, memory-efficient, but no unicode
   * Good for parsing vast quantities of data (GBs of IRC text!)

# Attoparsec
Core types
```haskell
type Parser = Parser ByteString
type Result = IResult ByteString
data IResult i r =
    Fail i [String] String     -- parse failed
  | Partial (i -> IResult i r) -- continue feeding input
  | Done i r                   -- complete
```

Starting functions

```haskell
-- If you want to feed more data in chunks
parse :: Parser a -> ByteString -> Result a

-- Take one string, return result or error
parseOnly :: Parser a -> ByteString -> Either String a
```

# Dealing with ByteStrings
* `Word8`s are unsigned 8-bit integers
* `ByteStrings` are `Word8` arrays in a whole grain compiler wrap

```haskell
data {-# CTYPE "HsWord8" #-} Word8 = W8# Word# -- :s
data ByteString = \
  PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
     {-# UNPACK #-} !Int                -- offset
     {-# UNPACK #-} !Int                -- length
  deriving (Typeable)

pack :: [Word8] -> ByteString
unpack :: ByteString -> [Word8]
```

# [Char] to ByteString

```haskell
-- Getting to/from Word8 is most easily done
-- by massaging a Char into something with Num

toW :: (Num c, Enum a) => a -> c
toW = fromIntegral . fromEnum
```

```
> :t pack [toW 'w']
pack [toW 'w'] :: ByteString

toBS str = pack $ map toW str

> :t toBS "Hello!"
toBS "Hello!" :: ByteString

```

# Let's parse a sentence
 * Words are separated by spaces
 * Punctuation terminates sentences / clauses
 * End of input to terminate sentences abruptly

```haskell
-- Source: A dog jumps over the log! The log was very long.
-- We want:
["A", "dog", "jumps", "over", "the", "log", "!",
 "The", "log", "was", "very", "long", "."]

-- So:
parseWords :: ByteString -> [ByteString]

-- Which'll be easier if we find out how to parse a single word first
parseWord :: ByteString -> [ByteString]
```

# Attoparsec predicates
Attoparsec takes `Word8 -> Bool` functions to do matching work. We'll need those to determine when we hit a "separator" or "punctuation".

```haskell
splitter       = (== toW ' ')
endline        = \x -> x == (toW '\n') || x == (toW '\r')
punct          = \x -> x == (toW '.')
                      || x == (toW '!') 
                      || x == (toW '?')
wordTerminator = \x -> (splitter x || endline x || punct x)
```

Attoparsec provides utility functions to do this, but they're slow. Documentation explicitly advises against their use.

```haskell
inClass    :: String -> Word8 -> Bool
notInClass :: String -> Word8 -> Bool 
vowel             = inClass "aeiou"
probablyConsonant = notInClass "aeiou"
```

# `Parser` Monad
`Parser` wraps your parsing functions. It carries parsed input and unused input forward as you consume it.

```haskell
parseWords :: Parser [ByteString]
parseWords = undefined  -- soon!

parseOnly :: Parser a -> ByteString -> Either String a

-- We will implement parseWord
parseWords :: ByteString -> [ByteString]
parseWords str = case parseOnly parseWords str of
  Left _  -> error "Parser error"
  Right c -> c
```

# `parseWord`
```haskell
takeWhile :: (Word8 -> Bool) -> Parser ByteString 

-- Input: toBS "abc 123 monad"
-- Output: ["abc", "123", "monad"]
-- Start with parsing individual words termed by spaces/punc
parseWord :: Parser ByteString
parseWord = do
  res <- takeWhile1 (not . wordTerminator)
  return res
```

 * wordTerminator is our `Word8` matcher (predicate) for punctuation, spaces, or newlines
 * `takeWhile1` requires a match and consumes input while the predicate is satisfied
   * `takeWhile` allows no match

# Using `parseWord`
```haskell
parseWord :: Parser ByteString
parseWord = do
  res <- takeWhile1 (not . wordTerminator)
  return res

> parseOnly parseWord (toBS "Hello there!")
Right "Hello"
```

Wow, *way cool*

# Edge Cases
There's some nasty edge cases if you begin your words with a splitter.
```haskell
> parseOnly parseWord (toBS "   Hello there!")
Left "Failed reading: takeWhile1"
```

We don't want these separators in our words anyway, so let's skip them.

```haskell
skipWhile :: (Word8 -> Bool) -> Parser () 
parseWord = do
  skipWhile splitter -- (== toW ' ')
  res <- takeWhile1 (not . wordTerminator)
  skipWhile splitter -- another edge case, dw about it
  return res

> parseOnly parseWord (toBS "   Hello there!")
Right "Hello" -- 😂
```

`skipWhile` is a `takeWhile` that discards. Not `skipWhile1` because if there are no spaces, that's fine.

# Now for Sentences
We need to parse words over and over. We have the inner bit done but how do we chain them?

```haskell
manyTill :: Alternative f => f a -> f b -> f [a] 
satisfy :: (Word8 -> Bool) -> Parser Word8 

parseWords :: ByteString -> [ByteString]
parseWords = do
  skipWhile (\x -> endline x || splitter x)
  manyTill parseWord (satisfy endline)
```

* We continue to skip any endlines or splitters that may come in between words.
* `manyTill` calls one parser until the other consumes input

# Does it work?
Yes!

```haskell
> parseOnly parseWords (toBS "Hello there\n")
Right ["Hello","there"]
```

But not for punctuation

```haskell
> parseOnly parseWords (toBS "Hello there!\n")
Left "Failed reading: takeWhile1"
```

# Why does it fail?

```haskell
-- inside parseWord:
takeWhile1 (not . wordTerminator)

-- Input remaining: "Hello there!"
-- First call: "Hello"

-- Input remaining: "there!"
-- Second call: "There"

-- Input remaining: "!"
-- Third call: ""
-- Error: takeWhile1 requires at least one Word8 match
```

# Beef it up 
Punctuation is an important part of a sentence. We should consume it and return it if it's there.

```haskell
-- We need to call this if there's punctuation
-- but takeWhile1 requires 1 punctuation
takeWhile1 punct
  -- \x -> x == (toW '.') || x == (toW '!') || x == (toW '?')
```

# Enter: choice
```haskell
choice :: Alternative f => [f a] -> f a 

parseWord :: Parser (ByteString)
parseWord = do
  skipWhile splitter
  res <- choice [takeWhile1 (not . wordTerminator),
                 takeWhile1 punct]
  skipWhile splitter
  return res
```

Now we can parse a word that's only punctuation.

# With choice
```haskell
> parseOnly parseWords (toBS "Hello there!\n")
Right ["Hello","there","!"]

> parseOnly parseWords (
    toBS "Hello there! How are you doing?\n"
  )
Right ["Hello","there","!","How","are","you","doing","?"]
```

Perfect, it's all separated now. This is (mostly) how marko-hs uses attoparsec to parse sentences into words.

# Summary of attoparsec and marko-hs
 * Marko used attoparsec over other libraries because:
   * `ByteString`s are very memory efficient
   * Parsing into `ByteString`s is faster than parsing into `Data.Text`
   * Parsing 500k IRC lines into chains takes seconds
   * Storing 500k chains is done <512 MiB memory
     * 6 million 2-pairs of chains, stored uncompressed, takes mere GBs
 * I hated attoparsec because:
   * Less feature rich / widely used than megaparsec or `ReadP`
   * Using `ByteString`s activated my C PTSD

# How about that IRC?
Here's the bare essentials of IRC that our bot needs:

 * PING: allows us to stay connected. When you get a PING you PONG.
 * MODE: this is an easy way to tell when we can join our channels.
 * PRIVMSG: all messages from users come over PRIVMSG.

Examples of each:

```
PING irc.freenode.net
:irc.freenode.net MODE <umask> +b
:irc.freenode.net PRIVMSG <umask> #monads :guys...
```

# Events
Marko uses an event-based approach for parsing IRC

```haskell
data Event = MessageEvent Source Message
               | ModeEvent User String
               | Ping Server

-- Parser gets events
ircParser :: ReadP Event
```

Main bot loops does stuff with Events:

```haskell
-- Ignore the man behind the curtain. See that Event?!
handleEvent :: StdGen -> ChainData -> ChainData
                 -> Event -> Maybe (String, String)
```

# `ReadP`: Same great taste, ByteString-free
IRC parsing was done with base `Text.ParserCombinators.ReadP`

 * `Network.Simple.TCP.TLS` likes `Data.Text`
 * IRC parsing doesn't need to perform like sentence parsing
 * I did this first; lots of guides out there for how to use ReadP

Major differences

 * `ReadP` is like the `Parser` Monad
 * No stupid `toW`/`toBS`
 * `takeWhile` becomes `munch`
 * `readP_to_S` is like `parseOnly`, sans `Result`
 

# Distinctions
```
PING irc.freenode.net
:irc.freenode.net MODE <umask> +b
:irc.freenode.net PRIVMSG <umask> #monads :guys...
```

There's clearly two formats here: PING and not PING. This looks like `choice`.

```haskell
ircParser :: ReadP Event -- ReadP = Parser
ircParser = choice [otherMessages, ping]
```

Bam, that was easy.

# PING
Ping is easy too.
```
PING irc.freenode.net
```

Let's start by defining some predicates:
```haskell
eol   :: Char -> Bool
eol   =  \x -> (x == '\n' || x == '\r')
eolws :: Char -> Bool
eolws =  \x -> (x == '\n' || x == '\r' || x == ' ')
```

Some utility functions too. These just gobble arbitrary input.
```haskell
_char :: Char -> ReadP ()
_char x = char x >> return ()
_string :: String -> ReadP ()
_string x = string x >> return ()
```

# Parsing PING
Ping has two parts:

 * PING
 * Everything after PING (which the server wants back)

So we have two things to parse:
```haskell
ping :: ReadP (Maybe Event)
ping = do
  -- get rid of leftover whitespace
  skipMany $ satisfy eolws 
  -- 1) Discard PING.
  _string "PING "
  -- 2) Grab everything else (server name)
  -- munch (not . eolws)
  server <- consumeEOLWS 
  -- A Ping Event for our event-based bot
  Just $ Ping server 
```

# Parsing *everything else*
```haskell
ircParser :: ReadP (Maybe Event)
ircParser = choice [otherMessages, ping]
```

otherMessages starts off like PING. Get the event type and parse each event differently.
```haskell
otherMessages :: ReadP (Maybe Event)
otherMessages = do
  skipMany $ satisfy eolws
  -- ':' is on these; if this fails, choice runs "ping"
  _char ':'
  server <- munch1 (not . eolws) -- munch1 is takeWhile1
  _char ' '
  eventType <- munch1 (/= ' ')
  _char ' '
  getEvent server eventType
```


# getEvent
```
:irc.freenode.net MODE <umask> +b
:irc.freenode.net PRIVMSG <umask> #monads :guys...
```

```haskell
getEvent :: String -> eventType -> ReadP (Maybe Event)
getEvent server x
    | x == "PRIVMSG" = handleMSG server
    | x == "MODE"    = do
        -- Very easy. Get a umask, get the mode.
        umask <- munch1 (not . eolws)
        let user = parseUser umask
        _char ' '
        mode <- munch1 (not . eolws)
        return $ Just $ ModeEvent user mode
    | otherwise       = do
        -- Unsupported IRC event
        _ <- consumeEOL
        return Nothing
```

# Finally: `handleMSG`
```
:irc.freenode.net PRIVMSG <umask> #monads :guys...
```

```haskell
handleMSG :: String -> ReadP (Maybe Event)
handleMSG user = do
  -- handles parsing umasks; boring
  let nick = parseUser user
  channel <- munch1 (/= ' ')
  _string " :"
  msg <- consumeEOL
  return $ Just $ MessageEvent ("", channel, Just $ nick) msg
```

# That's it
That's all the IRC parsing that you need to get into a server. It's incredibly simple. Just hook it up to your HTTP socket:

```haskell
parseIncoming :: String -> Maybe Event
parseIncoming str = do
  -- readP_to_S is parseOnly that includes all the intermediate steps.
  -- unreducedEvents :: [(Event, String)]
  let unreducedEvents = readP_to_S ircParser str 

  if length unreducedEvents > 0 then
    -- We just want the last result's event
    Just $ fst . last $ unreducedEvents
  else
    Nothing
```

# Awesome Resouces
Want more parsing? 

 * Examples
   * METAR weather data with `ReadP` ([blog](https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html))
   * Simple imperative language parsing ([wiki](https://wiki.haskell.org/Parsing_a_simple_imperative_language))
 * Other resources
   * Markov chains for nuts like me ([blog.brod.es](http://blog.brod.es/markov/chain/marko/ruby/bot/irc/2018/07/06/my-first-markov-chain-bot.html))
   * Deep comparison of parser combinator packages ([megaparsec GitHub](https://github.com/mrkkrp/megaparsec#comparison-with-other-solutions))

