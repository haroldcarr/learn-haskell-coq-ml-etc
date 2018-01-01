> {-# LANGUAGE GADTs #-}
> module PT where

https://blog.jakuba.net/2014/07/08/using-phantom-types-for-extra-safety.html

Jakub Arnold

Using Phantom Types for Extra Safety

Jul 8, 2014

date Message = Encrypted Text | Plaintext Text

send :: Message -> Recipient -> IO ()
send (Encrypted m) recipient = some magic with m
send (PlainText _) _ = undefined

better if call send with PlainText message rejected at compiletime.

Express relationship between Encrypted message and send function at
type level using Phantom Type.

Type called "Phantom Type"  if it has type parameter that only appears on LHS
(i.e,. not used by value constructors)

> data Message a = Message String

enables : Message Int, Message String, Message (Maybe Char), ...

> data Encrypted
> data PlainText

enables Message Encrypted, Message PlainText

Even if type parameter not used in constructors,
it is still verified by the type system:

> type Recipient = String
> send :: Message Encrypted -> Recipient -> IO ()
> send = undefined
> encrypt :: Message PlainText -> Message Encrypted
> encrypt = undefined
> decrypt :: Message Encrypted -> Message PlainText
> decrypt = undefined

For safety, make Message constructor private and export a smart constructor.
Makes it impossible to change state of Message type other than using
encrypt/decrypt.

> newMessage :: String -> Message PlainText
> newMessage s = Message s

Will be rejected:

send (newMessage "hello!") "john@example.com"

error:
    - Couldn't match type ‘PlainText’ with ‘Encrypted’
      Expected type: Message Encrypted
        Actual type: Message PlainText

> x = send (encrypt (newMessage "hello!")) "john@example.com"

------------------------------------------------------------------------------
https://blog.jakuba.net/2014/07/10/using-phantom-types-in-haskell-for-extra-safety-part-2.html

Using Phantom Types in Haskell for Extra Safety - Part 2

Jul 10, 2014

via newtype

> data MessageNT = MessageNT String
> newtype PlainTextMessageNT = PlainTextMessage MessageNT
> newtype EncryptedMessageNT = EncryptedMessage MessageNT

> sendNT :: EncryptedMessageNT -> IO ()
> sendNT = undefined
> encryptNT :: PlainTextMessageNT -> EncryptedMessageNT
> encryptNT = undefined
> decryptNT :: EncryptedMessageNT -> PlainTextMessageNT
> decryptNT = undefined

OK solution for statically typed language with no option for
representing Phantom Types.

Downside : PlainTextMessage and EncryptedMessage are not related, so can't
have function that operates on both, e.g.,
might need 'messageLength' that would operation on both.
Simple with Phantom Types:

> -- | ignore type parameter 'a', then do length of the inner String.
> messageLength :: Message a -> Int
> messageLength (Message m) = length m

Could achieve same in newtype solution via type classes.

GADTs

data Encrypted
data PlainText

> data MessageG a where
>   EncryptedMessageG :: String -> MessageG Encrypted
>   PlainTextMessageG :: String -> MessageG PlainText

typed value constructors enforce type of the Message.

> messageLengthG :: MessageG a -> Int
> messageLengthG (EncryptedMessageG m) = length m
> messageLengthG (PlainTextMessageG m) = length m

need to pattern match on both constructors.

> sendG :: MessageG Encrypted -> IO ()
> sendG (EncryptedMessageG m) = undefined

y = sendG (PlainTextMessageG "hello") -- type error
 error:
    - Couldn't match type ‘PlainText’ with ‘Encrypted’
      Expected type: MessageG Encrypted
        Actual type: MessageG PlainText

Or: run encryption inside send.

> sendE :: MessageG PlainText -> IO ()
> sendE (PlainTextMessageG m) = sendG (encryptG m)
>  where encryptG m' = undefined

What if multipe places where message needs encryption?
Also make encrypt do nothing for already encrypted messages.
If encrypt can fail, the need to handle failures in all places.
Also, code for constructing messages might in external lib.

Summary: Phantom Types for compiletime checkes
