module Main where


phonebook :: [(String, String)]
phonebook =
       [ ("Bob",   "953 777-44-45")
       , ("Fred",  "919 33-555-11")
       , ("Alice",  "383 11111111")
       , ("Jane",  "964 4000004") ]

type PhoneBook = [(String, String)]

validatePhones :: PhoneBook -> PhoneBook
validatePhones pb = map validate pb

validate :: (String, String) -> (String, String)
validate (name, phone) = (name, "+7 " ++ phone)