{-# LANGUAGE TemplateHaskell #-}

module WiseConstructor where


import WiseConstructorTH


data ADT = Zero
         | One Char
         | Two Int String
         

mkMapper "mapper" "" ''ADT

zero = undefined
one = undefined
two = undefined


-- target functions:
-- mapper (Zero)      = zeroCall
-- mapper (One x1)    = oneCall x1
-- mapper (Two x1 x2) = twoCall x1 x2

{-

toHttpMethod (SearchDialogs q)             = Vk.searchDialogs q
toHttpMethod (Authorize appId permissions) = Vk.authorization appId permissions
toHttpMethod (GetHistory offset msgCount userId chatId startMessageId rev) = 
    Vk.getHistory offset msgCount userId chatId startMessageId rev
    
    -}
