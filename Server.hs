{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import Yesod
import Text.Lucius

data ValidatorServer = ValidatorServer

mkYesod "ValidatorServer" [parseRoutes|
                           / HomeR GET
                          |]

instance Yesod ValidatorServer

--footer :: HtmlUrl ValidatorServer
footer = $(whamletFile "shakespeare/footer.hamlet")

getHomeR = defaultLayout $ do
  setTitle "CK2 Validator"
  toWidget $(luciusFile "shakespeare/root.lucius")
  toWidget $(whamletFile "shakespeare/main.hamlet")
--  toWidget $(whamletFile "shakespeare/footer.hamlet")


main = warp 3000 ValidatorServer
