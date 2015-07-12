module Tests.QuickMaker (quickMake) where

import Maker(Maker(),runMaker)
import AttoScoped(statefulParseOnly,value)
import Scoped(Error())

import Data.Text(Text)
import Text.Parsec.Pos(initialPos)

quickMake :: Maker a → Text → Either Error a
quickMake m s = case statefulParseOnly value (initialPos "test") s of
  Left _ → Left ("", Nothing)
  Right v → runMaker m v
