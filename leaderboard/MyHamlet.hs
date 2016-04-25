module MyHamlet (hamlet) where

import Text.Hamlet hiding (hamlet)

hamlet = hamletWithSettings htmlRules $ defaultHamletSettings { hamletNewlines = NoNewlines }
