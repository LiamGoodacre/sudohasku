module Download
  ( downloadAPuzzle,
  )
where

import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time.Calendar qualified as Time
import Data.Time.LocalTime qualified as Time
import Domain
import Network.HTTP.Simple as HTTP

missionCharToDigit :: Char -> Maybe (Maybe Digit)
missionCharToDigit = \case
  '0' -> Just Nothing
  '1' -> Just $ Just D1
  '2' -> Just $ Just D2
  '3' -> Just $ Just D3
  '4' -> Just $ Just D4
  '5' -> Just $ Just D5
  '6' -> Just $ Just D6
  '7' -> Just $ Just D7
  '8' -> Just $ Just D8
  '9' -> Just $ Just D9
  _ -> Nothing

missionIndices :: [CellLoc Digit]
missionIndices = do
  row <- digits
  col <- digits
  pure $ CellLoc (CellCol col) (CellRow row)

newtype SudokuDotComMission = SudokuDotComMission Grid
  deriving newtype (Show)

instance Aeson.FromJSON SudokuDotComMission where
  parseJSON = Aeson.withText "SudokuDotComMission" \txt -> do
    if Text.length txt /= 81
      then fail "Bad length"
      else pure ()
    case traverse missionCharToDigit (Text.unpack txt) of
      Nothing -> fail "Could not parse mission"
      Just parsedDigits ->
        pure $ SudokuDotComMission $ Map.fromList do
          zip missionIndices $
            parsedDigits <&> \case
              Just d -> Given d
              Nothing -> Input Nothing

data SudokuDotCom = SudokuDotCom
  { sdcMission :: SudokuDotComMission
  }

instance Aeson.FromJSON SudokuDotCom where
  parseJSON = Aeson.withObject "SudokuDotCom" \o -> do
    sdcMission <- o Aeson..: "mission"
    pure SudokuDotCom {..}

downloadAPuzzle :: IO (Maybe Grid)
downloadAPuzzle = do
  localToday <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
  request <-
    HTTP.parseRequest ("https://sudoku.com/api/dc/" <> Time.showGregorian localToday)
      <&> HTTP.addRequestHeader "X-Requested-With" "XMLHttpRequest"
  response <- HTTP.httpJSONEither request
  case HTTP.getResponseBody response of
    Left {} -> pure Nothing
    Right (SudokuDotCom (SudokuDotComMission grid)) -> pure $ Just grid
