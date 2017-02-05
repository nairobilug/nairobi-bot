module Bot.TypesSpec where

import Test.Hspec

spec :: Spec
spec = describe "Type tests: " parseJSONTests

parseJSONTests :: Spec
parseJSONTests = do
  describe "NowPlaying parseJSON instance makes sense." $ do
    it "Now Playing" $
     pending
  describe "Definition parseJSON instance makes sense." $ do
    it "Definition" $
       pending
  describe "GIF parseJSON instance makes sense." $ do
    it "Reurns successfully decodes GIF JSON" $
      pending
    it "Has a gif ending in .gif" $
      pending
