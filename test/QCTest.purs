module Test.QCTest where

import ThisPrelude

import Test.Spec
import Test.Spec.Assertions

import Control.Monad.Gen (chooseBool)
import Control.Monad.Gen.Common (genMaybe')
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (execStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.JSDate (JSDate, fromTime, getTime)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.String as S
import Data.String.Gen (genAlphaString)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (error, throwError)
import Test.Data (defEntry)
import Test.Data as Data
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.QuickCheck.Gen as QCG
import Test.SensData as SData
import Test.Util (mkDate)
import KvmMail.Server.Subscription.Entities (BBox, ChangeType(..), Frequency(..), Lang(..), LatLng, Subscription, intersects, maxlat, maxlng, minlat, minlng, mkBBox)
import KvmMail.Shared.Boundary (EntryChange)
import KvmMail.Shared.Entry (Category(..), Entry)

qcSpec :: Spec Unit
qcSpec = do
  describe "qc tests" do
    it "rocks" do
      quickCheck \n -> n + 1 > n
    -- it "bocks" do
    --   -- quickCheckGen $
    --   arr <- liftEffect $ QCG.randomSample' 1 $ QCG.vectorOf 2 (arbitrary :: Gen BBox)
    --   liftEffect $ log $ show arr
    -- it "docks" do
    --   quickCheck \bb1 bb2 -> intersects bb1 bb2 QC.<?> ("no intersect " <> show bb1 <> show bb2)
    it "intersects" do
      let bb1 = mkBBox { lng: 9.1837, lat: 48.8891 } { lng: 9.1941, lat: 48.9021 }
      let bb2 = mkBBox { lng: 9.2118, lat: 48.9021 } { lng: 9.1950, lat: 48.8891 }
      let bb3 = mkBBox { lng: 9.21, lat: 48.90 } { lng: 9.19, lat: 48.88 }
      intersects bb1 bb2 `shouldEqual` false
      intersects bb1 bb1 `shouldEqual` true
      intersects bb1 bb3 `shouldEqual` true
    -- it "mocks" do
      -- quickCheck' 2 genProp
    -- it "email" do
    --   let spl eml = fromMaybe "" $ A.head $ A.drop 1 $ S.split (S.Pattern "@") eml
    --   let x = A.nub $ map spl SData.emails
    --   liftEffect $ log $ show x




genEmailAddr :: Gen String
genEmailAddr = do
  nameGen <- QCG.elements $ fromMaybe (NEA.singleton "email-generation-failed") (NEA.fromArray SData.emailfront)
  domainGen <- QCG.elements $ fromMaybe (NEA.singleton "generation.failed.info") (NEA.fromArray SData.emaildomain)
  pure $ S.joinWith "@" [ nameGen, domainGen ]

genFullName :: Gen String
genFullName = do
  firstName <- QCG.elements $ fromMaybe (NEA.singleton "failed") (NEA.fromArray $ Data.germanFirstNamesM <> Data.germanFirstNamesW)
  lastName <- QCG.elements $ fromMaybe (NEA.singleton "failed") (NEA.fromArray Data.germanLastNames)
  pure $ S.joinWith " " [ firstName, lastName ]

genStreet :: Gen String
genStreet = do
  name <- QCG.elements $ fromMaybe (NEA.singleton "city generation failed") (NEA.fromArray Data.germanLastNames)
  str <- QCG.elements $ "straße" `NEA.cons'` [ "strasse", "str.", "gasse", "weg" ]
  num <- QCG.chooseInt 1 200
  pure $ name <> str <> " " <> show num

genCity :: Gen String
genCity = QCG.elements $ fromMaybe (NEA.singleton "city generation failed") (NEA.fromArray Data.germanTowns)

genCountry :: Gen String
genCountry = QCG.elements $ fromMaybe (NEA.singleton "country generation failed") (NEA.fromArray Data.countryExamples)

genState :: Gen String
genState = QCG.elements $ fromMaybe (NEA.singleton "state generation failed") (NEA.fromArray Data.germanStates)

genTelephone :: Gen String
genTelephone = QCG.elements $ fromMaybe (NEA.singleton "tel generation failed") (NEA.fromArray Data.telephoneExamples)

genOftbTag :: Gen String
genOftbTag = QCG.elements $ fromMaybe (NEA.singleton "tag generation failed") (NEA.fromArray SData.tags)

genZip :: Gen String
genZip = (QCG.vectorOf 5 $ genDigitChar) >>= pure <<< S.fromCodePointArray <<< map S.codePointFromChar

genCategory :: Gen Category
genCategory = QCG.elements $ NonProfit `NEA.cons'` [ Commercial, Event ]

genId :: Gen String
genId = do
  arr <- QCG.vectorOf 16 $ QCG.frequency $ (52.0 /\ genAlpha) `NEL.cons'` ((9.0 /\ genDigitChar) L.: L.Nil)
  pure $ S.fromCodePointArray $ map S.codePointFromChar arr

genOfdbId :: Gen String
genOfdbId = do
  let hex = 'a' `NEA.cons'` ['b', 'c', 'd', 'e', 'f']
  arr <- QCG.vectorOf 32 $ QCG.oneOf $ genDigitChar `NEA.cons'` [ genDigitChar, QCG.elements hex ]
  pure $ S.fromCodePointArray $ map S.codePointFromChar arr

genLang :: Gen Lang
genLang = QCG.elements $ EN `NEA.cons'` [ DE ]

genFrequency :: Gen Frequency
genFrequency = QCG.elements $ Hour `NEA.cons'` [ Day, Week ]

genChangeType :: Gen ChangeType
genChangeType = QCG.elements $ AllEntries `NEA.cons'` [ NewEntries ]

genJSDate :: Gen JSDate
genJSDate = QCG.choose 1e12 (-1e12) >>= fromTime >>> pure

chooseJSDate :: JSDate -> JSDate -> Gen JSDate
chooseJSDate a b = QCG.choose (getTime a) (getTime b) >>= fromTime >>> pure

defaultDateRange :: Gen JSDate
defaultDateRange = chooseJSDate (mkDate 2017 1 1 0 0) (mkDate 2021 1 1 0 0)

genEntry :: Gen Entry
genEntry = do
  id <- genOfdbId
  created <- defaultDateRange
  version <- QCG.oneOf $ QCG.chooseInt 0 49 `NEA.cons'` [ pure 0, pure 0, pure 1 ]
  title <- genAlphaString
  desc <- genAlphaString
  street <- genMaybe' 0.5 genStreet
  zip <- genMaybe' 0.5 genZip
  city <- genMaybe' 0.5 genCity
  country <- genMaybe' 0.5 genCountry
  state <- genMaybe' 0.5 genState
  contactName <- genMaybe' 0.5 genFullName
  email <- genMaybe' 0.5 genEmailAddr
  tel <- genMaybe' 0.5 genTelephone
  category <- genCategory
  tags <- QCG.arrayOf genOftbTag
  pure $ defEntry
    { id = id
    , created = created
    , version = version
    , title = "title " <> title
    , description = "desc " <> desc
    , categories = [ category ]
    , tags = tags
    , street = street
    , zip = zip
    , city = city
    , country = country
    , state = state
    , contactName = contactName
    , email = email
    , telephone = tel
    }

genEntryInBBox :: BBox -> Gen Entry
genEntryInBBox bbox = do
  entry <- genEntry
  { lat, lng } <- genLatLngInBBox bbox
  pure $ entry
    { lat = lat
    , lng = lng
    }

genEntryForSub :: Subscription -> Gen Entry
genEntryForSub sub = do
  entry <- genEntryInBBox sub.bbox
  pure $ entry

genEntryChangeForSub :: Gen JSDate -> Subscription -> Gen EntryChange
genEntryChangeForSub genDate sub = do
  entry <- genEntryForSub sub
  changed <- genDate
  pure { changed, entry }

genSubsNoIntersect :: Int -> Gen (Array Subscription)
genSubsNoIntersect n = do
  bboxes <- genBBoxesNoIntersect n
  let
    sub :: BBox -> Gen Subscription
    sub bbox = do
      id <- genId
      created <- defaultDateRange
      secret <- genId
      email <- genEmailAddr
      lang <- genLang
      lastSent <- chooseJSDate (mkDate 2017 1 1 0 0) (mkDate 2021 1 1 0 0)
      pure
        { id, title: "Subscription Title", secret, bbox, email, lang, lastSent, created
        , changeType: AllEntries, frequency: Hour, confirmed: true, tags: []
        }
  traverse sub bboxes

genSubscription :: Gen Subscription
genSubscription = do
  id <- genId
  bbox <- genBBox
  secret <- genId
  email <- genEmailAddr
  lang <- genLang
  lastSent <- defaultDateRange
  created <- defaultDateRange
  confirmed <- chooseBool
  frequency <- genFrequency
  changeType <- genChangeType
  tags <- QCG.arrayOf genOftbTag
  pure
    { id, title: "Subscription Title", secret, bbox, lang, email, lastSent
    , changeType, frequency, confirmed, tags, created
    }

-- genProp :: Number -> Gen QC.Result
-- genProp a = do
--   -- x <- QCG.choose (-90.0) 90.0
--   let d = mkDate 2017 1 1 0 0
--   x <- chooseJSDate (d) (DFN.add { months: 1 } d)
--   y <- chooseJSDate (x) (DFN.add { months: 1 } x)
--   eml <- genBBoxesNoIntersect 15
--   pure $ false QC.<?> ("val " <> toUTCString x <> " | " <> show eml)

genBBoxesNoIntersect :: Int -> Gen (Array BBox)
genBBoxesNoIntersect n = flip execStateT [] do
  let
    go i
      | i > 0 = do
        bboxes <- get
        let intersectsPrevs bb = foldl (\acc bb' -> if acc then acc else intersects bb' bb) false bboxes
        bbox <- lift $ suchThat genBBox (not intersectsPrevs)
        modify_ $ flip A.snoc bbox
        pure $ Loop $ i - 1
      | otherwise = pure $ Done unit
  tailRecM go n


genBBox :: Gen BBox
genBBox = do
  lat1 <- QCG.choose (-90.0) 90.0
  lat2 <- QCG.choose (-90.0) 90.0
  lng1 <- QCG.choose (-180.0) 180.0
  lng2 <- QCG.choose (-180.0) 180.0
  pure $ mkBBox { lat: lat1, lng: lng1 } { lat: lat2, lng: lng2 }

genLatLngInBBox :: BBox -> Gen LatLng
genLatLngInBBox bbox = do
  lat <- QCG.choose (minlat bbox) (maxlat bbox)
  lng <- QCG.choose (minlng bbox) (maxlng bbox)
  pure { lat, lng }

genEntryFull :: Gen Entry
genEntryFull = do
  entry <- genEntry
  lat <- QCG.choose (-90.0) 90.0
  lng <- QCG.choose (-180.0) 180.0
  street <- genMaybe' 1.0 genStreet
  zip <- genMaybe' 1.0 genZip
  city <- genMaybe' 1.0 genCity
  country <- genMaybe' 1.0 genCountry
  state <- genMaybe' 1.0 genState
  contactName <- genMaybe' 1.0 genFullName
  email <- genMaybe' 1.0 genEmailAddr
  tel <- genMaybe' 1.0 genTelephone
  category <- genCategory
  tags <- QCG.arrayOf genOftbTag
  let customLink = { url: "custom url", title: Just "custom title", description: Just "custom description" }
  pure $ entry
    { lat = lat
    , lng = lng
    , categories = [ category ]
    , tags = tags
    , street = street
    , zip = zip
    , city = city
    , country = country
    , state = state
    , contactName = contactName
    , email = email
    , telephone = tel
    , homepage = Just "Website"
    , openingHours = Just "Opening Hours"
    , foundedOn = Just "Founded On"
    , imageUrl = Just "Image Url"
    , imageLinkUrl = Just "Image Link Url"
    , custom = [ customLink, customLink ]
    }





-- | Runs a Testable with a random seed and 100 inputs.
quickCheck :: forall p.
              (QC.Testable p) =>
              p ->
              Aff Unit
quickCheck = quickCheck' 10

-- | Runs a Testable with a random seed and the given number of inputs.
quickCheck' :: forall p.
               (QC.Testable p) =>
               Int ->
               p ->
               Aff Unit
quickCheck' n prop = do
  seed <- liftEffect QC.randomSeed
  quickCheckPure seed n prop

getErrorMessage :: QC.Result -> Maybe String
getErrorMessage (QC.Failed msg) = Just msg
getErrorMessage _ = Nothing

-- | Runs a Testable with a given seed and number of inputs.
quickCheckPure :: forall p.
                  (QC.Testable p) =>
                  QC.Seed ->
                  Int ->
                  p ->
                  Aff Unit
quickCheckPure seed n prop = do
  let results = QC.quickCheckPure seed n prop
  let msgs = L.mapMaybe getErrorMessage results

  if L.length msgs > 0
    then throwError $ error $ L.intercalate "\n  " msgs
    else pure unit
