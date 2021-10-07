module KvmMail.Server.Subscription.EmailTemplates where

import ThisPrelude

import Data.Number.Format (fixed, toStringWith)
import Data.String as S
import Data.String.Utils (padEnd, unsafeRepeat)
import KvmMail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Lang(..), Subscription, maxlat, maxlng, minlat, minlng)
import KvmMail.Shared.Boundary (EntryChange)
import KvmMail.Shared.Entry (Category(..), CustomLink, Entry)
import KvmMail.Shared.Entry as EK


outroHint :: Lang -> String
outroHint DE = """Weitere Hinweise und Tipps zur Nutzung, z.B. wie du interaktive Karten
per iframe auf deiner Webseite einbettest oder Papierkarten erstellst,
findest du hier: https://blog.vonmorgen.org"""
outroHint EN = """For usage recommendations and other tips, e.g. how to embed an interactive map in an iframe on your
website or how to create paper maps, see https://blog.vonmorgen.org"""

greetings :: Lang -> String
greetings DE = "Visionäre Grüße, dein Karte von morgen-Team"
greetings EN = "Visionary greetings from the Karte von morgen-Team"

confirmationMail :: Subscription -> String -> { subject :: String, body :: String }
confirmationMail sub url = { subject, body }
  where
  subject = case sub.lang of
    DE -> "Karte von morgen: Bitte bestätige deine Email-Adresse"
    EN -> "Karte von morgen: please confirm your email address"
  body = intro <> "\n\n"
    <> url <> "\n\n"
    <> greetings sub.lang <> "\n\n"
    <> outroHint sub.lang
  intro = case sub.lang of
    DE -> "Na du Weltverbesserer*,\n\n"
      <> "wir freuen uns, dass du bei der Karte von morgen mit dabei bist!\n\n"
      <> "Bitte bestätige deine Email-Adresse und aktiviere dein Abonnement hier:"
    EN -> "Hi there changemaker,\n\n"
      <> "we're excited that you're participating in the Karte von morgen!\n\n"
      <> "Please confirm your email address and activate your subscription here:"

digestMail :: Subscription -> Array EntryChange -> String -> { subject :: String, body :: String }
digestMail sub ecs unsubscribeUrl =
  { subject: case sub.lang of
      DE -> "Kvm - deine " <> sfreq <> " Zusammenfassung - " <> sub.title
      EN -> "Kvm - your " <> sfreq <> " digest - " <> sub.title
  , body: placeEmailBody sub.lang
    { introSentence: intro sub.lang
    , mainText: digestText sub.lang ecs
    , unsubscribeUrl }
  }
  where
  intro DE = "hier ist die " <> sfreq <> " Zusammenfassung für dein Abonnement *" <> sub.title <> "*.\n\n" <>
      "Folgende Einträge sind im Abschnitt (`" <> sbbox <> "`) " <> stags <> " " <> sChangeType <> " worden:"
  intro EN = "here comes the " <> sfreq <> " digest for your subscription *" <> sub.title <> "*.\n\n" <>
      "The following entries in the region (`" <> sbbox <> "`) " <> stags <> " have been " <> sChangeType <> ":"
  stags = case sub.tags of
    [] -> ""
    tags -> case sub.lang of
      DE -> "mit einem der Tags `" <> S.joinWith "`, `" tags <> "`"
      EN -> "with one of the tags `" <> S.joinWith "`, `" tags <> "`"
  sChangeType = case sub.changeType of
    NewEntries -> case sub.lang of
      DE -> "hinzugefügt"
      EN -> "added"
    AllEntries -> case sub.lang of
      DE -> "hinzugefügt oder verändert"
      EN -> "added or changed"
  bboxF = toStringWith (fixed 4)
  sbbox = S.joinWith ", " [ bboxF (minlat sub.bbox), bboxF (minlng sub.bbox), bboxF (maxlat sub.bbox), bboxF (maxlng sub.bbox) ]
  sfreq = case sub.frequency of
    Hour -> case sub.lang of
      DE -> "stündliche"
      EN -> "hourly"
    Day -> case sub.lang of
      DE -> "tägliche"
      EN -> "daily"
    Week -> case sub.lang of
      DE -> "wöchentliche"
      EN -> "weekly"

placeEmailBody :: Lang -> { introSentence :: String, mainText :: String, unsubscribeUrl :: String } -> String
placeEmailBody lang { introSentence, mainText, unsubscribeUrl } =
    hello <> "\n\n"
    <> introSentence <> "\n\n"
    <> mainText <> "\n\n"
    <> unsub <> "\n\n"
    <> unsubscribeUrl <> "\n\n"
    <> greetings lang <> "\n\n"
    <> outroHint lang
  where
  hello = case lang of
    DE -> "Hallo du Weltverbesserer*,"
    EN -> "Hi there changemaker,"
  unsub = case lang of
    DE -> "Du kannst dein Abonnement des Kartenbereichs hier abbestellen:"
    EN -> "You can terminate your subscription here:"

entryText :: Lang -> Entry -> String
entryText lang entry = "**" <> S.trim entry.title <> "** `(" <> S.joinWith " " (categoryText <$> entry.categories)
    <> ") - " <> sVersion <>"` - " <> seeOrEdit <> "\n\n```\n"
    <> entryOptLn 0 EK.Description (Just entry.description)
    <> entryArr "Tags" entry.tags (("    " <> _) <<< S.joinWith ", ")
    <> entryOptLn 0 EK.License (Just entry.license)
    <> entryOptLn 0 EK.Street entry.street
    <> entryOptLn 0 EK.Zip entry.zip
    <> entryOptLn 0 EK.City entry.city
    <> entryOptLn 0 EK.Country entry.country
    <> entryOptLn 0 EK.State entry.state
    <> entryOptLn 0 EK.ContactName entry.contactName
    <> entryOptLn 0 EK.Email entry.email
    <> entryOptLn 0 EK.Telephone entry.telephone
    <> entryOptLn 0 EK.Homepage entry.homepage
    <> entryOptLn 0 EK.OpeningHours entry.openingHours
    <> entryOptLn 0 EK.FoundedOn entry.foundedOn
    <> entryOptLn 0 EK.ImageUrl entry.imageUrl
    <> entryOptLn 0 EK.ImageLinkUrl entry.imageLinkUrl
    <> entryArr "Custom Links" entry.custom (S.joinWith "\n" <<< map customLinkText)
    <> "\n```\n"
  where
  sVersion = case entry.version of
    0 -> case lang of
      DE -> "Neuer Eintrag"
      EN -> "new entry"
    v -> case lang of
      DE -> "Version " <> show v
      EN -> "version " <> show v

  link = "https://kartevonmorgen.org/#/?entry=" <> entry.id
  seeOrEdit = case lang of
    DE -> "[anschauen oder bearbeiten](" <> link <> ")"
    EN -> "[see or edit](" <> link <> ")"

  entryOptLn :: Int -> EK.EntryKey -> Maybe String -> String
  entryOptLn pad key mbVal = case mbVal of
    Nothing -> ""
    Just val -> unsafeRepeat pad " " <> padEnd 17 skey <> val <> "\n"
    where
    skey = entryKeyLoc lang key <> ":"

  entryOpt :: String -> Maybe String -> String
  entryOpt key mbVal = case mbVal of
    Nothing -> ""
    Just val -> key <> ": " <> val

  customLinkText :: CustomLink -> String
  customLinkText cl = "  - " <> cl.url
    <> entryOpt ("\n    " <> entryKeyLoc lang EK.CustomLinkTitle) cl.title
    <> entryOpt ("\n    " <> entryKeyLoc lang EK.CustomLinkDescription) cl.description


entryArr :: forall a. String -> Array a -> (Array a -> String) -> String
entryArr key aVal aShow = case aVal of
  [] -> ""
  arr -> key <> ":\n" <> aShow arr <> "\n"

digestText :: Lang -> Array EntryChange -> String
digestText lang ecs = S.joinWith "\n" $ map (\ec -> entryText lang ec.entry) ecs

categoryText :: Category -> String
categoryText = case _ of
  NonProfit -> "Non-Profit"
  Commercial -> "Commercial"
  Event -> "Event"


entryKeyLoc :: Lang -> EK.EntryKey -> String
entryKeyLoc DE key = case key of
  EK.Description -> "Beschreibung"
  EK.License -> "Lizenz"
  EK.Street -> "Straße"
  EK.Zip -> "PLZ"
  EK.City -> "Stadt"
  EK.Country -> "Land"
  EK.State -> "Bundesland"
  EK.ContactName -> "Kontakt"
  EK.Email -> "E-Mail"
  EK.Telephone -> "Telefon"
  EK.Homepage -> "Webseite"
  EK.OpeningHours -> "Öffnungszeiten"
  EK.FoundedOn -> "Gegründet"
  EK.Categories -> "Kategorien"
  EK.Tags -> "Tags"
  EK.ImageUrl -> "Bild URL"
  EK.ImageLinkUrl -> "Bild Link URL"
  EK.Custom -> "Custom Links"
  EK.CustomLinkTitle -> "Link Titel"
  EK.CustomLinkDescription -> "Link Beschreibung"
entryKeyLoc EN key = case key of
  EK.Description -> "Description"
  EK.License -> "License"
  EK.Street -> "Street"
  EK.Zip -> "Zip"
  EK.City -> "City"
  EK.Country -> "Country"
  EK.State -> "State"
  EK.ContactName -> "Contact name"
  EK.Email -> "E-mail"
  EK.Telephone -> "Telephone"
  EK.Homepage -> "Website"
  EK.OpeningHours -> "Business hours"
  EK.FoundedOn -> "Founded on"
  EK.Categories -> "Categories"
  EK.Tags -> "Tags"
  EK.ImageUrl -> "Image URL"
  EK.ImageLinkUrl -> "Image link URL"
  EK.Custom -> "Custom links"
  EK.CustomLinkTitle -> "Link title"
  EK.CustomLinkDescription -> "Link description"
