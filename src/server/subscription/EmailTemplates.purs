module WelcomeEmail.Server.Subscription.EmailTemplates where

import Prelude

dateTimeFormat = "%Y.%m.%d %H:%M:%S" :: String

introEntryCreated = "ein neuer Eintrag auf der Karte von morgen wurde erstellt" :: String

introEntryUpdated = "folgender Eintrag auf der Karte von morgen wurde verändert" :: String

outroHint = """Weitere Hinweise und Tipps zur Nutzung, z.B. wie du interaktive Karten
per iframe auf deiner Webseite einbettest oder Papierkarten erstellst,
findest du hier: https://blog.vonmorgen.org""" :: String


confirmationMail :: String -> { subject :: String, body :: String }
confirmationMail url = { subject, body }
  where
  subject = "Karte von morgen: Bitte bestätige deine Email-Adresse"
  body = """Na du Weltverbesserer*,

wir freuen uns, dass du bei der Karte von morgen mit dabei bist!

Bitte bestätige deine Email-Adresse und aktiviere deine Subscription hier:

""" <> url <> """

euphorische Grüße,
das Karte von morgen-Team

""" <> outroHint


subjectEntryCreated :: String -> String
subjectEntryCreated title = "Kvm - neuer Eintrag: " <> title

subjectEntryUpdated :: String -> String
subjectEntryUpdated title = "Kvm - Eintrag verändert: " <> title

placeCreatedMail :: String -> { subject :: String, body :: String }
placeCreatedMail unsubscribeUrl = { subject, body }
  where
  subject = subjectEntryCreated "{title}"
  body = placeEmailBody { unsubscribeUrl, introSentence: introEntryCreated }

placeEmailBody :: { introSentence :: String, unsubscribeUrl :: String } -> String
placeEmailBody { introSentence, unsubscribeUrl } = """Hallo,

""" <> introSentence <> """:

{title} ({category})
{description}\n
    Tags: {tags}
    Adresse: {address_line}
    Webseite: {homepage}
    Email-Adresse: {email}
    Telefon: {phone}\n
Eintrag anschauen oder bearbeiten:
https://kartevonmorgen.org/#/?entry={id}

Du kannst dein Abonnement des Kartenbereichs hier abbestellen:

""" <> unsubscribeUrl <> """

euphorische Grüße,\n
das Karte von morgen-Team

""" <> outroHint
