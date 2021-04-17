module WelcomeEmail.App.Caps where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import WelcomeEmail.App.Data (AppError)
import WelcomeEmail.Shared.Boundary (Settings, TestMailPayload, TestMailResponse)
import WelcomeEmail.Shared.Template (EmailTemplate)

class Monad m <= ManageTemplate m where
  getTemplate :: m (Either AppError EmailTemplate)
  saveTemplate :: EmailTemplate -> m (Either AppError Unit)

instance manageTemplateHalogenM :: ManageTemplate m => ManageTemplate (HalogenM st act slots msg m) where
  getTemplate = lift getTemplate
  saveTemplate = lift <<< saveTemplate


class Monad m <= ManageSettings m where
  getSettings :: m (Either AppError Settings)
  saveSettings :: Settings -> m (Either AppError Unit)

instance manageSettingsHalogenM :: ManageSettings m => ManageSettings (HalogenM st act slots msg m) where
  getSettings = lift getSettings
  saveSettings = lift <<< saveSettings

class Monad m <= SendTestMail m where
  sendTestMail :: TestMailPayload -> m (Either AppError TestMailResponse)

instance sendTestMailHalogenM :: SendTestMail m => SendTestMail (HalogenM st act slots msg m) where
  sendTestMail = lift <<< sendTestMail
