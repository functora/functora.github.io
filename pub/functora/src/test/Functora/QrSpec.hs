module Functora.QrSpec (spec) where

import qualified Codec.QRCode as QR
import qualified Codec.QRCode as QRCode
import Functora.Prelude
import Functora.Qr
import Functora.QrOrphan ()
import Test.Hspec
import qualified Prelude

sample :: Maybe QR.QRImage
sample =
  QR.encodeAutomatic
    (QR.defaultQRCodeOptions QR.L)
    QR.Iso8859_1OrUtf8WithoutECI
    ("21FA" :: Text)

sampleUri :: Text
sampleUri =
  "https://functora.github.io/apps/currency-converter/0.1.0.8/index.html?d=AAAAAAAABHC4tOpZeb3U7VIv4Z_-fcjuWk4QEum9c78R_XvhV-uQEGg3-thityayHJ1DJOKCAjjLliM95rwRo6HU24gbrbfLo2Wm7ARLy2Hh_fBmbWFnbGj9s7yQPsErbE2FJE_Tzeyo-a47L73i-7GkyeMlQqlhg_OV4w-3qQbw18Gma2-UA0X41uWpCLiz5QglXhHL87lX-bDv5iCXA_DTXZAErGoCJ9tJ91DsMOTQ_x8R6yfn-czJRQDNUV3p1kTdgQ3-3wM1pW4GSGsXrUseuY4eMMRkgga5Di542xRHN9jmfEcC32wkUnB6R8lug8QCqMuXsTj7P47vT2YjNk4SY7I4SMjKBpTa2jb5nFhUsbahSBRvkquPfHkiqsV-SLtUjbqNzBHMBBzxoWxcPK_8G7gJPHMI6wl2QZkBz1V-lEyLXO-VLql_w3crBRDvPh6WUNGL97OYXE9pbjz0M8D-VKMseQqelgpMv_bZ3PAL32dqi-JS2Q9K7yUR2SVn6BO9kEjFIYi0w7m4KtFuXeWev6LvpX8M6TOsDwY0ffqw5BqLvm_K4QyiGIavcCAlTn-T6bjvNa7ZZqsQHCyaNmnB8YjH_DFcFkRfWHKCzm_XNyvFHJ_jVztMpmd1gYMQayHpJXF-Ep2J22i9jW69LPX8eVEAOKzhBZKLfD1WR0ETnx0TREHb1mCMIVqz1ZJt7k30KoX-Sm79gMUqB5MQGLHgG-ITVOl9a0HO2ECFvWpAfQr2Um0K8KmH8zycH9aqPx_j4Bj5yFLIa9wQdpgf_yjr20scfrXElp99oPrpqrB_Ii4Baz5AM6aHTQFzHICAZMeDhhppfN9TXzwgf4ZN2IwdTxKOV0EOAcPOUuecPX1GsRWBDeS-V3HsVN_mivMVIjnIInqHoAOqGBIXLJ4EVsOsBiKBRGwrhxhWbAAw75KMmb7wd6u5V-RAakdD2vcmH5_MYZ1dJb4UQINGSqjKhDmr2Hj3MSZf3ybVl2Kwkp00oXbTDmSCiAcB_W_-1gusbjC56azsMrR3TNvWaWBFkqSmATdo0BOyWGIM0ypX9Vl00vsRkmk_Z-X9bBwYwhqSZ2EVrWmkMG82v1Em40dEQA8ykZg2Mf6lPTD_HJFQV3anu11Gxj_aFIZgdRQAa-eC7h6eLn5LHWLiU7GGJyOko-V7_ZnMSdnAI0aGOAkz9YOwvS2RK27_Zxm79KBfjuk38zRnDsSha7QKpUjhXUpNEntE8oijzMWF4TyKG_4v7IHWM15x9GBjsOTzB_26Mc325T6HjIXxSCpyqMyKWepfwY1HLBaLqPXqUt87kJvTrfQEeQQirU9AAoWyA6P2DgVMJbjG6fXpzCMJ_jw4rnwDE39NvYcNd0Sr0fydTJWUXIfJtkGki0GD7nfQG1Nio6Ei9_aI3BiVqodJYgllgGkdsFX-dpNAAIIRxSCkxhvmu2SWCus6NV65rBkj5Qa9UaYFE8EV85PSphk4gEOXRT-mnXITFeIlADRLJ1lc_6jEIGkf4qxdOca38wAAAAAAAAAgCzKH_v3TIaco55SBkdG1NU2_ZL5L7xIqNcVLqIQ-E68%3d&k=AAAAAAAAACBMOGy6GtTT49W1U8u6Bjcdni_DsgSAkkT29y_6XiSvPgAAAAAAAAAg5cTIv90uIZd8qvNKQvSjtV0clxgDaV_KgxRrA8k2npsAAAAAAAAAIHT1Rcgfx-xBRIVpXtCcdIehgAx7T2wbE2Frir5dnVr3AAAAAAAAACC7eq-GAYlHZ__KrJlke2Ftercv1QVjPK8Y1c7j8F93nw%3d%3d&s=Ag%3d%3d&p=AgAAAAAAAAAAAAAAAAAAAAAAAQE%3d"

spec :: Spec
spec = do
  it "sampleUri" $ do
    ts0 <- liftIO getCurrentTime
    let qr =
          QRCode.encodeAutomatic
            (QRCode.defaultQRCodeOptions QRCode.L)
            QRCode.Iso8859_1OrUtf8WithoutECI
            sampleUri
    let bmp1 = qrToBmp 0 5 <$> qr
    ts1 <- deepseq bmp1 $ liftIO getCurrentTime
    putStrLn . Prelude.show $ diffUTCTime ts1 ts0
    True `shouldBe` True
  it "qrToBmpDataUrlTL/0/0" $ do
    qrToBmpDataUrlTL @Text 0 0 <$> sample
      `shouldBe` Just
        "data:image/bmp;base64,Qk0aBwAAAAAAADYAAAAoAAAAFQAAABUAAAABACAAAAAAAOQGAAASCwAAEgsAAAAAAAAAAAAAAAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////8AAAD/AAAA/wAAAP8AAAD///////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA////////////////////////////AAAA////////////AAAA////////////AAAA//////8AAAD///////////////////////////8AAAD/AAAA//////8AAAD/AAAA/wAAAP//////AAAA////////////AAAA/wAAAP////////////////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD/AAAA//////8AAAD/AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD/AAAA//////8AAAD/AAAA/wAAAP//////AAAA////////////AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD/AAAA////////////////////////////AAAA////////////AAAA//////8AAAD///////////8AAAD///////////////////////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////8AAAD//////wAAAP//////AAAA//////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/////////////////////////////////////////////////AAAA////////////AAAA//////////////////////////////////////////////////////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD///////////8AAAD/AAAA/wAAAP////////////////8AAAD///////////8AAAD///////////8AAAD/AAAA/wAAAP///////////wAAAP////////////////8AAAD/AAAA//////8AAAD///////////////////////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP//////AAAA/////////////////////////////////wAAAP///////////wAAAP8AAAD/AAAA//////////////////////8AAAD/AAAA/wAAAP8AAAD///////////8AAAD/AAAA//////8AAAD/////////////////////////////////////////////////AAAA////////////AAAA/wAAAP8AAAD/AAAA/////////////////wAAAP////////////////8AAAD//////wAAAP8AAAD/AAAA/wAAAP8AAAD///////////////////////////////////////////8AAAD//////wAAAP///////////wAAAP//////AAAA//////8AAAD/AAAA/wAAAP//////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////////////////////8AAAD///////////8AAAD/AAAA//////////////////////8AAAD/AAAA////////////////////////////AAAA//////8AAAD/AAAA////////////////////////////AAAA/wAAAP8AAAD//////wAAAP8AAAD/AAAA//////8AAAD/AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP8AAAD/////////////////AAAA////////////AAAA//////8AAAD/AAAA//////8AAAD/AAAA/wAAAP//////AAAA////////////AAAA////////////AAAA//////8AAAD/////////////////AAAA/wAAAP//////AAAA//////8AAAD/AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP//////AAAA/////////////////wAAAP////////////////8AAAD/AAAA////////////////////////////AAAA////////////////////////////AAAA/wAAAP8AAAD/////////////////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA////////////AAAA/////////////////////////////////wAAAP//////AAAA//////8AAAD/"
  it "qrToBmpDataUrlTL/1/0" $ do
    qrToBmpDataUrlTL @Text 1 0 <$> sample
      `shouldBe` Just
        "data:image/bmp;base64,Qk16CAAAAAAAADYAAAAoAAAAFwAAABcAAAABACAAAAAAAEQIAAASCwAAEgsAAAAAAAAAAAAA////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////8AAAD/AAAA/wAAAP8AAAD///////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD///////////8AAAD///////////////////////////8AAAD///////////8AAAD///////////8AAAD//////wAAAP///////////////////////////wAAAP///////////wAAAP//////AAAA/wAAAP8AAAD//////wAAAP///////////wAAAP8AAAD/////////////////AAAA//////8AAAD/AAAA/wAAAP//////AAAA////////////AAAA//////8AAAD/AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD///////////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD///////////8AAAD/AAAA//////8AAAD//////wAAAP//////AAAA/wAAAP8AAAD//////wAAAP///////////wAAAP///////////////////////////wAAAP///////////wAAAP//////AAAA////////////AAAA////////////////////////////AAAA////////////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////8AAAD//////wAAAP//////AAAA//////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD///////////////////////////////////////////////////////////8AAAD///////////8AAAD/////////////////////////////////////////////////////////////////AAAA//////8AAAD/AAAA/wAAAP//////AAAA////////////AAAA/wAAAP8AAAD/////////////////AAAA////////////AAAA//////////////////////8AAAD/AAAA/wAAAP///////////wAAAP////////////////8AAAD/AAAA//////8AAAD///////////////////////////8AAAD///////////8AAAD/AAAA/wAAAP8AAAD/AAAA//////8AAAD/////////////////////////////////AAAA////////////AAAA/wAAAP8AAAD/////////////////////////////////AAAA/wAAAP8AAAD/AAAA////////////AAAA/wAAAP//////AAAA////////////////////////////////////////////////////////////AAAA////////////AAAA/wAAAP8AAAD/AAAA/////////////////wAAAP////////////////8AAAD//////wAAAP8AAAD/AAAA/wAAAP8AAAD//////////////////////////////////////////////////////wAAAP//////AAAA////////////AAAA//////8AAAD//////wAAAP8AAAD/AAAA/////////////////wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP//////////////////////AAAA////////////AAAA/wAAAP//////////////////////AAAA////////////AAAA////////////////////////////AAAA//////8AAAD/AAAA////////////////////////////AAAA/wAAAP8AAAD//////wAAAP8AAAD///////////8AAAD//////wAAAP8AAAD/AAAA//////8AAAD//////wAAAP//////AAAA/wAAAP////////////////8AAAD///////////8AAAD//////wAAAP///////////wAAAP//////AAAA/wAAAP8AAAD//////wAAAP///////////wAAAP///////////wAAAP//////AAAA/////////////////wAAAP8AAAD/////////////////AAAA//////8AAAD/AAAA/wAAAP//////AAAA//////8AAAD//////wAAAP//////AAAA/////////////////wAAAP////////////////8AAAD///////////8AAAD///////////////////////////8AAAD///////////////////////////8AAAD/AAAA/wAAAP////////////////8AAAD/AAAA/wAAAP///////////wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP///////////wAAAP////////////////////////////////8AAAD//////wAAAP//////AAAA/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////w=="
