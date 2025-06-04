{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}

module Stripe.Client
  ( -- * Basics
    ApiKey,
    StripeClient,
    makeStripeClient,
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    StripeList (..),

    -- * Customers
    createCustomer,
    retrieveCustomer,
    updateCustomer,
    listCustomers,
    CustomerId (..),
    Customer (..),
    CustomerCreate (..),
    CustomerUpdate (..),

    -- * Product catalog
    ProductId (..),
    PriceId (..),
    Product (..),
    Price (..),
    PriceRecurring (..),
    ProductCreate (..),
    PriceCreate (..),
    PriceCreateRecurring (..),
    createProduct,
    retrieveProduct,
    createPrice,
    retrievePrice,
    listPrices,

    -- * Subscriptions
    SubscriptionId (..),
    SubscriptionItemId (..),
    SubscriptionStatus (..),
    Subscription (..),
    SubscriptionItem (..),
    SubscriptionCreate (..),
    SubscriptionCreateItem (..),
    createSubscription,
    retrieveSubscription,
    cancelSubscription,
    listSubscriptions,

    -- * Customer Portal
    CustomerPortalId (..),
    CustomerPortal (..),
    CustomerPortalCreate (..),
    createCustomerPortal,

    -- * Checkout
    CheckoutSessionId (..),
    CheckoutSessionStatus (..),
    CheckoutSession (..),
    CheckoutSessionCreate (..),
    CheckoutSessionCreateLineItem (..),
    createCheckoutSession,
    retrieveCheckoutSession,

    -- * Invoices
    InvoiceId (..),
    InvoiceStatus (..),
    Invoice (..),
    retrieveInvoice,
    cancelInvoice,
    listInvoices,

    -- * Events
    retrieveEvent,
    listEvents,
    EventId (..),
    Event (..),
    EventData (..),
  )
where

import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager, Request (..))
import Servant.API
import Servant.Client
import Stripe.Api
import Stripe.Client.Internal.Helpers
import Stripe.Resources

-- | Your Stripe API key. Can be obtained from the Stripe dashboard. Format: @sk_<mode>_<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data StripeClient = StripeClient
  { scBasicAuthData :: BasicAuthData,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'StripeClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeStripeClient ::
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt. See also <https://stripe.com/docs/error-handling#safely-retrying-requests-with-idempotency Stripe Error Handling>
  Int ->
  StripeClient
makeStripeClient k = StripeClient (BasicAuthData (T.encodeUtf8 k) "")

api :: Proxy StripeApi
api = Proxy

stripeBaseUrl :: BaseUrl
stripeBaseUrl = BaseUrl Https "api.stripe.com" 443 ""

newClientEnv :: Manager -> BaseUrl -> ClientEnv
newClientEnv man url =
  env
    { makeClientRequest = mkReq
    }
  where
    env = mkClientEnv man url
    mkReq x y = do
      req <- makeClientRequest env x y
      pure
        req
          { requestHeaders =
              ("Stripe-Version", "2025-02-24.acacia")
                : requestHeaders req
          }

#define EP(N, ARG, R) \
    N##' :: BasicAuthData -> ARG -> ClientM R;\
    N :: StripeClient -> ARG -> IO (Either ClientError R);\
    N sc a = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a) (newClientEnv (scManager sc) stripeBaseUrl)

#define EP2(N, ARG, ARG2, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ClientM R;\
    N :: StripeClient -> ARG -> ARG2 -> IO (Either ClientError R);\
    N sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a b) (newClientEnv (scManager sc) stripeBaseUrl)

#define EP3(N, ARG, ARG2, ARG3, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ARG3 -> ClientM R;\
    N :: StripeClient -> ARG -> ARG2 -> ARG3 -> IO (Either ClientError R);\
    N sc a b c = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a b c) (newClientEnv (scManager sc) stripeBaseUrl)

EP (createCustomer, CustomerCreate, Customer)
EP (retrieveCustomer, CustomerId, Customer)
EP2 (updateCustomer, CustomerId, CustomerUpdate, Customer)
EP (listCustomers, Maybe CustomerId, (StripeList Customer))

EP (createProduct, ProductCreate, Product)
EP (retrieveProduct, ProductId, Product)

EP (createPrice, PriceCreate, Price)
EP (retrievePrice, PriceId, Price)
EP (listPrices, Maybe T.Text, (StripeList Price))

EP (createSubscription, SubscriptionCreate, Subscription)
EP (retrieveSubscription, SubscriptionId, Subscription)
EP (cancelSubscription, SubscriptionId, Subscription)
EP (listSubscriptions, Maybe CustomerId, (StripeList Subscription))

EP (retrieveInvoice, InvoiceId, Invoice)
EP (cancelInvoice, InvoiceId, Invoice)
EP (listInvoices, Maybe CustomerId, (StripeList Invoice))

EP (createCheckoutSession, CheckoutSessionCreate, CheckoutSession)
EP (retrieveCheckoutSession, CheckoutSessionId, CheckoutSession)

EP (createCustomerPortal, CustomerPortalCreate, CustomerPortal)

EP (retrieveEvent, EventId, Event)
EP (listEvents, Maybe EventId, (StripeList Event))

(createCustomer' :<|> retrieveCustomer' :<|> updateCustomer' :<|> listCustomers')
  :<|> (createProduct' :<|> retrieveProduct')
  :<|> (createPrice' :<|> retrievePrice' :<|> listPrices')
  :<|> ( createSubscription'
          :<|> retrieveSubscription'
          :<|> cancelSubscription'
          :<|> listSubscriptions'
        )
  :<|> ( retrieveInvoice'
          :<|> cancelInvoice'
          :<|> listInvoices'
        )
  :<|> (createCheckoutSession' :<|> retrieveCheckoutSession')
  :<|> (createCustomerPortal')
  :<|> (retrieveEvent' :<|> listEvents') =
    client api
