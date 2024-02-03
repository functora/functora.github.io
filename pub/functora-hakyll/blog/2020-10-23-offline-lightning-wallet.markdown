---
title: Offline Lightning Wallet
---

Lightning Network (LN) is awesome payment method. It's trustless, instant, private and cheap. It scales well. It still just good old bitcoin, which is awesome. Of course, nothing is for free, and LN have some drawbacks. One of them is interactive nature of LN payments. Both sender and recipient have to be online to process the payment. This means that LN wallet is always hot, and in order to pay offline (for example in restaurant) customer needs to have a smartphone or other device with internet connection and access to LN node (which might be remote or hosted on device itself).

Smartphone is general purpose device, and it implies very complex hardware and software. Some components of smartphone hardware, operating systems and applications are formally open source, but they inevitably are tied with proprietary components. Even if both hardware and software are open source, complexity of modern smartphone is so overwhelming that complete security audit is very hard, almost impossible task. In case where proprietary components are involved (which is 99% of the cases), such audit is just meaningless. Hardware bitcoin wallets like Coldcard, Ledger and Trezor are there for a good reason. They are specialized devices, open source and simple enough to allow complete security audit. Attack surface of smartphone is too big for such critical application as bitcoin wallet. The same for LN nodes. Lightning network is relatively new thing, but we already have a great choice of specialized LN devices like Nodl, Dojo and Casa. But as I meantioned before, LN node should always be online. These devices are not very portable and require electricity and internet connection to operate. They are meant to be run in a safe environment, for example at home behind the Tor for maximum security and privacy. Let's make a couple of assumptions regarding hypothetical offline lightning payment:

- Customer has operational LN node at home.
- Customer wants to make a purchase in offline environment like a restaurant.
- Customer don't have a smatphone, or other device with internet connection.
- Customer and merchant do not necessarily trust each other.
- Merchant has access to his own LN node and internet connection.

And now let's discuss LN bill - hypothetical standard for trustless offline lightning payments with bills printed on paper.

<h4 class="text-center">Step 1 - bills emission</h4>

Customer (issuer) preemptively generates at home some amount of new random invoice preimages (R), calculates corresponding SHA256 hashes HR=SHA256(R) and assigns some amount of satoshis (D) to every pair of R/HR. In terms of fiat money world, these 3 values together are representing disposable banknote/bill/check/voucher:

- D is a bill denomination, the maximum amount of satoshis (sat) that the banknote owner can request (and guaranteed to receive) from the banknote issuer.
- HR is a bill serial number, unique identifier assigned by the issuer.
- R is the proof of bill ownership, in fiat money world it's a bill itself.

Let's call them LN bills = lightning network bills. Like in case of fiat banknotes, customer needs to have a reasonable supply of LN bills of different denominations to carry in form of paper in his offline physical wallet for different use cases. Weekly supply for one person might look like this:

| D (sat) | pcs |
| ------- | --- |
| 500k    | 1   |
| 200k    | 1   |
| 100k    | 2   |
| 70k     | 14  |
| 20k     | 7   |
| 10k     | 7   |

As you can see, there can be multiple bills of the same denomination (like fiat banknotes), but still every bill must have individual unique preimage R and hash HR (like fiat banknotes serial numbers). Software which communicates with merchants, bill database and LN node itself should be hosted in safe place. For example at home behind the Tor. Good option is to host it at the same machine where customer LN node is running. For simplicity let's just call all this software and databases as customer LN node.

<h4 class="text-center">Step 2 - bills printing</h4>

Now when there is fresh database of new unspent bills, customer can print them on paper. Preimage R is kind of inidividial "private key" for every bill - this is important proof of bill ownership, and should somehow be segregated from other data presented on the bill. The simplest case is just to print 2-sided bills where front side contains all public information, and back side contains private preimage R. Printed bill will contain:

- Customer LN node address. This LN node is running at home, and for better security, privacy and accessability it should be hosted as Tor hidden service (address should be in .onion domain namespace).
- Bill denomination D.
- Bill preimage hash HR.
- Bill preimage R (segreagated, for example in a back side of the bill)

For convenience all the information should be printed on the bill in both human-readable format (text) and machine-readable format (QR code). In some specific cases bill can contain some other additional info, which is completely optional. For example, it might be legal disclamer, information about issuer, issuer digital or physical signature, or just advertisement.

<h4 class="text-center">Step 3 - transaction</h4>

For simplicity let's say the price of the meal in a restaurant is close enough to denomination of some bill in customer wallet. Then customer can use a single bill to pay full price, but the process where multiple bills are involved is very similar. For simple example let's say price is 69k, and bill denomination is 70k.

1. Firstly, merchant scans front side of customer's 70k bill, merchant software verifies public bill data and generates HODL LN invoice with amount = 69k and preimage hash = HR from customer's 70k bill.
2. Then merchant prints invoice on paper and gives a copy to customer. Customer can verify that amount and preimage hash HR is correct. This step is needed only in situations where customer don't trust merchant at all, which is not usually the case. In most cases offline merchants don't have financial intentions to defraud their customers.
3. After customer checked and confirmed invoice (verbally or with physical signature), merchant LN node sends it to the customer LN node (address is on the bill). Customer LN node recognizes invoice preimage hash (because it's presented in bill database), verifies that invoice amount is less or equal to bill denomination (69k is less than 70k), and if everything is fine - sends the payment and marks this bill as spent.
4. Merchant LN node receives the payment in form of HTLC (hash time locked contract), but can't redeem the money yet. Merchant says customer that payment is received and preimage R is needed to resolve HTLC.
5. Customer reveals preimage R from the back side of the bill, merchant LN node resolves HTLC, payment is completed.

<h4 class="text-center">What can go wrong</h4>

- Basically everything what can go wrong in normal LN payment, for example routing failures, insufficient inbound/outbound capacity, etc. It's not specific to offline bills, any kind of online or offline LN wallet can have these issues. In this case payment did not go through at all, it's safe to destroy the bill and use any other payment method.
- Customer can accidentally lose his physical wallet full of bills. In this case he should go online as soon as possible and mark all lost bills in LN node database as invalid. But anyway, in this case the person who found the wallet can try to redeem all the bills while they are valid. To prevent this possibility, customer can preemptively set a pin code (P) for his LN node (and obviously not print it anywhere, especially on the bills). It will introduce additional step in merchant-customer interaction. Customer needs to enter pin code P which will be added to preimage hash HR and hashed again to get HP=SHA256(P+HR) which will be included into invoice (customer LN node will decline invoices without correct HP hash). In this case merchant can steal customer's pin code (which is useless without unspent bills), but random person on the street who finds the wallet can't discover the pin code easily.
- Merchant LN node goes offline after customer LN node has sent the payment, but before HTLC was resolved (money are locked but not redeemed).
  - If customer did not reveal preimage R yet - it's safe to destroy the bill and use any other payment method, because moneyback of LN payment is guaranteed after HTLC timeout by LN software. Or he can just reveal preimage R to complete the payment (explained below).
  - If customer did reveal preimage R - it's not a problem. In this case he actually did completed the payment. Merchant can verify preimage R in offline mode because HR=SHA256(R). It's responsibility of the merchant to go online and redeem the money before HTLC timeout occurs, customer already did all his part.
- Customer can try to defraud merchant, providing wrong preimage R. Or vice versa, merchant can try to defraud customer saying that provided preimage R is wrong, although it is correct. In both cases preimage R is easily verifiable even offline because HR=SHA256(R) and this hash is printed on both bill and invoice.
- Merchant can defraud customer, printing an invoice for expected amount of X sat, but actually sending to customer LN node the other invoice for bigger amount of Y sat. As soon as Y is smaller or equal to bill denomination D, the payment will go through. In previous transaction example, merchant can obtain 70k sat instead of expected 69k sat if he really wants to do so (but not more than 70k sat). That's why it's very important to choose denominations of the bills properly. Bill denomination should exactly match the price (fraud is impossible) or be slightly higher (fraud is insignificant).

There is one common thing to note here - in case of any described fraud, both merchant and customer have a printed and signed copy of bill and invoice, which is comprehensive and irrefutable cryptographical proof of the defending side correctness, which might be easily verified by any third party if needed.

<h4 class="text-center">More offline use cases</h4>

In all the examples above, the person who spends the bill (customer) is the same person who created the bill (issuer). Merchant redeems bill immediately and atomically, it's a part of described interactive offline purchase procedure. Merchant should never store unredeemed customer's bills in form of paper to redeem them later - this is NOT safe. Described procedure is the only way to be protected from double spend or other fraud. In trustless environment the spender should always be the issuer. But there is a couple of other interesting use cases where issuer and spender can be different people. They are based on assumption that issuer don't have financial intentions to defraud the spender.

For example parent can give their child 70k sat bill to pay for a lunch. Or employer can giveaway bills to their employees for similar purpose as part of the company benefits. Or maybe crypto exchange can sell for a fiat money some sort of tamper-proof lightning bills where preimage R is sealed inside envelope, and other bill attributes are printed on envelope itself. The great thing about such use cases is that spender is able to use LN payments without having LN node or even LN wallet. Because of this, LN bills are also very cool for airdrop purposes because it does not require any kind of preparation or software installation. You can just giveaway paper bills to the people. And it's possible to giveaway a very tiny fractions of bitcoin (like 100 sat) increasing amount of the people getting airdrop but keeping expenses the same (if compare with onchain bitcoins). Such micro amounts are not spendable onchain (because miner fees are higher), but they are completely spendable in LN.

Another example of meaningful person to person paper bill transfer are activites where trust is just irrelevant, for example charity and tipping. Charity and tipping are completely voluntary activities, and spender in such cases does not have any legal obligations. Tipping of invalid bills can be considered as bad manners but not as a felony, because amount of tips money is not defined by law and can be literally equal to zero. Customers can even give to waiter special tipping bills without printed human-readable denomination. Waiter collects bills during the day, and redeems whatever is redeemable in the evening. Of course redeem procedure can be automated with some sort of machine (similar to fiat bill counters).

<h4 class="text-center">Comparison with fiat banknotes</h4>

LN bills might look similar to fiat banknotes. They do share some common properties like physicality and static denomination. But in all other aspects they are actually almost opposite to each other. LN bills are a bit closer to pre-1971 fiat banknotes, because they both are representing strictly defined fraction of hard base money unit (bitcoin or gold) which can (in case of LN bills - should) be redeemed through some after-trade procedure. Modern fiat banknotes are not redeemable at all. Fiat banknotes are absolutely centralized - the state has a monopoly on printing them. LN bills in opposite are absolutely decentralized, everybody can print and use them, money market finally can have a real competition. Any market should be competitive to be healthy. Security model is also very different. Fiat banknotes do require trust. They have watermarks which are supposed to be hard to replicate, but I doubt it really works. LN banknotes don't require trust, they do require only cryptographical verification. In modern global world, cryptographical verification can scale. Trust can't.
