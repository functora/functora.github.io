---
title: Bitcoin Seed Security
---

Practicing financial sovereignty and Bitcoin self-custody comes with a surprising number of non-trivial challenges, but they can only catch you off guard if you are unaware of them or do not know how to counter them in advance. In this series of articles and tutorials, I will teach you how to use Bitcoin securely. There are many aspects to cover: technical, social, geopolitical, and economic. We will start with the basics and work through each area step by step. The first step is to create and back up a strong, high-entropy, and secure BIP39 Bitcoin seed. A good seed is the foundation of your financial sovereignty. If you do not generate it correctly, doing anything else with Bitcoin does not make much sense. Here is the main principle that applies to everything we will cover in these tutorials:

```
Do not trust. Verify.
```

Bitcoin is global money with a very limited supply. Criminals, national governments, and supranational entities, in other words globalists, all want your Bitcoin. It is safest to assume that everyone is a scammer, including software and hardware manufacturers. Modern software, hardware, computers, smartphones, and even specialized hardware wallets are very complex and often rely on proprietary components. For the average Bitcoin user, it is extremely difficult to verify that a device’s hardware and software are doing what they are supposed to do. This is especially true for devices running proprietary software and those capable of connecting to the internet. There are several very real risks when using a digital device to generate a Bitcoin seed:

1. Software or hardware bugs: Bad entropy results in a weak seed.
2. Malware on an online device: Your seed could be sent to an attacker.
3. Malware on an offline device: The seed might be pre-generated and known by an attacker beforehand.
4. Supply chain attacks: Your hardware wallet could be swapped with a malicious one during shipping.
5. Physical security risks: Buying a hardware wallet exposes your identity. You are essentially announcing to the world that you probably own Bitcoin.

Because of how important your Bitcoin seed is, we cannot afford to take any risks. We cannot trust any online or offline digital device to generate the main part of the seed entropy, but we also cannot easily verify them. Therefore, we will not use any digital device to generate the first 23 words of the seed. The 24th word is a checksum that requires computing a SHA-256 hash, which is difficult to do manually. For this step, it is reasonable to use an offline, open-source, amnesic operating system such as Linux Tails OS.

### Generator tools

1. A good non-digital source of randomness. A coin is acceptable, but casino-grade dice are better. You need a die with an even number of sides.
2. A printed BIP39 dice [calculator](/bip39/calculator.html).
3. A printed BIP39 indexed [wordlist](/bip39/wordlist.html). Do not trust me. Verify the [script](https://github.com/functora/functora.github.io/blob/master/nix/bip39-wordlist.nix) and generate your own wordlist.
4. A pen or pencil.
5. A pocket calculator (optional).
6. A Linux Tails OS bootable USB stick (for calculating the 24th word only).
7. A second USB stick containing both the original BIP39 [wordlist](/bip39/english.txt) and a Python [script](/bip39/24th-word-calculator.py) to calculate the 24th word. Do not trust me. The original BIP39 wordlist is available in the main Bitcoin BIPs [repository](https://github.com/bitcoin/bips/blob/master/bip-0039/english.txt), and the script is simple enough for review and verification.

### 1st-23th words

This is the main source of your seed entropy. When performing this step, or working with your seed at any point, it is absolutely essential to ensure there are no phones, laptops, cameras, or other devices in sight, and that no one is watching. For maximum security, choose a private room with no electronic devices present, close all doors and windows, and use only dice, paper, pen, and (optionally) a pocket calculator.

The BIP39 wordlist contains 2048 words. To randomly select one of them, you need exactly 11 bits of randomness, since 2<sup>11</sup> = 2048. For each of the first 23 words, repeat the following procedure:

1. Flip a coin 11 times or roll the dice 11 times.
   - Coin: heads = 0 bit, tails = 1 bit
   - Dice: even side = 0 bit, odd side = 1 bit
2. On the printed BIP39 dice calculator, for each of the 11 bits in the word column, cross out the corresponding bit weight if the bit value is 0, or circle the corresponding bit weight if the bit value is 1.
3. Calculate the sum of all circled bit weights in the word column. Ignore the crossed-out bit weights.
4. Add 1 to the sum because the BIP39 wordlist is indexed starting at 1 (not 0). The resulting number is the index of the word in the BIP39 wordlist.
5. Write down the corresponding BIP39 seed word, which you can find by its index in the printed BIP39 indexed wordlist.

Example:

1. Rolling the standard six-sided die:
   - Rolls: 3, 2, 1, 4, 4, 2, 5, 4, 5, 6, 1.
   - Parity: odd, even, odd, even, even, even, odd, even, odd, even, odd.
   - Bits: 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1.
2. Weights: 1, ~~2~~, 4, ~~8~~, ~~16~~, ~~32~~, 64, ~~128~~, 256, ~~512~~, 1024.
3. Sum: 1 + 4 + 64 + 256 + 1024 = 1349.
4. Index: 1349 + 1 = 1350.
5. Word: post.

### 24th word

To calculate the 24th word, a laptop or desktop computer is required. The ideal setup for working with the seed is a stateless, amnesic, permanently offline, air-gapped machine. However, maintaining a separate device solely for this purpose may not be practical for everyone. A reasonable middle ground is to use a stateless, amnesic Linux system, such as Tails OS, booted from a USB stick. This method works on any laptop or desktop with USB ports. The same surveillance countermeasures used in the previous step apply here, except for computer usage.

1. Boot Linux Tails OS from the USB stick. Do not create permanent storage if the OS prompts you.
2. Ensure the machine is not connected to the internet or any other network. Check the connection status in the system tray. If you are using an external USB network dongle or Ethernet cable, physically disconnect it from the machine. If you are using a wireless connection, turn off the router and modem.
3. Connect the second USB stick containing the original BIP39 wordlist and the 24th word calculator Python script. Make sure they are located in the same directory. Then run the script:

```shell
python3 ./24th-word-calculator.py
```

4. Follow the script’s instructions and enter the first 23 seed words. If everything is correct, the script will generate 8 possible candidates for the 24th word.
5. Choose one of the 8 proposed words using the dice-roll or coin-flip method, as described in the previous section. The only difference is that you need only 3 bits of entropy, since 2<sup>3</sup> = 8. Write the selected 24th word in its place on the printed BIP39 dice calculator sheet.
6. Turn off the computer and disconnect the USB sticks.

### Seed backup

Now you have a strong, high-entropy, and secure BIP39 Bitcoin seed. This seed will be used in the future to create or restore your Bitcoin wallets. It is extremely important and must be backed up securely.

It is a good idea to have multiple backups. The printed seed calculator sheet can be one of them. Store it in a location where only you have access. The same surveillance countermeasures used in the previous steps apply here. However, keep in mind that paper is not a reliable long-term storage medium. It burns easily, is vulnerable to water damage, and could even be accidentally destroyed by pets.

Remember: losing your seed means losing all of your Bitcoin. Therefore, you should create at least one additional backup on a more durable, non-digital medium. Never store your seed digitally, as digital storage is vulnerable to many types of attacks. Physical, durable, offline storage is the safest option.

One of the simplest and most effective backup methods is stamping the seed words onto a small piece of sheet metal. Avoid using aluminum, as it is very soft, melts easily, and is unlikely to survive a serious fire.

Titanium is the ideal material, but it can be expensive, and purchasing it from a Bitcoin-related company will expose your identity. A good compromise is steel sheet metal with a thickness of at least a few millimeters. It is inexpensive, widely available, and can often be obtained without raising any suspicion about your Bitcoin ownership.

### Backup tools

1. Steel sheet metal.
2. Angle grinder or metal snips.
3. Hammer.
4. Set of letter stamps for metal stamping.
5. Metal file (optional).
6. Paint (optional).

Use the angle grinder or snips to cut the sheet metal into a rectangle about the size of a credit card. Use the hammer and letter stamps to imprint the first four letters of each word in your seed phrase, in order from the 1st to the 24th word. You only need to back up the first four letters of each word because the BIP39 wordlist is designed so that the first four letters of every word are unique. This makes it easy to reconstruct the full word later, since the BIP39 wordlist is publicly available.

If you are precise and stamp only four letters per word, you should be able to fit the entire seed phrase onto a single credit card sized piece of sheet metal, especially if you use both sides. After stamping, smooth any remaining sharp edges with the angle grinder or metal file. You can then coat the backup plate with a few layers of paint to prevent corrosion, especially if you are using regular (non-stainless) steel.

Store your steel backup plate in a secure location that only you can access. The same surveillance countermeasures discussed in the previous steps apply here.

### Conclusion

You have created a strong, pure analog high-entropy seed and secured it with a durable backup. You are ready to take your first steps in interacting with the Bitcoin blockchain. This is just the beginning of your journey toward financial sovereignty. In upcoming articles, I will explore more essential concepts and best practices for using Bitcoin. See you soon, sovereign!
