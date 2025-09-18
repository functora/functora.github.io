---
title: Secure Bitcoin Seed
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

Because of how important your Bitcoin seed is, we cannot afford to take any risks. We cannot trust any online or offline digital device to generate the main part of the seed entropy, but we also cannot easily verify them. Therefore, we will not use any digital device to generate the first 23 words of the seed. The 24th word is a checksum that requires computing a SHA-256 hash, which is difficult to do manually. For this step, it is reasonable to use an offline, open-source, amnesic operating system such as Tails Linux.

### Tools

1. A good non-digital source of randomness. Coin is acceptable, but casino-grade dice is better.
2. A printed BIP39 dice [calculator](/bip39/calculator.html).
3. A printed BIP39 indexed [wordlist](/bip39/wordlist.html). Do not trust me. Verify the [script](https://github.com/functora/functora.github.io/blob/master/nix/bip39-wordlist.nix) and generate your own wordlist.
4. A pen or pencil.
5. A pocket calculator (optional).
6. A Linux Tails bootable USB stick (for calculating the 24th word only).
7. A second USB stick containing both the original BIP39 [wordlist](/bip39/wordlist.txt) and a Python [script](/bip39/24th-word-calculator.py) to calculate the 24th word. Do not trust me. The original BIP39 wordlist is available in the main Bitcoin BIPs [repository](https://github.com/bitcoin/bips/blob/master/bip-0039/english.txt), and the script is simple enough for review and verification.

### 1st-23th words

This is the main source of your seed entropy. When performing this step, or working with your seed at any point, it is absolutely essential to ensure there are no phones, laptops, cameras, or other devices in sight, and that no one is watching. For maximum security, choose a private room with no electronic devices present, close all doors and windows, and use only dice, paper, pen, and (optionally) a pocket calculator.
