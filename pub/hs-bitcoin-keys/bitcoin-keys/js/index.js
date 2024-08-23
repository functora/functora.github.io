import S from 'secp256k1';

export const prvVerify = S.privateKeyVerify;
export const pubVerify = S.publicKeyVerify;

export function pubAddTweak(pub, tweak) {
    const pub1 = pub.slice();
    try { return S.publicKeyTweakAdd(pub1, tweak, true); }
    catch { return null; }
};

export function prvAddTweak(prv, tweak) {
    const prv1 = prv.slice();
    try { return S.privateKeyTweakAdd(prv1, tweak); }
    catch { return null; }
};

export function prvToPub(prv) {
    return S.publicKeyCreate(prv, true);
};

export function pubUncompressed(pubc) {
    return S.publicKeyConvert(pubc, false);
}
