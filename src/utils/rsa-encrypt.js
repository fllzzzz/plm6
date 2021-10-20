import JSEncrypt from 'jsencrypt/bin/jsencrypt'

// 密钥对生成 http://web.chacuo.net/netrsakeypair
// PKCS#8 1024bit

const publicKey = `MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQClJ9100OdfPnY1972bgAI+BJWs
h02NxlCBjowjNdw6A/miJm0NwSaZTutCwCO4MOnU57x7FYC4spL34byWsCaHswt1
s8RFqDxPHp5VjMrI9FUKimpXh6Ng6QmZNMFiTCJffs4C0vFWbEgq44d2EWb0bjtD
kAZ28Y43zMgOk9zzBwIDAQAB`

// -----BEGIN PUBLIC KEY-----
// MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQClJ9100OdfPnY1972bgAI+BJWs
// h02NxlCBjowjNdw6A/miJm0NwSaZTutCwCO4MOnU57x7FYC4spL34byWsCaHswt1
// s8RFqDxPHp5VjMrI9FUKimpXh6Ng6QmZNMFiTCJffs4C0vFWbEgq44d2EWb0bjtD
// kAZ28Y43zMgOk9zzBwIDAQAB
// -----END PUBLIC KEY-----

// 加密
export function encrypt(txt) {
  const encryptUtil = new JSEncrypt()
  encryptUtil.setPublicKey(publicKey) // 设置公钥
  return encryptUtil.encrypt(txt) // 对需要加密的数据进行加密
}

// // 解密
// export function decrypt(txt) {
//   const encryptUtil = new JSEncrypt()
//   encryptUtil.setPrivateKey(privateKey)
//   return encryptUtil.decrypt(txt)
// }

