/**
 * 存储工具类
 */
import { TokenKey, RequestUrlKey, tokenCookieExpires } from '@/settings/user'
import Storage from '@/classes/Storage'

const storage = new Storage()
export default storage

/**
 * 访问令牌：本地缓存操作
 * @returns
 */
export function getToken() {
  return storage.get(TokenKey)
}

export function setToken(token, rememberMe) {
  if (rememberMe) {
    return storage.set(TokenKey, token, tokenCookieExpires)
  } else return storage.set(TokenKey, token)
}

export function removeToken() {
  return storage.remove(TokenKey)
}

/**
 * 请求地址：本地缓存操作
 * @returns
 */
export function getRequestUrl() {
  return storage.get(RequestUrlKey)
}

export function setRequestUrl(url) {
  return storage.set(RequestUrlKey, url)
}

export function removeRequestUrl() {
  return storage.remove(RequestUrlKey)
}

