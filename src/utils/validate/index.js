/**
 * 校验工具类
 */
import { luhn } from '@/utils'

/**
 * 外部：链接、邮箱、电话
 * @param {string} path
 * @returns {Boolean}
 */
export function isExternal(path) {
  return /^(https?:|mailto:|tel:)/.test(path)
}

// uri：https \ http \ ftp
export function validURI(uri) {
  const reg = /^(http|ftp|https):\/\/[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?/
  return reg.test(uri)
}

// localhost
export function validLocation(uri) {
  const reg = /^(http|ftp|https):\/\/localhost:[0-9]+[\w\-\@?^=%&amp;/~\+#]?/
  return reg.test(uri)
}

// 请求路径
export function validRequestUrl(uri) {
  const reg = /^(http|ftp|https):\/\/([\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?)|(localhost:[0-9]+[\w\-\@?^=%&amp;/~\+#]?)/
  return reg.test(uri)
}

// 起始符号
export function validStartSymbol(routePath, symbol) {
  const reg = new RegExp('^' + symbol)
  return reg.test(routePath)
}

/**
 * 小写英文字母
 * @param {string} str
 * @returns {Boolean}
 */
export function validLowerCase(str) {
  const reg = /^[a-z]+$/
  return reg.test(str)
}

/**
 * 大写英文字母
 * @param {string} str
 * @returns {Boolean}
 */
export function validUpperCase(str) {
  const reg = /^[A-Z]+$/
  return reg.test(str)
}

/**
 * 英文字母
 * @param {string} str
 * @returns {Boolean}
 */
export function validAlphabets(str) {
  const reg = /^[A-Za-z]+$/
  return reg.test(str)
}

/**
 * 邮箱
 * @param {string} email
 * @returns {Boolean}
 */
export function validEmail(email) {
  const reg = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
  return reg.test(email)
}

/**
 * 正数
 * @export
 * @param {*} num
 * @returns
 */
export function greaterThanZero(num) {
  const _num = +num
  return !isNaN(_num) && _num > 0
}

/**
 * 字符串
 * @param {string} str
 * @returns {Boolean}
 */
export function isString(str) {
  if (typeof str === 'string' || str instanceof String) {
    return true
  }
  return false
}

/**
 * 数组
 * @param {Array} arg
 * @returns {Boolean}
 */
export function isArray(arg) {
  if (typeof Array.isArray === 'undefined') {
    return Object.prototype.toString.call(arg) === '[object Array]'
  }
  return Array.isArray(arg)
}

/**
 * 银行卡号
 * @export
 * @param {*} str
 * @returns
 */
export function checkBankCardNumber(str) {
  if (str == null) {
    return false
  }
  // 6位IIN+最多12位自定义数字+1位校验数字
  // 注意ISO/IEC 7812-1:2017中重新定义8位IIN+最多10位自定义数字+1位校验数字
  // 这里为了兼容2017之前的版本，使用8~19位数字校验
  if (!str.match('^\\d{8,19}$')) {
    return false
  }
  // mod10 模十算法
  return luhn(str)
}
