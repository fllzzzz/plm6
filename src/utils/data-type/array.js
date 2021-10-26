/**
 * 数组工具类
 */
import { isNotBlank } from '@data-type/index'

/**
 * 去除数组中 0,false,null,undefined 等
 * @param {Array} actual
 * @returns {Array}
 */
export function cleanArray(actual) {
  const newArray = []
  for (let i = 0; i < actual.length; i++) {
    if (actual[i]) {
      newArray.push(actual[i])
    }
  }
  return newArray
}

/**
 * 判断数组是否含重复元素
 * @param {*} arr
 * @returns
 */
export function arrIsRepeat(arr) {
  const hash = {}
  for (const i in arr) {
    if (hash[arr[i]]) return true
    hash[arr[i]] = true
  }
  return false
}

/**
 * 数组去重
 * @param {Array} arr
 * @returns {Array}
 */
export function uniqueArr(arr) {
  return Array.from(new Set(arr))
}

// 比较数组是否相同
export function compareArray(a, b, key, order = true) {
  // b--要比较的数组（必需）
  // key--如果数组里边包裹着对象，则可以比较对象的某个键值对（可选）
  // order--数组中的顺序是否可以打乱（可选）
  if (!(a instanceof Array)) return false
  if (!(b instanceof Array)) return false
  if (a.length !== b.length) return false
  if (order) {
    // 顺序要求一致
    for (let i = 0; i < a.length; i++) {
      if (isNotBlank(key)) {
        if (a[i][key] !== b[i][key]) {
          return false
        }
      } else {
        if (a[i] !== b[i]) {
          return false
        }
      }
    }
  } else {
    // 顺序可以不一致
    for (let i = 0; i < a.length; i++) {
      let log = false
      for (let j = 0; j < b.length; j++) {
        if (isNotBlank(key)) {
          if (a[i][key] === b[j][key]) {
            log = true
            break
          }
        } else {
          if (a[i] === b[j]) {
            log = true
            break
          }
        }
      }
      if (!log) {
        return false
      }
    }
  }
  return true
}

