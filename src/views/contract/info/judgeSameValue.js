import { isNotBlank, deepClone } from '@data-type/index'
import { compareArray } from '@data-type/array'

export function isObjectValueEqual(a, b) {
  a = deepClone(a) // 深拷贝可以将proxy（代理对象）转换为普通对象
  b = deepClone(b)
  // 取对象a和b的属性名
  var aProps = Object.getOwnPropertyNames(a)
  var bProps = Object.getOwnPropertyNames(b)
  // 判断一和判断二可以保证是否真的相等
  // 判断一，判断属性名的length是否一致
  if (aProps.length !== bProps.length) {
    return false
  }

  // 判断二，循环取出属性名，再判断属性值是否一致
  for (var i = 0; i < aProps.length; i++) {
    var propName = aProps[i]
    if (propName !== '__ob__' && a[propName] !== b[propName]) {
      if (typeof a[propName] === 'object') {
        return isObjectValueEqual(a[propName], b[propName])
      } else if (a[propName] || b[propName]) {
        return false
      }
    }
  }
  return true
}
/**
 * 比较两个值是否相同
 * TODO:待优化
 * @param {*} a 比较的值
 * @param {*} b 比较的值
 * @returns
 */
export function judgeSameValue(a, b) {
  if (!isNotBlank(a) && !isNotBlank(b)) {
    return true
  }
  if (!isNotBlank(a) || !isNotBlank(b)) {
    return false
  }
  if (a.constructor === b.constructor) {
    const constructor = a.constructor
    switch (constructor) {
      case Object:
        return isObjectValueEqual(a, b)
      case Array:
        return compareArray(a, b)
      default:
        return a === b
    }
  } else {
    return false
  }
}
