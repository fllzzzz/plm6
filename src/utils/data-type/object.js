import { deepClone } from './index'

// 判断对象内的值是否相同
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
      }
      return false
    }
  }
  return true
}

/**
 * 合并两个对象，后一个优先级更高
 * @param {Object} target
 * @param {(Object|Array)} source
 * @returns {Object}
 */
export function objectMerge(target, source) {
  if (typeof target !== 'object') {
    target = {}
  }
  if (Array.isArray(source)) {
    return source.slice()
  }
  Object.keys(source).forEach(property => {
    const sourceProperty = source[property]
    if (typeof sourceProperty === 'object') {
      target[property] = objectMerge(target[property], sourceProperty)
    } else {
      target[property] = sourceProperty
    }
  })
  return target
}
