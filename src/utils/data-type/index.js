import { isObjectValueEqual } from './object'
import { compareArray } from './array'

/**
 * 是否json数据
 * @param {*} str
 * @returns
 */
export function isJSON(str) {
  if (typeof str === 'string') {
    try {
      var obj = JSON.parse(str)
      if (typeof obj === 'object' && obj) {
        return true
      } else {
        return false
      }
    } catch (e) {
      console.log('error：' + str + ',' + e)
      return false
    }
  }
}

/**
 * 添加后缀
 * @param {*} value 值
 * @param {*} suffix 后缀
 * @returns
 */
export function addSuffix(value, suffix) {
  if (isBlank(value)) return value
  return value + suffix
}

/**
 * 空值显示
 * @export
 * @param {*} val 值
 * @param {string} [sign='-'] 空值显示字符
 * @returns
 */
export function emptyTextFormatter(val, sign = '-') {
  if (val === 0) {
    val = '0'
  }
  val = `${val || ''}`.trim()
  return val || sign
}

/**
 * toFixed
 * @param {number} num
 * @param {number} precision 精确度
 */
export function toFixed(num, precision, { toNum = false } = {}) {
  if (num === undefined || num === null || isNaN(+num)) return num
  // isNaN(num) 字符串类型的数字是false，所以使用 +num 转为数字,但为了保险在上面的isNaN中的num也使用+num
  if (toNum) {
    return +((+num).toFixed(precision))
  } else {
    return (+num).toFixed(precision)
  }
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

// 1.判断是否为空
// TODO: 待修改：不支持判断el(列如getElementById获得的数据)
// 不能用于部分解析后为 { } 的对象
export function isNotBlank(...arr) {
  if (!arr.length) {
    throw new Error('No parameters passed in')
  }
  for (const value of arr) {
    if (value === null || value === undefined || value === 'null' || value === 'undefined' || value === 'NaN' || value === '') {
      return false
    } else {
      let jsonStr = null
      try {
        jsonStr = JSON.stringify(value)
      } catch (error) {
        jsonStr = ''
      }
      if (jsonStr === '{}') {
        return false
      }
      if (jsonStr === '[]') {
        return false
      }
    }
  }
  return true
}

// 1.判断是否为空
export function isBlank(...arr) {
  if (!arr.length) {
    throw new Error('No parameters passed in')
  }
  for (const value of arr) {
    if (value === null || value === undefined || value === 'null' || value === 'undefined' || value === 'NaN' || value === '') {
      return true
    } else {
      let jsonStr = null
      try {
        jsonStr = JSON.stringify(value)
      } catch (error) {
        jsonStr = ''
      }
      if (jsonStr === '{}') {
        return true
      }
      if (jsonStr === '[]') {
        return true
      }
    }
  }
  return false
}

/**
 * 这只是深度复制的简单版本
 * 有很多边缘案例的错误
 * 如果要使用完美的深层副本，请使用lodash's _.cloneDeep
 * @param {Object} source
 * @returns {Object}
 */
export function deepClone(source) {
  if (!source && typeof source !== 'object') {
    throw new Error('error arguments', 'deepClone')
  }
  const targetObj = source.constructor === Array ? [] : {}
  Object.keys(source).forEach(keys => {
    if (source[keys] && typeof source[keys] === 'object') {
      targetObj[keys] = deepClone(source[keys])
    } else {
      targetObj[keys] = source[keys]
    }
  })
  return targetObj
}

// 转换数据 用于 值是ref、computed需要.value的情况
export function convertVal(val) {
  switch (val.constructor.name) {
    case 'RefImpl':
    case 'ComputedRefImpl': return val.value
    default: return val
  }
}
