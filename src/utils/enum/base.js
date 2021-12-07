import { isBlank, isNotBlank } from '@data-type/index'
import { getBitwiseBack } from '@data-type/number'

/**
 * 创建枚举对象(常量对象)
 * 以树的形式把对象的子孙对象都freeze
 * constantize实现递归freeze
 */
export function constantize(obj) {
  const KV = {}
  const VL = {}
  const VK = {}
  const V = {}
  const KEYS = Object.keys(obj)
  obj.ENUM = Object.assign({}, obj)
  KEYS.forEach(key => {
    const value = obj[key].V
    const label = obj[key].L
    KV[key] = value
    VL[value] = label
    VK[value] = key
    V[value] = obj[key]
  })
  obj.KV = KV // key - value
  obj.VL = VL // value : label
  obj.VK = VK // value : key
  obj.V = V // value : item
  obj.KEYS = KEYS // keys
  // Object.freeze(obj)
  // Object.keys(obj).forEach((key, i) => {
  //   if (typeof obj[key] === 'object') {
  //     constantize(obj[key])
  //   }
  // })
}

/**
 * 将enum转为以value为key
 * @export
 * @param {object} enumerate 枚举对象
 * @returns
 */
export function key2val(enumerate) {
  const newEnum = {}
  for (const key in enumerate) {
    newEnum[enumerate[key].V] = enumerate[key]
  }
  constantize(newEnum)
  return newEnum
}

/**
 * 对象转数组
 * @export
 * @param {*} obj 对象
 * @param {boolean} [newObj=true] 对象中的数据是否采用深拷贝
 * @returns
 */
export function toArr(obj, newObj = true) {
  let arr = []
  for (const key in obj.ENUM) {
    arr.push(obj[key])
  }
  if (newObj) {
    arr = JSON.parse(JSON.stringify(arr))
  }
  return arr
}

/**
 * 返回位包含的枚举值
 * @param {*} enumerate 枚举对象
 * @param {*} value 值
 * @returns 枚举值数组
 */
export function getBits(enumerate, value, type) {
  console.log('enumerate, value,', enumerate, value)
  const bitArr = []
  for (const i in enumerate) {
    if (enumerate[i].V & value) {
      switch (type) {
        case 'K':
        case 'V':
        case 'L': bitArr.push(enumerate[i][type])
          break
        default: bitArr.push(enumerate[i])
          break
      }
    }
  }
  return bitArr
}

/**
 * 返回枚举对象的位运算只和
 * @param {*} enumerate 枚举对象
 * @param {*} value 值
 * @returns 枚举值数组
 */
export function getBitsSum(enumerate) {
  let _e
  if (Array.isArray(enumerate)) {
    _e = enumerate
  } else if (typeof enumerate === 'object') { // enum
    _e = Object.keys(enumerate.VK)
  }
  return _e.reduce((res, cur) => {
    return res | cur
  }, 0)
}

/**
 * 根据位运算结果获取对应label
 * @param {*} enumerate 枚举类
 * @param {*} bitArr  位运算转来的数组
 * @param {String} separator  分隔符
 * @param {String} label 标题：字段名
 * @param {String} val  值：字段名
 */
export function getBitwiseLabel(enumerate, bitArr, separator, label = 'L', val = 'V') {
  if (isBlank(bitArr) && isBlank(enumerate)) {
    return
  }
  const resArr = []
  Object.keys(enumerate).forEach(key => {
    if (bitArr.includes(enumerate[key][val])) {
      resArr.push(enumerate[key][label])
    }
  })

  // 拼接字符串
  if (isNotBlank(separator)) {
    return resArr.join(separator)
  } else {
    return resArr
  }
}

/**
 * 根据位运算获取对应label
 * @param {*} enumerate 枚举类
 * @param {*} num  数值
 * @param {String} separator  分隔符
 * @param {String} label 标题：字段名
 * @param {String} val  值：字段名
 */
export function getLabelByBit(enumerate, num, separator, label = 'L', val = 'V') {
  return getBitwiseLabel(enumerate, getBitwiseBack(num), separator, label, val)
}

/**
 * 为对象中的字段 设置枚举对象中对应的其他字段
 * @export
 * @param {object} data 数据对象
 * @param {string} field 字段名
 * @param {string} needField 需要改变字段名
 * @param {object} enumerate 枚举对象
 * @param {string} enumField 枚举对象字段名
 * @param {string} [newEnumField=undefined] 需要获取的枚举对象字段名
 * @returns
 */
export function setEnumValue(data, field, needField, enumerate, enumField, needEnumField = undefined) {
  return data.map(o => {
    let newData
    Object.keys(enumerate).forEach((key, i) => {
      if (typeof enumerate[key] === 'object') {
        if (enumerate[key][enumField] === o[field]) {
          newData = enumerate[key]
        }
      }
    })
    if (needEnumField !== undefined && newData) {
      o[needField] = newData[needEnumField]
    }
    return o
  })
}
