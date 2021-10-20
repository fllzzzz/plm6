import { isNotBlank } from '@/utils/data-type'
import { toThousandFilter } from '@/utils/data-type/number'
/*eslint no-extend-native: ["error", { "exceptions": ["Array", 'Number', 'String'] }]*/

// 判断两个数组是否相等（包括数组里的键值是否相等）
Array.prototype.equals = function (array, key, order = true) {
  // array--要比较的数组（必需）
  // key--如果数组里边包裹着对象，则可以比较对象的某个键值对（可选）
  // order--数组中的顺序是否可以打乱（可选）
  if (!(array instanceof Array)) return false
  if (this.length !== array.length) return false
  if (order) {
    // 顺序要求一致
    for (let i = 0; i < this.length; i++) {
      if (isNotBlank(key)) {
        if (this[i][key] !== array[i][key]) {
          return false
        }
      } else {
        if (this[i] !== array[i]) {
          return false
        }
      }
    }
  } else {
    // 顺序可以不一致
    for (let i = 0; i < this.length; i++) {
      let log = false
      for (let j = 0; j < array.length; j++) {
        if (isNotBlank(key)) {
          if (this[i][key] === array[j][key]) {
            log = true
            break
          }
        } else {
          if (this[i] === array[j]) {
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
Object.defineProperty(Array.prototype, 'equals', { enumerable: false })

Array.prototype.remove = function (val) {
  // TODO: 优化，多个值
  var index = this.indexOf(val)
  if (index > -1) {
    this.splice(index, 1)
  }
}
Object.defineProperty(Array.prototype, 'remove', { enumerable: false })

Array.prototype.maxIndex = function () {
  var max = this[0]
  var index = 0
  for (var i = 0; i < this.length; i++) {
    if (max < this[i]) {
      max = this[i]
      index = i
    }
  }
  return index
}
Object.defineProperty(Array.prototype, 'maxIndex', { enumerable: false })

// 10000 => "10,000"
Number.prototype.toThousand = function () {
  return toThousandFilter(this)
}
Object.defineProperty(Number.prototype, 'toThousand', { enumerable: false })

Number.prototype.toFloor = function (num) {
  return Math.floor(this * Math.pow(10, num)) / Math.pow(10, num)
}
Object.defineProperty(Number.prototype, 'toFloor', { enumerable: false })

String.prototype.toThousand = function (num) {
  return toThousandFilter(num)
}
Object.defineProperty(String.prototype, 'toThousand', { enumerable: false })
