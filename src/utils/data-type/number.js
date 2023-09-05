/**
 * 数字位数不够，补0。 例 (1,3) => 001
 * @param {number} num 被操作数
 * @param {number} n 固定的总位数（default：2）
 */
export function prefixZero(num, n = 2) {
  if (num.toString().length >= n) return num
  return (Array(n).join(0) + num).slice(-n)
}

/**
 * 位运算结果转为数组
 * @param {number} num 数值
 */
export function getBitwiseBack(num) {
  const str = parseInt(num).toString(2)
  const re = new RegExp('\\d{1,' + 1 + '}', 'g')
  const ma = str.match(re).reverse()
  const arr = []
  ma.forEach((v, x) => {
    if (parseInt(v) === 1) {
      arr.push(Math.pow(2, x))
    }
  })
  return arr
}

export function getBits(options, val, type, { label = 'label', key = 'key', value = 'value' } = {}) {
  const bitArr = []
  for (const i in options) {
    if (options[i][value] & val) {
      switch (type) {
        case 'key':
          bitArr.push(options[i][key])
          break
        case 'value':
          bitArr.push(options[i][value])
          break
        case 'label':
          bitArr.push(options[i][label])
          break
        default:
          bitArr.push(options[i].value)
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
  }
  return _e.reduce((res, cur) => {
    return res | cur
  }, 0)
}

// 科学计数法转为普通数值
export function getFullNum(num) {
  if (isNaN(num)) return num
  const str = '' + num
  if (!/e/i.test(str)) return num
  return (+num).toFixed(18).replace(/\.?0+$/, '')
}

/**
 * 数字转大写中文
 *
 * @export
 * @param {*} n 数字
 * @returns
 */
export function digitUppercase(n) {
  const maxNum = 999999999999999.9999 // 最大处理的数字
  if (isNaN(+n)) {
    return ''
  }
  n = parseFloat(n)
  if (n >= maxNum) {
    // $.alert('超出最大处理数字')
    return ''
  }
  const fraction = ['角', '分', '厘', '毫']
  const digit = ['零', '壹', '贰', '叁', '肆', '伍', '陆', '柒', '捌', '玖']
  const unit = [
    ['', '拾', '佰', '仟'],
    ['', '万', '亿', '兆']
  ]
  const head = n < 0 ? '欠' : ''
  n = Math.abs(n)
  let s = ''
  let integerNum // 金额整数部分
  let decimalNum // 金额小数部分
  const money = n.toString() // 转换为字符串
  if (money.indexOf('.') === -1) {
    integerNum = money
    decimalNum = ''
  } else {
    const parts = money.split('.')
    integerNum = parts[0]
    decimalNum = parts[1].substr(0, 4)
  }
  if (decimalNum !== '') {
    // 小数部分
    const decLen = decimalNum.length
    for (let i = 0; i < decLen; i++) {
      const n = decimalNum.substr(i, 1)
      if (n !== '0') {
        s += digit[Number(n)] + fraction[i]
      }
    }
  }
  s = s || '整'
  if (parseInt(integerNum, 10) > 0) {
    // 获取整型部分转换
    let zeroCount = 0
    let is = ''
    const intLen = integerNum.length
    for (let i = 0; i < intLen; i++) {
      const n = integerNum.substr(i, 1)
      const p = intLen - i - 1
      const q = p / 4
      const m = p % 4
      if (n === '0') {
        zeroCount++
      } else {
        if (zeroCount > 0) {
          is += digit[0]
        }
        zeroCount = 0 // 归零
        is += digit[parseInt(n)] + unit[0][m]
      }
      if (m === 0 && zeroCount < 4) {
        is += unit[1][q]
      }
    }
    is += '元'
    // 整型部分处理完毕
    s = is + s
  }
  return head + s.replace(/^整$/, '零元整')
}

/**
 * 数字转中文小写
 * @param {number} num 数字
 * @returns
 */
export function dightLowercase(num) {
  if (!/^\d*(\.\d*)?$/.test(num)) {
    return '错误的数字'
  }
  var AA = ['零', '一', '二', '三', '四', '五', '六', '七', '八', '九']
  var BB = ['', '十', '百', '千', '万', '亿', '点', '']
  var a = ('' + num).replace(/(^0*)/g, '').split('.')
  var k = 0
  var re = ''
  for (var i = a[0].length - 1; i >= 0; i--) {
    switch (k) {
      case 0:
        re = BB[7] + re
        break
      case 4:
        if (!new RegExp('0{4}\\d{' + (a[0].length - i - 1) + '}$').test(a[0])) {
          re = BB[4] + re
        }
        break
      case 8:
        re = BB[5] + re
        BB[7] = BB[5]
        k = 0
        break
    }
    if (k % 4 === 2 && a[0].charAt(i + 2) !== 0 && a[0].charAt(i + 1) === 0) re = AA[0] + re
    if (a[0].charAt(i) !== 0) re = AA[a[0].charAt(i)] + BB[k % 4] + re
    k++
  }
  if (a.length > 1) {
    // 加上小数部分(如果有小数部分)
    re += BB[6]
    for (i = 0; i < a[1].length; i++) re += AA[a[1].charAt(i)]
  }
  return re
}

/**
 * 10000 => "10,000"
 * @param {number/string} num
 */
export function toThousand(num, precision = 2) {
  if (num === '' || num === undefined || num === null) {
    return num
  }
  if (isNaN(+num)) {
    return num
  } else {
    return (+num || 0).toFixed(precision).replace(/^-?\d+/g, (m) => m.replace(/(?=(?!\b)(\d{3})+$)/g, ','))
  }
}

/**
 * 获取精度
 */
export function getDP(num) {
  const arr = Number(num).toString().split('.')
  if (arr.length === 2) {
    return arr[1].length
  } else {
    return 0
  }
}
