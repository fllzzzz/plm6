
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
 * 10000 => "10,000"
 * @param {number/string} num
 */
export function toThousandFilter(num) {
  if (num === '') {
    return num
  }
  if (isNaN(+num)) {
    return num
  } else {
    return (+num || 0).toString().replace(/^-?\d+/g, m => m.replace(/(?=(?!\b)(\d{3})+$)/g, ','))
  }
}
