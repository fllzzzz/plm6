/**
 * 计算字节长度
 * @param {string} input value
 * @returns {number} output value
 */
export function byteLength(str) {
  // returns the byte length of an utf8 string
  let s = str.length
  for (var i = str.length - 1; i >= 0; i--) {
    const code = str.charCodeAt(i)
    if (code > 0x7f && code <= 0x7ff) s++
    else if (code > 0x7ff && code <= 0xffff) s += 2
    if (code >= 0xdc00 && code <= 0xdfff) i--
  }
  return s
}

// 转驼峰
export function toCamel(str, sign) {
  const pattern = new RegExp('\\' + sign + '(\\w)', 'g')
  return str.replace(pattern, function (all, letter) {
    return letter.toUpperCase()
  })
}

/**
 * 创建唯一的字符串
 * @returns {string}
 */
export function createUniqueString() {
  const timestamp = +new Date() + ''
  const randomNum = parseInt((1 + Math.random()) * 65536) + ''
  return (+(randomNum + timestamp)).toString(32)
}

// 去前后空格
export function trimStr(str) {
  if (typeof str !== 'string') return str
  return str.replace(/(^\s*)|(\s*$)/g, '')
}
