// FIXME: 该工具类方法为考虑透明的情况

/**
 * 浅色
 * @param {string} color rgb/hex
 * @returns
 */
export function isLight(color) {
  const grayLevel = getGrayLevel(hex2Rgb(color))
  if (typeof grayLevel === 'number' && grayLevel >= 192) {
    return true
  }
  return false
}

/**
 * 深色
 * @param {string} color rgb/hex
 * @returns
 */
export function isDark(color) {
  const grayLevel = getGrayLevel(hex2Rgb(color))
  if (typeof grayLevel === 'number' && grayLevel < 192) {
    return true
  }
  return false
}

/**
 * 获取颜色深度
 * @param {string} rgb
 * @returns 深度
 */
export function getGrayLevel(rgb) {
  const rgbArr = rgb.replace(/(?:\(|\)|rgb|RGB)*/g, '').split(',')
  const grayLevel = rgbArr[0] * 0.299 + rgbArr[1] * 0.587 + rgbArr[2] * 0.114
  return grayLevel
}

// 随机浅色颜色
// TODO: 5678956789defdef 重复的原因
export function getLightColor() {
  return '#' +
    (function random(color) {
      return (color += '5678956789defdef'[Math.floor(Math.random() * 16)]) &&
        (color.length === 6) ? color : random(color)
    })('')
}

// 随机深色颜色
export function getDarkColor() {
  return '#' +
      (function random(color) {
        return (color += '0123401234abcabc'[Math.floor(Math.random() * 16)]) &&
          (color.length === 6) ? color : random(color)
      })('')
}

/**
 * 比较颜色是否相同
 * @param {string} a color
 * @param {string} b color
 */
export function compare(a, b) {
  return rgb2Hex(a).toLocaleLowerCase() === rgb2Hex(b).toLocaleLowerCase()
}

/**
 * 十六进制 转 rgb
 * @param {string} color
 * @returns rgb颜色
 */
export function hex2Rgb(color) {
  // 16进制颜色值的正则
  var reg = /^#([0-9a-fA-f]{3}|[0-9a-fA-f]{6})$/
  // 把颜色值变成小写
  color = color.toLowerCase()
  if (reg.test(color)) {
    // 如果只有三位的值，需变成六位，如：#fff => #ffffff
    if (color.length === 4) {
      var colorNew = '#'
      for (let i = 1; i < 4; i += 1) {
        colorNew += color.slice(i, i + 1).concat(color.slice(i, i + 1))
      }
      color = colorNew
    }
    // 处理六位的颜色值，转为RGB
    var colorChange = []
    for (let i = 1; i < 7; i += 2) {
      colorChange.push(parseInt('0x' + color.slice(i, i + 2)))
    }
    return 'RGB(' + colorChange.join(',') + ')'
  } else {
    return color
  }
}

/**
 * rgb 转 十六进制
 * @param {string} color
 * @returns 十六进制
 */
export function rgb2Hex(color) {
  // RGB颜色值的正则
  var reg = /^(rgb|RGB)/
  if (reg.test(color)) {
    var strHex = '#'
    // 把RGB的3个数值变成数组
    var colorArr = color.replace(/(?:\(|\)|rgb|RGB)*/g, '').split(',')
    // 转成16进制
    for (let i = 0; i < colorArr.length; i++) {
      var hex = Number(colorArr[i]).toString(16)
      if (hex === '0') {
        hex += hex
      }
      strHex += hex
    }
    return strHex
  } else {
    return String(color)
  }
}

export default {
  isLight, // 是否浅色
  isDark, // 是否深色
  compare, // 是否相同
  getLightColor, // 获取亮色
  getDarkColor, // 获取深色
  hex2Rgb, // 16进制 转 rgb
  rgb2Hex // rgb 转 16进制
}
