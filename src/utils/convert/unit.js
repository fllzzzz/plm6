/**
 * 单位转换工具类
 */
import convert from 'convert-units'

/**
 * 单位转换
 * @param {number} num 数值
 * @param {string} from 转换前单位
 * @param {string} to 转换后单位
 * @param {number} decimals 小数
 * @param {boolean} showUnit 是否显示单位
 * @param {boolean} isNum 是否转为数字
 */
export function convertUnits(num, from, to, decimals = 10, showUnit = false, isNum = true) {
  if (num === undefined || num === null || isNaN(+num)) return num
  if (!from || !to) return num
  const _to = to === 't' ? 'mt' : to
  const _from = from === 't' ? 'mt' : from
  num = convert(+num)
    .from(_from)
    .to(_to)
  num = num.toFixed(decimals)
  if (isNum) {
    num = +num
  }
  if (showUnit) {
    num += ` ${to}`
  }
  return num
}

/**
 * 像素（px） 转 长度单位
 * @export
 * @param {number} pixel px
 * @param {string} unit 长度单位
 * @param {number} [precision=2] 小数精度
 * @returns
 */
export function px2lengthUnit(pixel, unit, precision = 2) {
  const millimeter = px2mm(pixel, precision + 1)
  return convertUnits(millimeter, 'mm', unit, precision)
}

/**
 * 长度单位 转 像素（px）
 * @export
 * @param {number} length 长度值
 * @param {string} unit 长度单位
 * @param {number} [precision=2]
 * @returns
 */
export function lengthUnit2px(length, unit, precision = 2) {
  const millimeter = convertUnits(length, unit, 'mm', precision + 1)
  return mm2px(millimeter, precision)
}

/**
 * 像素（px）转毫米（mm）
 * @export
 * @param {*} pixel 像素（px）
 * @param {number} [precision=2] 转换后的小数精度
 * @returns
 */
export function px2mm(pixel, precision = 2) {
  const dpi = getDPI()
  const millimeter = (pixel * 25.4) / dpi[0]
  // var pixel = parseFloat(val) / 25.4 * dpi[0] // 只计算x轴的dPI
  return parseFloat(millimeter.toFixed(precision))
}

/**
 * 毫米（mm）转像素（px）
 * @export
 * @param {*} millimeter 毫米
 * @param {number} [precision=2] 转换后的小数精度
 * @returns
 */
export function mm2px(millimeter, precision = 2) {
  var dpi = getDPI()
  var pixel = (parseFloat(millimeter) / 25.4) * dpi[0] // 只计算x轴的dPI
  return parseFloat(pixel.toFixed(precision))
}

/**
 * 点（pt）转 像素（px）
 * @export
 * @param {*} point 点（pt）
 * @param {number} [precision=2] 转换后的小数精度
 * @returns
 */
export function pt2px(point, precision = 2) {
  var dpi = getDPI()
  var pixel = (parseFloat(point) / 96) * dpi[0] // 只计算x轴的dPI
  return parseFloat(pixel.toFixed(precision))
}

/**
 * 像素（px）转 点（pt）
 * @export
 * @param {*} point 点（pt）
 * @param {number} [precision=2] 转换后的小数精度
 * @returns
 */
export function px2pt(pixel, precision = 2) {
  var dpi = getDPI()
  var point = (parseFloat(pixel) * 96) / dpi[0] // 只计算x轴的dPI
  return parseFloat(point.toFixed(precision))
}

/**
 * 获取当前设备的dpi
 * @returns
 */
function getDPI() {
  var arrDPI = []
  if (window.screen.deviceXDPI !== undefined) {
    // ie 9
    arrDPI[0] = window.screen.deviceXDPI
    arrDPI[1] = window.screen.deviceYDPI
  } else {
    // chrome firefox
    var tmpNode = document.createElement('DIV')
    tmpNode.style.cssText = 'width:1in;height:1in;position:absolute;left:0px;top:0px;z-index:99;visibility:hidden'
    document.body.appendChild(tmpNode)
    arrDPI[0] = parseInt(tmpNode.offsetWidth)
    arrDPI[1] = parseInt(tmpNode.offsetHeight)
    tmpNode.parentNode.removeChild(tmpNode)
  }
  return arrDPI
}
