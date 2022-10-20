/**
 * 单位转换工具类
 */
import convert from 'convert-units'
import store from '@/store'
import { codeWait } from '..'

/**
 * 单位转换
 * @param {number} num 数值
 * @param {string} from 转换前单位
 * @param {string} to 转换后单位
 * @param {number} precision 小数
 * @param {boolean} showUnit 是否显示单位
 * @param {boolean} toNum 是否转为数字
 */
export function convertUnits(num, from, to, precision = 10, { showUnit = false, toNum = true } = {}) {
  if (num === undefined || num === null || isNaN(+num)) return num
  if (!from || !to) return num
  const _to = getUsableUnit(to)
  const _from = getUsableUnit(from)
  num = convert(+num)
    .from(_from)
    .to(_to)
  num = num.toFixed(precision)
  if (toNum) {
    num = +num
  }
  if (showUnit) {
    num += ` ${to}`
  }
  return num
}

// 获取可用来转换的单位
export function getUsableUnit(unit) {
  switch (unit) {
    case 'L': return 'l'
    case 'dL': return 'dl'
    case 'cL': return 'cl'
    case '㎏': return 'kg'
    case '㎝': return 'cm'
    case '㎜': return 'mm'
    case 't' : return 'mt' // 国内的t等于国外的mt
    case 'mm²' : return 'mm2'
    case 'cm²' : return 'cm2'
    case '㎡' :
    case 'm²' : return 'm2'
    case 'km²' : return 'km2'
    case 'sq.in' : return 'in2'
    case 'sq.ft' : return 'ft2'
    case 'sq.mi' : return 'mi2'
    case 'mm³' : return 'mm3'
    case 'cm³' : return 'cm3'
    case 'm³' : return 'm3'
    case 'km³' : return 'km3'
    case 'cu in' : return 'in3'
    case 'cu ft' : return 'ft3'
    case 'tcu yd' : return 'yd3'
    default: return unit
  }
}

// convert.js 支持单位
// use one of: mm, cm, m, km, in, yd, ft-us, ft, mi, mm2, cm2, m2, ha, km2, in2, yd2, ft2, ac, mi2, mcg, mg, g, kg,
//  mt, oz, lb, t, mm3, cm3, ml, cl, dl, l, kl, m3, km3, krm, tsk, msk, kkp, glas, kanna, tsp, Tbs, in3, fl-oz, cup,
//  pnt, qt, gal, ft3, yd3, ea, dz, C, K, F, R, ns, mu, ms, s, min, h, d, week, month, year, b, Kb, Mb, Gb, Tb, B, KB,
//  MB, GB, TB, ppm, ppb, ppt, ppq, m/s, km/h, m/h, knot, ft/s, min/km, s/m, min/mi, s/ft, Pa, kPa, MPa, hPa, bar, torr,
//  psi, ksi, A, mA, kA, V, mV, kV, W, mW, kW, MW, GW, VAR, mVAR, kVAR, MVAR, GVAR, VA, mVA, kVA, MVA, GVA, Wh, mWh, kWh,
//  MWh, GWh, J, kJ, VARh, mVARh, kVARh, MVARh, GVARh, mm3/s, cm3/s, ml/s, cl/s, dl/s, l/s, l/min, l/h, kl/s, kl/min, kl/h,
//  m3/s, m3/min, m3/h, km3/s, tsp/s, Tbs/s, in3/s, in3/min, in3/h, fl-oz/s, fl-oz/min, fl-oz/h, cup/s, pnt/s, pnt/min, pnt/h,
//  qt/s, gal/s, gal/min, gal/h, ft3/s, ft3/min, ft3/h, yd3/s, yd3/min, yd3/h, lx, ft-cd, mHz, Hz, kHz, MHz, GHz, THz, rpm, deg/s,
//  rad/s, rad, deg, grad, arcmin, arcsec

// 判断单位是否存在
export async function judgeMaterialUnitExist(unit, checkUnit) {
  const unitCfg = await getUnit()
  const u = unitCfg.get(unit)
  // 单位名称与符号与传入均不相同时
  if (!u || (checkUnit !== u.name && checkUnit !== u.symbol)) {
    return false
  }
  return true
}

// 获取单位,避免同时调用 例：库存预警页面与库存预警铃铛
export async function getUnit(pollingNumber = 1) {
  try {
    const _unit = store.state.config.loaded.unit
    if (_unit) {
      return store.state.config.unit.MAP
    }
    await store.dispatch('config/fetchUnit')
    return store.state.config.unit.MAP
  } catch (error) {
    if (pollingNumber && pollingNumber <= 10) {
      await codeWait(1000)
      return await getUnit(++pollingNumber)
    } else {
      throw new Error('加载单位失败')
    }
  }
}

/**
 * px转emu
 * @param {*} val
 * @param {*} scale
 * @returns
 */
export function px2emu(val, scale) {
  const mmVal = px2mm(val, 10)
  // 1 mm = 36000 EMUs
  return Math.floor(mmVal * 36000 * (scale || 1))
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
