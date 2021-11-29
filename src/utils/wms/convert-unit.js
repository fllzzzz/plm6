import { MIN_UNIT, STEEL_ENUM } from '@/settings/config'
import store from '@/store'
import { convertUnits } from '../convert/unit'
import { deepClone, isBlank, isNotBlank } from '../data-type'
import { matClsEnum } from '../enum/modules/classification'
import { patternNumerical } from '../validate/pattern'

async function getBaseUnit() {
  const _unit = store.getters.baseUnit
  if (isNotBlank(_unit)) {
    return _unit
  } else {
    await store.dispatch('wms/fetchWmsConfig') // 目前该信息未走接口，此处无用
  }
  return store.getters.baseUnit
}

/**
 * TODO: 优化
 * 数值显示根据单位转换
 * @param {object} data | required 需要转换的对象
 * @param {number} basicClass | required 基础类型
 * @param {array} length 长度字段数组
 * @param {array} width 宽度字段数组
 * @param {array} weight 重量字段数组
 * @param {array} thickness 厚度字段数组
 * @param {boolean} newObj | default:false 是否返回新对象
 * @param {boolean} isNum | default:false 是否转为数字
 * @param {boolean} showUnit | default:false 是否显示单位
 * @param {boolean} toSmallest | default:false 是否转为最小单位
 */
export async function numFmtByBasicClass(
  data, { basicClass, length = ['length', 'totalLength'], width = ['width'], weight = ['weight', 'totalWeight'],
    thickness = ['thickness'], amount = ['unitPrice'], unit = ['accountingUnit'],
    newObj = false, isNum = false, showUnit = false, toSmallest = false } = {}) {
  const _unitDec = await getBaseUnit()
  const format = (row) => {
    if (isBlank(row)) {
      return
    }
    if (!basicClass) basicClass = row.basicClass
    const _d = newObj ? deepClone(row) : row
    if (!basicClass || !_unitDec[basicClass]) {
      return _d
    }
    // 长
    if (length && length instanceof Array) {
      const curUnit = toSmallest ? _unitDec[basicClass].length.unit : MIN_UNIT.LENGTH // 当前单位
      const fmtUnit = toSmallest ? MIN_UNIT.LENGTH : _unitDec[basicClass].length.unit // 转换单位
      const precision = toSmallest ? MIN_UNIT.LENGTH_DP : _unitDec[basicClass].length.precision // 小数精度
      length.forEach(len => {
        if (patternNumerical.test(_d[len])) {
          _d[len] = convertUnits(_d[len], curUnit, fmtUnit, precision, { showUnit, isNum })
        }
      })
    }
    // 宽
    if (width && width instanceof Array) {
      const curUnit = toSmallest ? _unitDec[basicClass].length.unit : MIN_UNIT.LENGTH
      const fmtUnit = toSmallest ? MIN_UNIT.LENGTH : _unitDec[basicClass].length.unit
      const precision = toSmallest ? MIN_UNIT.LENGTH_DP : _unitDec[basicClass].length.precision
      width.forEach(wd => {
        if (patternNumerical.test(_d[wd])) {
          _d[wd] = convertUnits(_d[wd], curUnit, fmtUnit, precision, { showUnit, isNum })
        }
      })
    }
    // 重
    if (weight && weight instanceof Array) {
      const curUnit = toSmallest ? _unitDec[basicClass].weight.unit : MIN_UNIT.WEIGHT
      const fmtUnit = toSmallest ? MIN_UNIT.WEIGHT : _unitDec[basicClass].weight.unit
      const precision = toSmallest ? MIN_UNIT.WEIGHT_DP : _unitDec[basicClass].weight.precision
      weight.forEach(wt => {
        if (patternNumerical.test(_d[wt])) {
          _d[wt] = convertUnits(_d[wt], curUnit, fmtUnit, precision, { showUnit, isNum })
        }
      })
    }
    // 厚
    if (thickness && thickness instanceof Array) {
      const curUnit = toSmallest ? _unitDec[basicClass].thickness.unit : MIN_UNIT.THICKNESS
      const fmtUnit = toSmallest ? MIN_UNIT.THICKNESS : _unitDec[basicClass].thickness.unit
      const precision = toSmallest ? MIN_UNIT.THICKNESS_DP : _unitDec[basicClass].thickness.precision
      thickness.forEach(tn => {
        if (patternNumerical.test(_d[tn])) {
          _d[tn] = convertUnits(_d[tn], curUnit, fmtUnit, precision, { showUnit, isNum })
        }
      })
    }
    // 金额
    if (amount && amount instanceof Array) {
      let curUnit
      let fmtUnit
      const precision = 2
      if (basicClass & STEEL_ENUM) { // 元/g
        curUnit = toSmallest ? _unitDec[basicClass].weight.unit : MIN_UNIT.WEIGHT
        fmtUnit = toSmallest ? MIN_UNIT.WEIGHT : _unitDec[basicClass].weight.unit
      }
      if (basicClass & matClsEnum.ENCL_MANUFACTURED.V) { // 元/mm
        curUnit = toSmallest ? _unitDec[basicClass].length.unit : MIN_UNIT.LENGTH
        fmtUnit = toSmallest ? MIN_UNIT.LENGTH : _unitDec[basicClass].length.unit
      }
      amount.forEach(at => {
        if (patternNumerical.test(_d[at])) {
          if (!(basicClass & matClsEnum.MATERIAL.V)) {
            _d[at] = convertUnits(_d[at], fmtUnit, curUnit, precision, { showUnit, isNum })
          } else {
            _d[at] = _d[at].toFixed(precision)
          }
        }
      })
    }
    // 核算单位
    if (unit && unit instanceof Array) {
      let fmtUnit
      if (basicClass & STEEL_ENUM) { // 重量单位
        fmtUnit = _unitDec[basicClass].weight.unit
      }
      if (basicClass & matClsEnum.ENCL_MANUFACTURED.V) { // 长度单位
        fmtUnit = _unitDec[basicClass].length.unit
      }
      unit.forEach(ut => {
        if (!(basicClass & matClsEnum.MATERIAL.V)) {
          _d[ut] = fmtUnit
        }
      })
    }
    return _d
  }
  if (Array.isArray(data)) {
    return data.map(row => {
      return format(row)
    })
  } else {
    return format(data)
  }
}
