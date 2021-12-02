import { MIN_UNIT, STEEL_ENUM } from '@/settings/config'
import store from '@/store'
import { convertUnits } from '../convert/unit'
import { deepClone, isBlank, isNotBlank, toFixed } from '../data-type'
import { matClsEnum } from '../enum/modules/classification'
import { unitTypeEnum } from '../enum/modules/common'
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

// 获取单位
async function getUnit() {
  const _unit = store.state.config.loaded.unit
  if (isNotBlank(_unit)) {
    return _unit
  } else {
    await store.dispatch('wms/fetchUnit')
  }
  return store.state.config.unit.MAP
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
 * @param {boolean} toNum | default:false 是否转为数字
 * @param {boolean} showUnit | default:false 是否显示单位
 * @param {boolean} toSmallest | default:false 是否转为最小单位
 */
export async function numFmtByBasicClass(
  data,
  { basicClass, measureUnit, accountingUnit, accountingPrecision, measurePrecision, newObj = false, toNum = false, showUnit = false, toSmallest = false } = {},
  fieldsConfig
) {
  // 获取单位配置
  const baseUnitCfg = await getBaseUnit()
  const unitCfg = await getUnit()
  const format = (row) => {
    if (isBlank(row)) {
      return
    }
    if (!basicClass) basicClass = row.basicClass
    const _d = newObj ? deepClone(row) : row
    if (basicClass < STEEL_ENUM) {
      steelFormat(_d, baseUnitCfg, { basicClass, toNum, showUnit, toSmallest }, fieldsConfig)
    } else {
      measureUnit = measureUnit || data.measureUnit
      accountingUnit = accountingUnit || data.accountingUnit
      accountingPrecision = accountingPrecision || data.accountingPrecision
      measurePrecision = measurePrecision || data.measurePrecision
      otherRawMatFormat(_d, unitCfg, { measureUnit, accountingUnit, accountingPrecision, measurePrecision, toNum, showUnit, toSmallest }, fieldsConfig)
    }
    return _d
  }
  if (Array.isArray(data)) {
    return data.map((row) => {
      return format(row)
    })
  } else {
    return format(data)
  }
}

// 钢板转换
function steelFormat(
  data,
  unitCfg,
  { basicClass, toNum = false, showUnit = false, toSmallest = false } = {},
  {
    length = ['length', 'totalLength'],
    width = ['width'],
    weight = ['mete', 'weight', 'totalWeight'],
    thickness = ['thickness'],
    amount = ['unitPrice'],
    unit = ['accountingUnit']
  } = {}
) {
  if (!basicClass || !unitCfg[basicClass]) {
    return data
  }
  // 长
  if (length && length instanceof Array) {
    const curUnit = toSmallest ? unitCfg[basicClass].length.unit : MIN_UNIT.LENGTH // 当前单位
    const fmtUnit = toSmallest ? MIN_UNIT.LENGTH : unitCfg[basicClass].length.unit // 转换单位
    const precision = toSmallest ? MIN_UNIT.LENGTHdataP : unitCfg[basicClass].length.precision // 小数精度
    length.forEach((len) => {
      if (patternNumerical.test(data[len])) {
        data[len] = convertUnits(data[len], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  }
  // 宽
  if (width && width instanceof Array) {
    const curUnit = toSmallest ? unitCfg[basicClass].length.unit : MIN_UNIT.LENGTH
    const fmtUnit = toSmallest ? MIN_UNIT.LENGTH : unitCfg[basicClass].length.unit
    const precision = toSmallest ? MIN_UNIT.LENGTHdataP : unitCfg[basicClass].length.precision
    width.forEach((wd) => {
      if (patternNumerical.test(data[wd])) {
        data[wd] = convertUnits(data[wd], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  }
  // 重
  if (weight && weight instanceof Array) {
    const curUnit = toSmallest ? unitCfg[basicClass].weight.unit : MIN_UNIT.WEIGHT
    const fmtUnit = toSmallest ? MIN_UNIT.WEIGHT : unitCfg[basicClass].weight.unit
    const precision = toSmallest ? MIN_UNIT.WEIGHTdataP : unitCfg[basicClass].weight.precision
    weight.forEach((wt) => {
      if (patternNumerical.test(data[wt])) {
        data[wt] = convertUnits(data[wt], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  }
  // 厚
  if (thickness && thickness instanceof Array) {
    if (unitCfg[basicClass].thickness) { // 型钢没有厚度
      const curUnit = toSmallest ? unitCfg[basicClass].thickness.unit : MIN_UNIT.THICKNESS
      const fmtUnit = toSmallest ? MIN_UNIT.THICKNESS : unitCfg[basicClass].thickness.unit
      const precision = toSmallest ? MIN_UNIT.THICKNESSdataP : unitCfg[basicClass].thickness.precision
      thickness.forEach((tn) => {
        if (patternNumerical.test(data[tn])) {
          data[tn] = convertUnits(data[tn], curUnit, fmtUnit, precision, { showUnit, toNum })
        }
      })
    }
  }
  // 金额
  if (amount && amount instanceof Array) {
    let curUnit
    let fmtUnit
    const precision = 2
    if (basicClass & STEEL_ENUM) {
      // 元/g
      curUnit = toSmallest ? unitCfg[basicClass].weight.unit : MIN_UNIT.WEIGHT
      fmtUnit = toSmallest ? MIN_UNIT.WEIGHT : unitCfg[basicClass].weight.unit
    }
    if (basicClass & matClsEnum.ENCL_MANUFACTURED.V) {
      // 元/mm
      curUnit = toSmallest ? unitCfg[basicClass].length.unit : MIN_UNIT.LENGTH
      fmtUnit = toSmallest ? MIN_UNIT.LENGTH : unitCfg[basicClass].length.unit
    }
    amount.forEach((at) => {
      if (patternNumerical.test(data[at])) {
        if (!(basicClass & matClsEnum.MATERIAL.V)) {
          data[at] = convertUnits(data[at], fmtUnit, curUnit, precision, { showUnit, toNum })
        } else {
          data[at] = data[at].toFixed(precision)
        }
      }
    })
  }
  // // 核算单位
  // if (unit && unit instanceof Array) {
  //   let fmtUnit
  //   if (basicClass & STEEL_ENUM) {
  //     // 重量单位
  //     fmtUnit = unitCfg[basicClass].weight.unit
  //   }
  //   if (basicClass & matClsEnum.ENCL_MANUFACTURED.V) {
  //     // 长度单位
  //     fmtUnit = unitCfg[basicClass].length.unit
  //   }
  //   unit.forEach((ut) => {
  //     if (!(basicClass & matClsEnum.MATERIAL.V)) {
  //       data[ut] = fmtUnit
  //     }
  //   })
  // }
}

/**
 * 辅材及气体等最小单位计算
 * @param {*} measureUnit // 计量单位
 * @param {*} accountingUnit // 核算单位
 * @param {*} accountingPrecision // 核算单位小数精度
 * @param {*} measurePrecision // 计量单位小数精度
 */
function otherRawMatFormat(
  data,
  unitCfg,
  { measureUnit, accountingUnit, accountingPrecision, measurePrecision, toNum = false, showUnit = false, toSmallest = false } = {},
  {
    mete = ['mete'],
    quantity = ['quantity'],
    amount = ['unitPrice']
  } = {}
) {
  // 计量
  const _measureUnit = unitCfg.get(measureUnit)
  if (!_measureUnit) return
  // 核算
  const _accountingUnit = unitCfg.get(accountingUnit)
  if (!_accountingUnit) return

  // 数量
  if (isNotBlank(quantity)) {
    fieldsFormat({
      data,
      fields: quantity,
      symbol: _measureUnit.symbol,
      unitPrecision: measurePrecision,
      type: _measureUnit.type,
      toSmallest, showUnit, toNum
    })
  }

  // 核算量
  if (isNotBlank(mete)) {
    fieldsFormat({
      data,
      fields: mete,
      symbol: _accountingUnit.symbol,
      unitPrecision: accountingPrecision,
      type: _accountingUnit.type,
      toSmallest, showUnit, toNum
    })
  }

  // 金额
  if (isNotBlank(amount)) {
    fieldsFormat({
      data,
      fields: amount,
      symbol: _accountingUnit.symbol,
      unitPrecision: accountingPrecision,
      type: _accountingUnit.type,
      toSmallest, showUnit, toNum
    })
  }
}

// 转换
function fieldsFormat({
  data, fields, symbol, unitPrecision, type, toSmallest, showUnit, toNum
}) {
  // 获取单位类型
  const UT = getUnitType(type)
  if (type && symbol) {
    const curUnit = toSmallest ? symbol : MIN_UNIT[UT] // 当前单位
    const fmtUnit = toSmallest ? MIN_UNIT[UT] : symbol // 转换单位
    const precision = toSmallest ? unitPrecision : MIN_UNIT[`${UT}_DP`] // 小数精度
    fields.forEach((field) => {
      if (patternNumerical.test(data[field])) {
        data[field] = convertUnits(data[field], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  } else {
    fields.forEach((field) => {
      if (patternNumerical.test(data[field])) {
        data[field] = toFixed(data[field], unitPrecision, { toNum: toNum })
      }
    })
  }
}

// 获取单位类型
function getUnitType(type) { // 计数单位不参与计算
  switch (type) {
    case unitTypeEnum.WEIGHT.V: return 'WEIGHT'
    case unitTypeEnum.LENGTH.V: return 'LENGTH'
    case unitTypeEnum.AREA.V: return 'AREA'
    case unitTypeEnum.VOLUME.V: return 'VOLUME'
  }
}
