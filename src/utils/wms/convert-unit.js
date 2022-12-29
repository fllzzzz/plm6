import { MIN_UNIT, STEEL_ENUM, UNIT_NET_PRECISION } from '@/settings/config'
import store from '@/store'
import { codeWait } from '..'
import { convertUnits } from '../convert/unit'
import { deepClone, isBlank, isNotBlank, toFixed, toPrecision } from '../data-type'
import { unitTypeEnum } from '../enum/modules/common'
import { matClsEnum } from '@enum-ms/classification'
import { patternNumerical } from '../validate/pattern'
import { getFullNum } from '../data-type/number'

async function getBaseUnit() {
  const _unit = store.getters.baseUnit
  if (isNotBlank(_unit)) {
    return _unit
  }
  await store.dispatch('wms/fetchWmsConfig') // 目前该信息未走接口，此处无用
  return store.getters.baseUnit
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
 * TODO: 优化
 * 数值显示根据单位转换
 * @param {object} data | required 需要转换的对象
 * @param {number} basicClass | required 基础类型
 * @param {array} length 长度字段数组
 * @param {array} width 宽度字段数组
 * @param {array} weight 重量字段数组
 * @param {array} thickness 厚度字段数组
 * @param {string} prefix 对象前缀 如 list:[ {material:{}}] 实际要转换的是material,则prefix为material, 不支持多层嵌套
 * @param {boolean} newObj | default:false 是否返回新对象
 * @param {boolean} toNum | default:false 是否转为数字
 * @param {boolean} showUnit | default:false 是否显示单位
 * @param {boolean} toSmallest | default:false 是否转为最小单位
 */
export async function numFmtByBasicClass(
  data,
  {
    basicClass,
    measureUnit,
    accountingUnit,
    accountingPrecision,
    measurePrecision,
    prefix,
    newObj = false,
    toNum = false,
    showUnit = false,
    toSmallest = false
  } = {},
  fieldsConfig
) {
  // 获取单位配置
  const baseUnitCfg = await getBaseUnit()
  const unitCfg = await getUnit()
  let _basicClass = basicClass
  const format = (row) => {
    if (isBlank(row)) {
      return
    }
    if (!basicClass) _basicClass = row.basicClass
    const _d = newObj ? deepClone(row) : row
    if (_basicClass <= STEEL_ENUM) {
      // 长宽厚换算
      steelFormat(_d, baseUnitCfg, { basicClass: _basicClass, toNum, showUnit, toSmallest }, fieldsConfig)
    }

    // 金额及量换算
    otherRawMatFormat(
      _d,
      unitCfg,
      {
        basicClass: _basicClass,
        measureUnit: isNotBlank(measureUnit) ? measureUnit : _d.measureUnit,
        accountingUnit: isNotBlank(accountingUnit) ? accountingUnit : _d.accountingUnit,
        accountingPrecision: isNotBlank(accountingPrecision) ? accountingPrecision : _d.accountingPrecision,
        measurePrecision: isNotBlank(measurePrecision) ? measurePrecision : _d.measurePrecision,
        toNum,
        showUnit,
        toSmallest
      },
      fieldsConfig
    )
    return _d
  }
  if (Array.isArray(data)) {
    return data.map((row) => {
      return format(prefix ? row[prefix] : row)
    })
  } else {
    return format(prefix ? data[prefix] : data)
  }
}

// 钢材转换
function steelFormat(
  data,
  unitCfg,
  { basicClass, toNum = false, showUnit = false, toSmallest = false } = {},
  { length = ['length', 'totalLength'], width = ['width'], weight = ['weight', 'totalWeight'], thickness = ['thickness'] } = {}
) {
  if (!basicClass || !unitCfg[basicClass]) {
    return data
  }
  // 钢卷需要转换quantity【按长度】
  if (basicClass & matClsEnum.STEEL_COIL.V) {
    length.push('quantity')
  }
  // 长
  if (length && length instanceof Array) {
    const curUnit = toSmallest ? unitCfg[basicClass].length.unit : MIN_UNIT.LENGTH // 当前单位
    const fmtUnit = toSmallest ? MIN_UNIT.LENGTH : unitCfg[basicClass].length.unit // 转换单位
    const precision = toSmallest ? MIN_UNIT.LENGTH_DP : unitCfg[basicClass].length.precision // 小数精度
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
    const precision = toSmallest ? MIN_UNIT.LENGTH_DP : unitCfg[basicClass].length.precision
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
    const precision = toSmallest ? MIN_UNIT.WEIGHT_DP : unitCfg[basicClass].weight.precision
    weight.forEach((wt) => {
      if (patternNumerical.test(data[wt])) {
        data[wt] = convertUnits(data[wt], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  }
  // 厚
  if (thickness && thickness instanceof Array) {
    if (unitCfg[basicClass].thickness) {
      // 型材没有厚度
      const curUnit = toSmallest ? unitCfg[basicClass].thickness.unit : MIN_UNIT.THICKNESS
      const fmtUnit = toSmallest ? MIN_UNIT.THICKNESS : unitCfg[basicClass].thickness.unit
      const precision = toSmallest ? MIN_UNIT.THICKNESS_DP : unitCfg[basicClass].thickness.precision
      thickness.forEach((tn) => {
        if (patternNumerical.test(data[tn])) {
          data[tn] = convertUnits(data[tn], curUnit, fmtUnit, precision, { showUnit, toNum })
        }
      })
    }
  }
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
  {
    basicClass,
    measureUnit,
    measurePrecision,
    accountingUnit,
    accountingPrecision,
    toNum = false,
    showUnit = false,
    toSmallest = false
  } = {},
  {
    unitNetCalcMete = 'mete',
    unitNetCalcQuantity = 'quantity',
    mete = ['mete', 'frozenMete'],
    quantity = ['quantity', 'frozenQuantity'],
    unitNet = 'unitNet',
    accountingUnitNet = 'accountingUnitNet',
    amount = ['unitPrice', 'unitPriceExcludingVAT']
  } = {}
) {
  // 计量
  const _measureUnit = unitCfg.get(measureUnit)
  // 核算
  const _accountingUnit = unitCfg.get(accountingUnit)

  if (isNotBlank(measureUnit) && isBlank(_measureUnit)) console.error(`“${measureUnit}”:无法从当前系统获取当该单位配置`)
  if (isNotBlank(accountingUnit) && isBlank(_accountingUnit)) console.error(`“${accountingUnit}”:无法从当前系统获取当该单位配置`)

  // 计算钢材的单位净量
  if (isBlank(data['unitNet'])) {
    data['unitNet'] = toFixed(data[unitNetCalcMete] / data[unitNetCalcQuantity], UNIT_NET_PRECISION)
  }

  // 数量
  if (isNotBlank(_measureUnit) && isNotBlank(quantity)) {
    fieldsFormat({
      data,
      fields: quantity,
      symbol: _measureUnit.symbol,
      unitPrecision: measurePrecision,
      type: _measureUnit.type,
      toSmallest,
      showUnit,
      toNum
    })
  }
  if (isNotBlank(_accountingUnit)) {
    // 核算量
    if (isNotBlank(mete)) {
      fieldsFormat({
        data,
        fields: mete,
        symbol: _accountingUnit.symbol,
        unitPrecision: accountingPrecision,
        type: _accountingUnit.type,
        toSmallest,
        showUnit,
        toNum
      })
    }

    // 金额 金额的大小转换与量相反
    if (isNotBlank(amount)) {
      fieldsFormat({
        data,
        fields: amount,
        symbol: _accountingUnit.symbol,
        // unitPrecision: !toSmallest ? 8 : 2,
        unitPrecision: 10,
        type: _accountingUnit.type,
        toSmallest: !toSmallest,
        boolPrice: true,
        showUnit,
        toNum
      })
    }

    // 辅材、气体等（除钢材）单位净量转换
    // 服务端unitNet是根据最小单位计算的，所以此处需要转换（服务端unitNet: 入库时：核算量/计量量）
    // if (!(basicClass & STEEL_ENUM)) {
    unitNetFormat({
      data,
      unitNet,
      accountingUnitNet,
      measureUnit: _measureUnit,
      accountingUnit: _accountingUnit,
      measurePrecision,
      accountingPrecision,
      toSmallest,
      showUnit,
      toNum
    })
    // }
  }
}

// 辅材、气体等（除钢材）单位净量转换
function unitNetFormat({
  data,
  unitNet = 'unitNet',
  accountingUnitNet = 'accountingUnitNet',
  measureUnit,
  accountingUnit,
  measurePrecision,
  accountingPrecision,
  toSmallest,
  showUnit,
  toNum
}) {
  if (isBlank(unitNet)) return
  if (isNotBlank(measureUnit)) {
    // 计量与最小单位的比例
    const measureMinUnit = getUnitType(measureUnit.type)
    const measureScale = convertUnits(1, measureUnit.symbol, MIN_UNIT[measureMinUnit], MIN_UNIT[`${measureMinUnit}_DP`], {
      showUnit,
      toNum
    })
    // 核算与最小单位的比例
    const accountingMinUnit = getUnitType(accountingUnit.type)
    const accountingScale = convertUnits(1, accountingUnit.symbol, MIN_UNIT[accountingMinUnit], MIN_UNIT[`${accountingMinUnit}_DP`], {
      showUnit,
      toNum
    })

    const ratio = accountingScale / measureScale
    if (patternNumerical.test(data[unitNet])) {
      const un = toSmallest ? data[unitNet] * ratio : data[unitNet] / ratio
      data[unitNet] = toPrecision(un, UNIT_NET_PRECISION)
      data[accountingUnitNet] = toPrecision(1 / un, UNIT_NET_PRECISION)
    }
  } else {
    // 计量单位为空，单位净量为1
    data[unitNet] = 1
    data[accountingUnitNet] = 1
  }
}

// 转换
function fieldsFormat({ data, fields, symbol, unitPrecision, type, toSmallest, showUnit, toNum, boolPrice = false }) {
  // 获取单位类型
  const UT = getUnitType(type)
  if (type && symbol) {
    const curUnit = toSmallest ? symbol : MIN_UNIT[UT] // 当前单位
    const fmtUnit = toSmallest ? MIN_UNIT[UT] : symbol // 转换单位
    const precision = toSmallest && !boolPrice ? MIN_UNIT[`${UT}_DP`] : unitPrecision // 小数精度
    fields.forEach((field) => {
      // 科学计数法转为普通数值
      data[field] = getFullNum(data[field])
      if (patternNumerical.test(data[field])) {
        data[field] = convertUnits(data[field], curUnit, fmtUnit, precision, { showUnit, toNum })
      }
    })
  } else {
    fields.forEach((field) => {
      // 科学计数法转为普通数值
      data[field] = getFullNum(data[field])
      if (patternNumerical.test(data[field])) {
        data[field] = toFixed(data[field], unitPrecision, { toNum: toNum })
      }
    })
  }
}

// 根据单位进行数据转换 列表 转换
// 若行字段有差异（如unit与unitPrecision）,可修改传入对象，增加字段参数
export async function numFmtByUnitForList(
  list = [],
  { fields, unitField = 'unit', unitPrecisionField = 'unitPrecision', toSmallest = false, showUnit = false, toNum = false } = {}
) {
  const unitCfg = await getUnit()
  list.forEach((row) =>
    numFmtBySysUnit(row, { unit: unitCfg.get(row[unitField]), precision: row[unitPrecisionField], fields, toSmallest, showUnit, toNum })
  )
}

// 根据单位进行数据转换 单条数据 转换
export async function numFmtByUnit(data, { unit, precision = 0, fields, toSmallest = false, showUnit = false, toNum = false } = {}) {
  const unitCfg = await getUnit()
  numFmtBySysUnit(data, { unit: unitCfg.get(unit), precision, fields, toSmallest, showUnit, toNum })
}

// 根据单位装换
// unit : 转换后的unit
export function numFmtBySysUnit(data, { unit, precision = 0, fields, toSmallest = false, showUnit = false, toNum = false } = {}) {
  if (isBlank(unit) || isBlank(fields)) return
  fieldsFormat({
    data,
    fields: fields,
    symbol: unit.symbol,
    unitPrecision: precision,
    type: unit.type,
    toSmallest,
    showUnit,
    toNum
  })
}

// 获取单位类型
function getUnitType(type) {
  // 计数单位不参与计算
  switch (type) {
    case unitTypeEnum.WEIGHT.V:
      return 'WEIGHT'
    case unitTypeEnum.LENGTH.V:
      return 'LENGTH'
    case unitTypeEnum.AREA.V:
      return 'AREA'
    case unitTypeEnum.VOLUME.V:
      return 'VOLUME'
  }
}
