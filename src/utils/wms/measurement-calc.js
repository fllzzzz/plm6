import { STEEL_DENSITY, STAINLESS_STEEL_DENSITY } from '@/settings/config'

import { matClsEnum } from '../enum/modules/classification'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { convertUnits } from '@/utils/convert/unit'
import store from '@/store'
import { isNotBlank, toPrecision } from '../data-type'
import { materialIsWholeEnum } from '../enum/modules/wms'

const STEEL_PLATE = matClsEnum.STEEL_PLATE.V
const SECTION_STEEL = matClsEnum.SECTION_STEEL.V
const STEEL_COIL = matClsEnum.STEEL_COIL.V

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
 * 钢板计算重量的公式
 * @param {String} name 二级名称
 * @param {Number} length 长
 * @param {Number} width 宽
 * @param {Number} thickness 厚
 * @param {Number} quantity 数量
 * @param {Number} lengthUnit 长度单位
 * @param {Number} weightToUnit 重量转化至什么单位
 * @param {Number} precision 保留小数位
 */
export async function calcSteelPlateWeight({ name, length, width, thickness, quantity = 1 }, boolPrecise = false) {
  if (!name || !length || !width || !thickness) {
    return null
  }

  const baseUnit = await getBaseUnit()
  const lengthUnit = baseUnit[STEEL_PLATE].length.unit
  const weightUnit = baseUnit[STEEL_PLATE].weight.unit
  const dp = boolPrecise ? baseUnit[STEEL_PLATE].weight.precision : 2

  let density = STEEL_DENSITY // 密度 t/m³
  if (name && name.indexOf('不锈钢') > -1) {
    density = STAINLESS_STEEL_DENSITY
  }

  let theoryWeight
  const len = convertUnits(length, lengthUnit, 'm')
  const wth = convertUnits(width, lengthUnit, 'm')
  // 长宽以m为基础单位，换算出来的重量为kg
  theoryWeight = len * wth * thickness * density
  theoryWeight = convertUnits(theoryWeight, 'kg', weightUnit, dp)
  return toPrecision(theoryWeight * quantity, dp)
}

/**
 * 型材总长度计算
 * @param {Number} length 定尺长度
 * @param {Number} quantity 数量
 * @returns {Number | null}
 */
export function calcSectionSteelTotalLength({ length = 0, lengthUnit = 'mm', quantity = 1, precision = 3 }, boolPrecise = true) {
  const dp = boolPrecise ? precision : 10
  const totalLength = (length * quantity).toFixed(dp)
  return convertUnits(totalLength, lengthUnit, 'm', dp)
}

/**
 * 型材计算重量的公式
 * @param {Number} length 长
 * @param {Number} quantity 数量
 * @param {Number} specWeight 规格理论重量
 * @param {Number} lengthUnit 长度单位
 * @param {Number} weightToUnit 重量转化至什么单位
 * @param {Number} precision 保留小数位
 */
export async function calcSectionSteelWeight({ length, quantity = 1, unitWeight }, boolPrecise = false) {
  if (!length || !quantity || !unitWeight) {
    return null
  }
  const baseUnit = await getBaseUnit()
  const lengthUnit = baseUnit[SECTION_STEEL].length.unit
  const weightUnit = baseUnit[SECTION_STEEL].weight.unit
  const dp = boolPrecise ? baseUnit[SECTION_STEEL].weight.precision : 2

  let theoryWeight
  const _length = convertUnits(length, lengthUnit, 'm', 3)
  // 长宽以m为基础单位，换算出来的重量为kg
  theoryWeight = _length * unitWeight
  theoryWeight = convertUnits(theoryWeight, weightUnit, 'kg', dp)
  return toPrecision(theoryWeight * quantity, dp)
}

/**
 * 钢卷计算长度的公式
 * @param {Number} weight 重量
 * @param {Number} width 宽度
 * @param {Number} thickness 厚
 * @param {Number} quantity 数量
 * @param {Number} weightUnit 重量单位
 * @param {Number} lengthUnit 长度单位
 * @param {Number} precision 保留小数位
 */
export async function calcSteelCoilLength({ name, weight, width, thickness, quantity = 1 }) {
  if (!weight || !quantity || !width || !thickness) {
    return null
  }

  let density = STEEL_DENSITY // 密度 t/m³
  if (name && name.indexOf('不锈钢') > -1) {
    density = STAINLESS_STEEL_DENSITY
  }

  const baseUnit = await getBaseUnit()
  const widthUnit = baseUnit[STEEL_COIL].width.unit
  const lengthUnit = baseUnit[STEEL_COIL].length.unit
  const lengthPrecision = baseUnit[STEEL_COIL].length.precision
  const weightUnit = baseUnit[STEEL_COIL].weight.unit
  const weightPrecision = baseUnit[STEEL_COIL].weight.precision

  let theoryLength
  const weg = convertUnits(weight, weightUnit, 'kg', weightPrecision)
  const wth = convertUnits(width, widthUnit, 'm', 3)
  // 计算结果为 m
  theoryLength = weg / (wth * thickness * density)
  theoryLength = convertUnits(theoryLength, 'm', lengthUnit)
  return Number((theoryLength * quantity).toFixed(lengthPrecision))
}

/**
 * 钢卷计算重量的公式
 * @param {Number} length 长度
 * @param {Number} width 宽度
 * @param {Number} thickness 厚
 * @param {Number} quantity 数量
 * @param {Number} weightUnit 重量单位
 * @param {Number} lengthUnit 长度单位
 * @param {Number} precision 保留小数位
 */
export async function calcSteelCoilWeight({ name, length, width, thickness, quantity = 1 }) {
  if (!length || !quantity || !width || !thickness) {
    return null
  }

  let density = STEEL_DENSITY // 密度 t/m³
  if (name && name.indexOf('不锈钢') > -1) {
    density = STAINLESS_STEEL_DENSITY
  }

  const baseUnit = await getBaseUnit()
  const widthUnit = baseUnit[STEEL_COIL].width.unit
  const lengthUnit = baseUnit[STEEL_COIL].length.unit
  // const lengthPrecision = baseUnit[STEEL_COIL].length.precision
  const weightUnit = baseUnit[STEEL_COIL].weight.unit
  const weightPrecision = baseUnit[STEEL_COIL].weight.precision

  let theoryWeight
  const len = convertUnits(length, lengthUnit, 'm')
  const wth = convertUnits(width, widthUnit, 'm')
  // 计算结果为 kg
  theoryWeight = len * wth * thickness * density * quantity
  theoryWeight = convertUnits(theoryWeight, 'kg', weightUnit, weightPrecision)
  return toPrecision(theoryWeight, weightPrecision)
}

// 钢材入库表单格式转换
export async function steelInboundFormFormat(form) {
  // 生成三种钢材的列表
  form.steelPlateList = []
  form.sectionSteelList = []
  form.steelCoilList = []
  const promiseList = []
  let p
  for (const row of form.list) {
    row.uid = row.id
    row.weighingTotalWeight = row.mete // 过磅重量
    row.specificationMap = row.specKV // 规格KV格式
    row.projectId = row.projectId || row.project?.id // 项目id
    row.warehouseId = row.warehouseId || row.warehouse?.id // 仓库id
    switch (row.basicClass) {
      case matClsEnum.STEEL_PLATE.V:
        p = calcSteelPlateWeight({
          name: row.classifyFullName,
          length: row.length,
          width: row.width,
          thickness: row.thickness
        }).then((val) => {
          row.theoryWeight = val
          row.theoryTotalWeight = row.theoryWeight * row.quantity // 理论总重量
          form.steelPlateList.push(row)
        })
        break
      case matClsEnum.SECTION_STEEL.V:
        p = calcSectionSteelWeight({
          length: row.length, // 长度
          unitWeight: row.unitWeight // 单位重量
        }).then((val) => {
          row.theoryWeight = val
          row.theoryTotalWeight = row.theoryWeight * row.quantity // 理论总重量
          row.totalLength = calcSectionSteelTotalLength({
            length: row.length, // 长度
            quantity: row.quantity // 数量
          })
          form.sectionSteelList.push(row)
        })
        break
      case matClsEnum.STEEL_COIL.V:
        p = calcSteelCoilLength({
          name: row.classifyFullName,
          weight: row.weighingTotalWeight,
          width: row.width,
          thickness: row.thickness
        }).then((val) => {
          row.theoryLength = val // 理论总长
          // row.length = row.theoryWeight // 理论总重量
          row.quantity = row.length
          form.steelCoilList.push(row)
        })
        break
    }
    promiseList.push(p)
  }
  await Promise.all(promiseList)
}

/**
 * 计算列表理论重量
 * @param {string} prefix 对象前缀 如 list:[ {material:{}}] 实际要转换的是material,则prefix为material, 不支持多层嵌套
 */
export async function calcTheoryWeight(list, { prefix } = {}) {
  const psList = []
  for (const item of list) {
    const row = prefix ? item[prefix] : item
    let ps
    if (row.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
      // 为余料的钢板，以实际重量作为理论重量
      if (row.materialIsWhole === materialIsWholeEnum.ODDMENT.V) {
        row.theoryWeight = toPrecision(row.mete / row.quantity)
        continue
      }
      ps = calcSteelPlateWeight({
        name: row.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
        length: row.length,
        width: row.width,
        thickness: row.thickness
      }).then((data) => {
        row.theoryWeight = data
      })
    }

    if (row.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      ps = calcSectionSteelWeight({
        length: row.length, // 长度
        unitWeight: row.unitWeight // 单位重量
      }).then((data) => {
        row.theoryWeight = data
      })
    }

    if (row.basicClass === rawMatClsEnum.STEEL_COIL.V) {
      ps = calcSteelCoilLength(
        {
          weight: row.mete,
          width: row.width,
          thickness: row.thickness
        },
        false
      ).then((data) => {
        row.theoryLength = data
      })
    }
    if (ps) psList.push(ps)
  }
  await Promise.all(psList)
}

// export function calcPriceShow(price, basicClass, unitShow = false) {
//   if (!price) {
//     return price
//   }
//   const unitObj = Object.assign({}, store.getters.unitObj)
//   if ([basicClassEnum.STEEL_PLATE.V, basicClassEnum.SECTION_STEEL.V, basicClassEnum.STEEL_COIL.V].includes(basicClass)) {
//     const unit = unitObj[basicClass].weightUnit
//     const priceChange = convertUnits(1, minUnit.WEIGHT, unit)
//     let afterPrice = (price / priceChange).toFixed(decimalPlace.YUAN)
//     if (unitShow) {
//       afterPrice = afterPrice + '元/' + unit
//     }
//     return afterPrice || 0
//   } else if ([basicClassEnum.ENCLOSURE.V].includes(basicClass)) {
//     const unit = unitObj[basicClass].lengthUnit
//     const priceChange = convertUnits(1, minUnit.LENGTH, unit)
//     let afterPrice = (price / priceChange).toFixed(decimalPlace.YUAN)
//     if (unitShow) {
//       afterPrice = afterPrice + '元/' + unit
//     }
//     return afterPrice || 0
//   } else {
//     let afterPrice = price.toFixed(decimalPlace.YUAN)
//     if (unitShow) {
//       afterPrice = afterPrice + '元'
//     }
//     return afterPrice || 0
//   }
// }
