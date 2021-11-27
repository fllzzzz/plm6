import { STEEL_DENSITY, STAINLESS_STEEL_DENSITY } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

// TODO: 重写
/**
 * 钢板计算重量的公式
 * @param {String} name 二级名称
 * @param {Number} length 长
 * @param {Number} width 宽
 * @param {Number} thickness 厚
 * @param {Number} number 数量
 * @param {Number} lengthUnit 长度单位
 * @param {Number} weightToUnit 重量转化至什么单位
 * @param {Number} precision 保留小数位
 * @returns {Number | null}
 */
export function calcSteelPlateWeight({ name, length, width, thickness, number = 1, lengthUnit = 'm', weightUnit = 'kg', precision = 3 }) {
  if (!name || !length || !width || !thickness) {
    return null
  }
  let density = STEEL_DENSITY // 密度 t/m³
  if (name && name.indexOf('不锈钢') > -1) {
    density = STAINLESS_STEEL_DENSITY
  }

  let theoryWeight
  const len = convertUnits(length, lengthUnit, 'm')
  const wth = convertUnits(width, lengthUnit, 'm')
  // 长宽以m为基础单位，换算出来的重量为kg
  theoryWeight = len * wth * thickness * density
  theoryWeight = convertUnits(theoryWeight, 'kg', weightUnit, precision)
  return Number((theoryWeight * number).toFixed(precision))
}

/**
 * 型钢总长度计算
 * @param {Number} length 定尺长度
 * @param {Number} number 数量
 * @returns {Number | null}
 */
export function calcSectionSteelTotalLength({ length = 0, lengthUnit = 'mm', number = 1, precision = 3 }) {
  const totalLength = (length * number).toFixed(precision)
  return convertUnits(totalLength, lengthUnit, 'm', precision)
}

/**
 * 型钢计算重量的公式
 * @param {Number} length 长
 * @param {Number} number 数量
 * @param {Number} specWeight 规格理论重量
 * @param {Number} lengthUnit 长度单位
 * @param {Number} weightToUnit 重量转化至什么单位
 * @param {Number} precision 保留小数位
 * @returns {Number | null}
 */
export function calcSectionSteelWeight({ length, number = 1, unitWeight, lengthUnit, weightUnit, weightResUnit, precision = 3 }) {
  if (!length || !number || !unitWeight) {
    return null
  }
  let theoryWeight
  const _length = convertUnits(length, lengthUnit, 'm', 3)
  // 长宽以m为基础单位，换算出来的重量为kg
  theoryWeight = _length * unitWeight
  theoryWeight = convertUnits(theoryWeight, weightUnit, weightResUnit, precision)
  return Number((theoryWeight * number).toFixed(precision))
}

/**
 * 钢卷计算重量的公式
 * @param {Number} weight 重量
 * @param {Number} width 宽度
 * @param {Number} thickness 厚
 * @param {Number} number 数量
 * @param {Number} weightUnit 重量单位
 * @param {Number} lengthUnit 长度单位
 * @param {Number} precision 保留小数位
 * @returns {Number | null}
 */
export function calcSteelCoilLength({ weight, width, thickness, number = 1, weightUnit, weightPrecision, lengthUnit, lengthPrecision = 3 }) {
  if (!weight || !number || !width || !thickness) {
    return null
  }
  let theoryLength
  const weg = convertUnits(weight, weightUnit, 'kg', weightPrecision)
  const wth = convertUnits(width, lengthUnit, 'm', 3)
  // 计算结果为 m
  theoryLength = weg / (wth * thickness * STEEL_DENSITY)
  theoryLength = convertUnits(theoryLength, 'm', lengthUnit)
  return Number((theoryLength * number).toFixed(lengthPrecision))
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

