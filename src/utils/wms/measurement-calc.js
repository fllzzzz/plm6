import { STEEL_DENSITY, STAINLESS_STEEL_DENSITY } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import store from '@/store'
import { isNotBlank } from '../data-type'
import { matClsEnum } from '../enum/modules/classification'

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
export async function calcSteelPlateWeight({ name, length, width, thickness, quantity = 1 }) {
  if (!name || !length || !width || !thickness) {
    return null
  }

  const baseUnit = await getBaseUnit()
  const lengthUnit = baseUnit[STEEL_PLATE].length.unit
  const weightUnit = baseUnit[STEEL_PLATE].weight.unit
  const precision = baseUnit[STEEL_PLATE].weight.precision

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
  return Number((theoryWeight * quantity).toFixed(precision))
}

/**
 * 型钢总长度计算
 * @param {Number} length 定尺长度
 * @param {Number} quantity 数量
 * @returns {Number | null}
 */
export function calcSectionSteelTotalLength({ length = 0, lengthUnit = 'mm', quantity = 1, precision = 3 }) {
  const totalLength = (length * quantity).toFixed(precision)
  return convertUnits(totalLength, lengthUnit, 'm', precision)
}

/**
 * 型钢计算重量的公式
 * @param {Number} length 长
 * @param {Number} quantity 数量
 * @param {Number} specWeight 规格理论重量
 * @param {Number} lengthUnit 长度单位
 * @param {Number} weightToUnit 重量转化至什么单位
 * @param {Number} precision 保留小数位
 */
export async function calcSectionSteelWeight({ length, quantity = 1, unitWeight }) {
  if (!length || !quantity || !unitWeight) {
    return null
  }
  const baseUnit = await getBaseUnit()
  const lengthUnit = baseUnit[SECTION_STEEL].length.unit
  const weightUnit = baseUnit[SECTION_STEEL].weight.unit
  const precision = baseUnit[SECTION_STEEL].weight.precision

  let theoryWeight
  const _length = convertUnits(length, lengthUnit, 'm', 3)
  // 长宽以m为基础单位，换算出来的重量为kg
  theoryWeight = _length * unitWeight
  theoryWeight = convertUnits(theoryWeight, weightUnit, 'kg', precision)
  return Number((theoryWeight * quantity).toFixed(precision))
}

/**
 * 钢卷计算重量的公式
 * @param {Number} weight 重量
 * @param {Number} width 宽度
 * @param {Number} thickness 厚
 * @param {Number} quantity 数量
 * @param {Number} weightUnit 重量单位
 * @param {Number} lengthUnit 长度单位
 * @param {Number} precision 保留小数位
 */
export async function calcSteelCoilLength({ weight, width, thickness, quantity = 1 }) {
  if (!weight || !quantity || !width || !thickness) {
    return null
  }

  const baseUnit = await getBaseUnit()
  const lengthUnit = baseUnit[STEEL_COIL].length.unit
  const lengthPrecision = baseUnit[STEEL_COIL].length.precision
  const weightUnit = baseUnit[STEEL_COIL].weight.unit
  const weightPrecision = baseUnit[STEEL_COIL].weight.precision

  let theoryLength
  const weg = convertUnits(weight, weightUnit, 'kg', weightPrecision)
  const wth = convertUnits(width, lengthUnit, 'm', 3)
  // 计算结果为 m
  theoryLength = weg / (wth * thickness * STEEL_DENSITY)
  theoryLength = convertUnits(theoryLength, 'm', lengthUnit)
  return Number((theoryLength * quantity).toFixed(lengthPrecision))
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
          weight: row.weighingTotalWeight,
          width: row.width,
          thickness: row.thickness
        }).then((val) => {
          row.theoryLength = val
          row.length = row.theoryWeight * row.quantity // 理论总重量
          form.steelCoilList.push(row)
        })
        break
    }
    promiseList.push(p)
  }
  await Promise.all(promiseList)
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
