import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank } from '@/utils/data-type'

// 规格格式化
export function specFormat(row) {
  switch (row.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlateSpec(row)
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteelSpec(row)
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoilSpec(row)
    case rawMatClsEnum.MATERIAL.V:
      return steelCoilSpec(row)
    case rawMatClsEnum.GAS.V:
      return steelCoilSpec(row)
    default:
      return ''
  }
}

// 规格格式化
export function specTip(row) {
  switch (row.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlateSpecTip(row)
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteelSpecTip(row)
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoilSpecTip(row)
    case rawMatClsEnum.MATERIAL.V:
      return steelCoilSpecTip(row)
    case rawMatClsEnum.GAS.V:
      return steelCoilSpecTip(row)
    default:
      return ''
  }
}

// 钢板规格
function steelPlateSpec(row) {
  const spec = []
  if (isNotBlank(row.specification)) spec.push(row.specification)
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) spec.push(`${row.thickness} * ${row.width} * ${row.length}`)
  return spec.join(' | ')
}

// 型材规格
function sectionSteelSpec(row) {
  const spec = []
  if (isNotBlank(row.specification)) spec.push(row.specification)
  if (isNotBlank(row.length)) spec.push(row.length)
  return spec.join(' | ')
}

// 钢卷规格
function steelCoilSpec(row) {
  const spec = []
  if (isNotBlank(row.specification)) spec.push(row.specification)
  if (isNotBlank(row.color)) spec.push(row.color)
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) spec.push(`${row.thickness} * ${row.width} * ${row.length}`)
  return spec.join(' | ')
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------

// 钢板规格提示
function steelPlateSpecTip(row) {
  const tip = []
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) tip.push('厚(mm) * 宽(mm) * 长(mm)')
  return tip.join(' | ')
}

// 型材规格提示
function sectionSteelSpecTip(row) {
  const tip = []
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  if (isNotBlank(row.length)) { tip.push('长(mm)') }
  return tip.join(' | ')
}

// 钢卷规格提示
function steelCoilSpecTip(row) {
  const tip = []
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  if (isNotBlank(row.color)) tip.push('颜色')
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) tip.push('厚(mm) * 宽(mm) * 长(mm)')
  return tip.join(' | ')
}
