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
      return auxMatSpec(row)
    case rawMatClsEnum.GAS.V:
      return gasSpec(row)
    default:
      return row.spec
  }
}

// 规格格式化
export function specTip(row) {
  let tip = ''
  switch (row.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      tip = steelPlateSpecTip(row)
      break
    case rawMatClsEnum.SECTION_STEEL.V:
      tip = sectionSteelSpecTip(row)
      break
    case rawMatClsEnum.STEEL_COIL.V:
      tip = steelCoilSpecTip(row)
      break
    case rawMatClsEnum.MATERIAL.V:
      tip = auxMatSpecTip(row)
      break
    case rawMatClsEnum.GAS.V:
      tip = gasSpecTip(row)
      break
    default:
      tip = row.specificationLabels
      break
  }
  return tip || '无规格'
}

// 钢板规格
function steelPlateSpec(row) {
  const spec = []
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) spec.push(`${row.thickness}*${row.width}*${row.length}`)
  if (isNotBlank(row.specification)) spec.push(row.specification)
  return spec.join(' * ')
}

// 型材规格
function sectionSteelSpec(row) {
  const spec = []
  if (isNotBlank(row.length)) spec.push(row.length)
  if (isNotBlank(row.specification)) spec.push(row.specification)
  return spec.join(' * ')
}

// 钢卷规格
function steelCoilSpec(row) {
  const spec = []
  if (isNotBlank(row.thickness) && isNotBlank(row.width)) spec.push(`${row.thickness}*${row.width}`)
  if (isNotBlank(row.specification)) spec.push(row.specification)
  if (isNotBlank(row.color)) spec.push(row.color)
  // if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) spec.push(`${row.thickness}*${row.width}*${row.length}`)
  return spec.join(' * ')
}

// 辅材规格
function auxMatSpec(row) {
  const spec = []
  if (isNotBlank(row.specification)) spec.push(row.specification)
  if (isNotBlank(row.color)) spec.push(row.color)
  return spec.join(' * ')
}

// 气体规格
function gasSpec(row) {
  return row.specification
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------

// 钢板规格提示
function steelPlateSpecTip(row) {
  const tip = []
  if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) tip.push('厚(mm)*宽(mm)*长(mm)')
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  return tip.join(' * ')
}

// 型材规格提示
function sectionSteelSpecTip(row) {
  const tip = []
  if (isNotBlank(row.length)) {
    tip.push('长(mm)')
  }
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  return tip.join(' * ')
}

// 钢卷规格提示
function steelCoilSpecTip(row) {
  const tip = []
  if (isNotBlank(row.thickness) && isNotBlank(row.width)) tip.push('厚(mm)*宽(mm)')
  if (isNotBlank(row.specificationLabels)) tip.push(row.specificationLabels)
  if (isNotBlank(row.color)) tip.push('颜色')
  // if (isNotBlank(row.thickness) && isNotBlank(row.width) && isNotBlank(row.length)) tip.push('厚(mm)*宽(mm)*长(mm)')
  return tip.join(' * ')
}

// 辅材规格
function auxMatSpecTip(row) {
  const tip = []
  if (isNotBlank(row.specificationLabels) && row.specificationLabels !== '无规格') tip.push(row.specificationLabels)
  if (isNotBlank(row.color)) tip.push('颜色')
  return tip.join(' * ')
}

// 气体规格
function gasSpecTip(row) {
  const tip = []
  if (isNotBlank(row.specificationLabels) && row.specificationLabels !== '无规格') tip.push(row.specificationLabels)
  return tip.join(' * ')
}
