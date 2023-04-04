import { matClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank } from '@/utils/data-type'

import { MAT_BASE_UNIT } from '@/settings/config'

// 规格格式化
export function specFormat(row) {
  switch (row.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateSpec(row)
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelSpec(row)
    case matClsEnum.STEEL_COIL.V:
      return steelCoilSpec(row)
    case matClsEnum.MATERIAL.V:
      return auxMatSpec(row)
    case matClsEnum.GAS.V:
      return gasSpec(row)
    case matClsEnum.STRUC_MANUFACTURED.V:
      return strucSpec(row)
    case matClsEnum.ENCL_MANUFACTURED.V:
      return enclSpec(row)
    default:
      return row.spec
  }
}

// 规格格式化
export function specTip(row) {
  let tip = ''
  switch (row.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      tip = steelPlateSpecTip(row)
      break
    case matClsEnum.SECTION_STEEL.V:
      tip = sectionSteelSpecTip(row)
      break
    case matClsEnum.STEEL_COIL.V:
      tip = steelCoilSpecTip(row)
      break
    case matClsEnum.MATERIAL.V:
      tip = auxMatSpecTip(row)
      break
    case matClsEnum.GAS.V:
      tip = gasSpecTip(row)
      break
    case matClsEnum.STRUC_MANUFACTURED.V:
      tip = strucSpecTip(row)
      break
    case matClsEnum.ENCL_MANUFACTURED.V:
      tip = enclSpecTip(row)
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
  const twl = []
  if (isNotBlank(row.thickness)) {
    twl.push(row.thickness)
  }
  if (isNotBlank(row.width)) {
    twl.push(row.width)
  }
  if (isNotBlank(row.length)) {
    twl.push(row.length)
  }
  if (isNotBlank(twl)) spec.push(twl.join('*'))
  if (isNotBlank(row.specification)) spec.push(row.specification)
  return spec.join(' * ')
}

// 型材规格
function sectionSteelSpec(row) {
  const spec = []
  if (isNotBlank(row.length)) spec.push(row.length)
  if (isNotBlank(row.specification) && row.specificationLabels !== '无规格') spec.push(row.specification)
  return spec.join(' * ')
}

// 钢卷规格
function steelCoilSpec(row) {
  const spec = []
  const twl = []
  if (isNotBlank(row.thickness)) {
    twl.push(row.thickness)
  }
  if (isNotBlank(row.width)) {
    twl.push(row.width)
  }
  if (isNotBlank(row.length)) {
    twl.push(row.length)
  }
  if (isNotBlank(twl)) spec.push(twl.join('*'))
  if (isNotBlank(row.specification) && row.specificationLabels !== '无规格') spec.push(row.specification)
  if (isNotBlank(row.color)) spec.push(row.color)
  return spec.join(' * ')
}

// 辅材规格
function auxMatSpec(row) {
  const spec = []
  if (isNotBlank(row.specification) && row.specificationLabels !== '无规格') spec.push(row.specification)
  if (isNotBlank(row.color)) spec.push(row.color)
  return spec.join(' * ')
}

// 气体规格
function gasSpec(row) {
  return row.specification
}

// 成品构件规格
function strucSpec(row) {
  return row.specification
}

// 成品围护规格
function enclSpec(row) {
  const spec = []
  const twl = []
  if (isNotBlank(row.plateId)) {
    twl.push(row.plateId)
  }
  if (isNotBlank(row.thickness)) {
    twl.push(row.thickness)
  }
  if (isNotBlank(row.color)) {
    twl.push(row.color)
  }
  if (isNotBlank(twl)) spec.push(twl.join('*'))
  return spec.join(' * ')
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------
// 提示

// 钢板规格提示
function steelPlateSpecTip(row) {
  const tip = []
  const twl = []
  if (isNotBlank(row.thickness)) {
    twl.push('厚(mm)')
  }
  if (isNotBlank(row.width)) {
    twl.push('宽(mm)')
  }
  if (isNotBlank(row.length)) {
    twl.push('长(mm)')
  }
  if (isNotBlank(twl)) tip.push(twl.join('*'))
  if (isNotBlank(row.specificationLabels) && row.specificationLabels !== '无规格') tip.push(row.specificationLabels)
  return tip.join(' * ')
}

// 型材规格提示
function sectionSteelSpecTip(row) {
  const tip = []
  if (isNotBlank(row.length)) {
    tip.push('长(mm)')
  }
  if (isNotBlank(row.specificationLabels) && row.specificationLabels !== '无规格') tip.push(row.specificationLabels)
  return tip.join(' * ')
}

// 钢卷规格提示
function steelCoilSpecTip(row) {
  const baseUnit = MAT_BASE_UNIT[matClsEnum.STEEL_COIL.V]
  const tip = []
  const twl = []
  if (isNotBlank(row.thickness)) {
    twl.push('厚(mm)')
  }
  if (isNotBlank(row.width)) {
    twl.push('宽(mm)')
  }
  if (isNotBlank(row.length)) {
    twl.push(`长(${baseUnit.length.unit})`)
  }
  if (isNotBlank(twl)) tip.push(twl.join('*'))
  if (isNotBlank(row.specificationLabels) && row.specificationLabels !== '无规格') tip.push(row.specificationLabels)
  if (isNotBlank(row.color)) tip.push('颜色')
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

// 成品构件规格
function strucSpecTip(row) {
  const tip = []
  tip.push('规格')
  return tip.join(' * ')
}

// 成品围护规格
function enclSpecTip(row) {
  const tip = []
  const twl = []
  if (isNotBlank(row.plateId)) {
    twl.push('板型')
  }
  if (isNotBlank(row.thickness)) {
    twl.push('厚(mm)')
  }
  if (isNotBlank(row.color)) {
    twl.push('颜色')
  }
  if (isNotBlank(twl)) tip.push(twl.join('*'))
  return tip.join(' * ')
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------
// 拼接 自定义规格（如：材质）
export function spliceMaterialSpec(material) {
  const specNameKV = material.specNameKV
  const keys = Object.keys(specNameKV)
  const specArr = []
  if (keys) {
    keys.forEach((key) => {
      // 排除型材的国标
      if (material.basicClass !== matClsEnum.SECTION_STEEL.V || key !== material.nationalStandard) {
        specArr.push(`${key}: ${specNameKV[key]}`)
      }
    })
  }
  return specArr.join('　')
}

// 拼接 钢材形状尺寸
export function spliceSteelSize(material) {
  if (!material) return ''
  switch (material.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      // 厚度mm
      return `${Number(material.thickness)} * ${material.width} * ${material.length}`
    case matClsEnum.SECTION_STEEL.V:
      return `${material.specNameKV[material.nationalStandard] || ''}`
    case matClsEnum.STEEL_COIL.V:
      return `${Number(material.thickness)} * ${material.width}`
    default:
      return ''
  }
}
