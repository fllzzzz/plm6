import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank } from '@/utils/data-type'

import { MAT_BASE_UNIT } from '@/settings/config'

// 规格格式化
export function otherFormat(row) {
  switch (row.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlateOther(row)
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteelOther(row)
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoilOther(row)
    case rawMatClsEnum.MATERIAL.V:
      return auxMatOther(row)
    case rawMatClsEnum.OTHER.V:
      return otherMatOther(row)
    case rawMatClsEnum.GAS.V:
      return gasOther(row)
    default:
      return ''
  }
}

// 规格格式化
export function otherTip(row) {
  let tip = ''
  switch (row.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      tip = steelPlateOtherTip(row)
      break
    case rawMatClsEnum.SECTION_STEEL.V:
      tip = sectionSteelOtherTip(row)
      break
    case rawMatClsEnum.STEEL_COIL.V:
      tip = steelCoilOtherTip(row)
      break
    case rawMatClsEnum.MATERIAL.V:
      tip = auxMatOtherTip(row)
      break
    case rawMatClsEnum.OTHER.V:
      tip = otherMatOtherTip(row)
      break
    case rawMatClsEnum.GAS.V:
      tip = gasOtherTip(row)
      break
    default:
      tip = ''
      break
  }
  return tip
}

// 钢板规格
function steelPlateOther(row) {
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
  return twl.join('*')
}

// 型材规格
function sectionSteelOther(row) {
  const Other = []
  if (isNotBlank(row.length)) Other.push(row.length)
  return Other.join(' * ')
}

// 钢卷规格
function steelCoilOther(row) {
  const Other = []
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
  if (isNotBlank(twl)) Other.push(twl.join('*'))
  if (isNotBlank(row.color)) Other.push(row.color)
  return Other.join(' * ')
}

// 辅材规格
function auxMatOther(row) {
  const Other = []
  if (isNotBlank(row.color)) Other.push(row.color)
  return Other.join(' * ')
}

// 其它规格
function otherMatOther(row) {
  const Other = []
  if (isNotBlank(row.color)) Other.push(row.color)
  return Other.join(' * ')
}

// 气体规格
function gasOther(row) {
  return ''
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------
// 提示

// 钢板规格提示
function steelPlateOtherTip(row) {
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
  return twl.join('*')
}

// 型材规格提示
function sectionSteelOtherTip(row) {
  const tip = []
  if (isNotBlank(row.length)) {
    tip.push('长(mm)')
  }
  return tip.join(' * ')
}

// 钢卷规格提示
function steelCoilOtherTip(row) {
  const baseUnit = MAT_BASE_UNIT[rawMatClsEnum.STEEL_COIL.V]
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
  if (isNotBlank(row.color)) tip.push('颜色')
  return tip.join(' * ')
}

// 辅材规格
function auxMatOtherTip(row) {
  const tip = []
  if (isNotBlank(row.color)) tip.push('颜色')
  return tip.join(' * ')
}

// 其他规格
function otherMatOtherTip(row) {
  const tip = []
  if (isNotBlank(row.color)) tip.push('颜色')
  return tip.join(' * ')
}

// 气体规格
function gasOtherTip(row) {
  return ''
}
