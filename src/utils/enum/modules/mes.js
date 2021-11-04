import { constantize } from '../base'

// 班组类型
const teamTypeEnum = {
  TEAM: { L: '班组', V: 0 },
  INSPECTION: { L: '质检', V: 1 }
}
constantize(teamTypeEnum)

// 班组属性
const teamAttributeEnum = {
  IN_STAFF: { L: '编内', K: 'IN_STAFF', V: true },
  OFF_STAFF: { L: '编外', K: 'OFF_STAFF', V: false }
}
constantize(teamAttributeEnum)

// mes 产品类型
const productTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 1 << 1 },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 << 0 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 2 },
  AUXILIARY_MATERIAL: { L: '辅材', K: 'AUXILIARY_MATERIAL', V: 1 << 3 },
  ASSEMBLE: { L: '组立', K: 'ASSEMBLE', V: 1 << 4 }
}
constantize(productTypeEnum)

// 含有工序的材料类型
const processMaterialListTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: productTypeEnum.ARTIFACT.V, T: 'primary' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: productTypeEnum.MACHINE_PART.V, T: 'success' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: productTypeEnum.ENCLOSURE.V, T: 'warning' }
}
constantize(processMaterialListTypeEnum)

export {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum
}

export default {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum
}
