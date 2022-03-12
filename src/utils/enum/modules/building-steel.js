import { constantize } from '../base'

// 围护结算类型
const enclosureSettlementTypeEnum = {
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1 },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 2 }
}
constantize(enclosureSettlementTypeEnum)

// 产品类型
const componentTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1 << 0 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 1 },
  AUXILIARY_MATERIAL: { L: '辅材', K: 'AUXILIARY_MATERIAL', V: 1 << 2 }
}
constantize(componentTypeEnum)

// 材料清单上传类型
const componentListTypeEnum = {
  ARTIFACT: { L: '构件清单', SL: '构件', K: 'ARTIFACT', V: 1 },
  MACHINE_PART: { L: '零件清单', SL: '零件', K: 'MACHINE_PART', V: 0 },
  ENCLOSURE: { L: '围护清单', SL: '围护', K: 'ENCLOSURE', V: 2 },
  AUXILIARY_MATERIAL: { L: '辅材清单', SL: '辅材', K: 'AUXILIARY_MATERIAL', V: 3 },
  ARTIFACT_TREE: { L: '构件零件关联清单', SL: '零构件', K: 'ARTIFACT_TREE', V: 4 }
}
constantize(componentListTypeEnum)

export { componentTypeEnum, componentListTypeEnum, enclosureSettlementTypeEnum }

export default {
  componentTypeEnum,
  componentListTypeEnum,
  enclosureSettlementTypeEnum
}
