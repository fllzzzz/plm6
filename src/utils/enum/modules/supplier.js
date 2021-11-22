
import { constantize } from '../base'

// 供应商类型分类 TODO:类型优化
const supplierClassEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型钢', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 3 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 4 },
  STRUC_MANUFACTURED: { L: '成品构件', K: 'STRUC_MANUFACTURED', V: 1 << 5 },
  ENCL_MANUFACTURED: { L: '成品围护', K: 'ENCL_MANUFACTURED', V: 1 << 6 },
  LOGISTICS: { L: '物流', K: 'LOGISTICS', V: 1 << 10 }
}
constantize(supplierClassEnum)

// 供应商类型
const supplierTypeEnum = {
  RAW_MATERIAL: { L: '原材料', K: 'RAW_MATERIAL', V: 1 << 0 },
  MANUFACTURED: { L: '制成品', K: 'MANUFACTURED', V: 1 << 1 },
  LOGISTICS: { L: '物流', K: 'LOGISTICS', V: 1 << 2 }
}
constantize(supplierTypeEnum)

// 供应商是否是隐藏
const supplierIsHideEnum = {
  FALSE: { L: '显示', K: 'FALSE', V: false },
  TRUE: { L: '隐藏', K: 'TRUE', V: true }
}
constantize(supplierIsHideEnum)

export {
  supplierClassEnum, // 供应商类型
  supplierTypeEnum,
  supplierIsHideEnum // 供应商是否是隐藏
}

export default {
  supplierClassEnum, // 供应商类型
  supplierTypeEnum,
  supplierIsHideEnum // 供应商是否是隐藏
}
