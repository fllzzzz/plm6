
import { constantize } from '../base'

// 供应商类型分类 TODO:类型优化
const supplierClassEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型钢', K: 'STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  ENCLOSURE: { L: '成品围护', K: 'ENCLOSURE', V: 1 << 3 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 4 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 5 },
  LOGISTICS: { L: '物流', K: 'LOGISTICS', V: 1 << 6 }
}
constantize(supplierClassEnum)

// 供应商基础分类
const supplierBasicClassEnum = {
  MATERIAL_SUPPLIER: { L: '物料供应商', K: 'MATERIAL_SUPPLIER', V: 1 << 0 },
  LOGISTICS_SUPPLIER: { L: '物流供应商', K: 'LOGISTICS_SUPPLIER', V: 1 << 1 }
}
constantize(supplierBasicClassEnum)

// 供应商是否是隐藏
const supplierIsHideEnum = {
  SHOW: { L: '显示', K: 'SHOW', V: 0 },
  HIDE: { L: '隐藏', K: 'HIDE', V: 1 }
}
constantize(supplierIsHideEnum)

export {
  supplierClassEnum, // 供应商类型
  supplierBasicClassEnum, // 供应商基础分类
  supplierIsHideEnum // 供应商是否是隐藏
}

export default {
  supplierClassEnum, // 供应商类型
  supplierBasicClassEnum, // 供应商基础分类
  supplierIsHideEnum // 供应商是否是隐藏
}
