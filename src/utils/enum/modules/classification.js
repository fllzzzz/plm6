import { constantize } from '../base'

// 基础分类
const classificationEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MANUFACTURED: { L: '制成品', K: 'MANUFACTURED', V: 1 << 3 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 4 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 5 }
}
constantize(classificationEnum)

// 物料基础分类
const materialClassificationEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MANUFACTURED: { L: '制成品', K: 'MANUFACTURED', V: 1 << 3 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 4 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 5 }
}
constantize(materialClassificationEnum)

// 普通物料基础分类（不含制成品）
const normMatClsEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 4 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 5 }
}
constantize(normMatClsEnum)

export {
  classificationEnum,
  materialClassificationEnum,
  normMatClsEnum
}

export default {
  classificationEnum,
  materialClassificationEnum,
  normMatClsEnum
}
