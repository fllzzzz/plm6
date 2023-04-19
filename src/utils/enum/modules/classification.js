import { constantize } from '../base'

// 基础分类
const classificationEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 3 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 4 },
  OTHER: { L: '其它', K: 'OTHER', V: 1 << 7 },
  STRUC_MANUFACTURED: { L: '成品构件', K: 'STRUC_MANUFACTURED', V: 1 << 5 },
  ENCL_MANUFACTURED: { L: '成品围护', K: 'ENCL_MANUFACTURED', V: 1 << 6 }
}
constantize(classificationEnum)

// 物料基础分类
const matClsEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 3 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 4 },
  OTHER: { L: '其它', K: 'OTHER', V: 1 << 7 },
  STRUC_MANUFACTURED: { L: '成品构件', K: 'STRUC_MANUFACTURED', V: 1 << 5 },
  ENCL_MANUFACTURED: { L: '成品围护', K: 'ENCL_MANUFACTURED', V: 1 << 6 }
}
constantize(matClsEnum)

// 普通物料基础分类（不含制成品）
const rawMatClsEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 3 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 4 },
  OTHER: { L: '其它', K: 'OTHER', V: 1 << 7 }
}
constantize(rawMatClsEnum)

// 钢材物料分类
const steelClsEnum = {
  STEEL_PLATE: matClsEnum.STEEL_PLATE,
  SECTION_STEEL: matClsEnum.SECTION_STEEL,
  STEEL_COIL: matClsEnum.STEEL_COIL
}
constantize(steelClsEnum)

// 制成品基础分类
const manufClsEnum = {
  STRUC_MANUFACTURED: classificationEnum.STRUC_MANUFACTURED,
  ENCL_MANUFACTURED: classificationEnum.ENCL_MANUFACTURED
}
constantize(manufClsEnum)

// 项目备料材料类型
const projectPreparationMatClsEnum = {
  STEEL_PLATE: matClsEnum.STEEL_PLATE,
  SECTION_STEEL: matClsEnum.SECTION_STEEL,
  STEEL_COIL: matClsEnum.STEEL_COIL,
  MATERIAL: matClsEnum.MATERIAL,
  OTHER: matClsEnum.OTHER
}
constantize(projectPreparationMatClsEnum)

// 型材、非型材筛选
const extrusionClsEnum = {
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: true },
  NOT_SECTION_STEEL: { L: '非型材', K: 'NOT_SECTION_STEEL', V: false }
}
constantize(extrusionClsEnum)

// 材料申购物料分类
const materialPurchaseClsEnum = {
  STEEL: { L: '钢材', K: 'STEEL', V: matClsEnum.STEEL_PLATE.V | matClsEnum.SECTION_STEEL.V | matClsEnum.STEEL_COIL.V, T: 'success' },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: matClsEnum.MATERIAL.V, T: '' },
  OTHER: { L: '其他', K: 'OTHER', V: matClsEnum.OTHER.V, T: 'info' },
  MANUFACTURED: { L: '制成品', K: 'MANUFACTURED', V: matClsEnum.STRUC_MANUFACTURED.V | matClsEnum.ENCL_MANUFACTURED.V, T: 'warning' }
}
constantize(materialPurchaseClsEnum)

export {
  classificationEnum,
  matClsEnum,
  rawMatClsEnum,
  manufClsEnum,
  steelClsEnum,
  materialPurchaseClsEnum,
  projectPreparationMatClsEnum,
  extrusionClsEnum
}

export default {
  classificationEnum,
  matClsEnum,
  rawMatClsEnum,
  manufClsEnum,
  steelClsEnum,
  materialPurchaseClsEnum,
  projectPreparationMatClsEnum,
  extrusionClsEnum
}
