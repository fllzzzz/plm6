import { materialListTypeEnum, materialTypeEnum, enclosureSettlementTypeEnum } from '@enum-ms/building-steel'
import { materialClassificationEnum } from '@enum-ms/classification'

// 工厂标签默认颜色
export const TAG_FACTORY_DEF_COLOR = '#1682e6'

// 甲供标签默认颜色
export const TAG_PARTY_DEF_COLOR = '#e64242'

// 标签默认颜色
export const TAG_DEF_COLOR = '#1682e6'

// 钢板密度
export const STEEL_DENSITY = 7.85

// 不锈钢密度
export const STAINLESS_STEEL_DENSITY = 7.93

// 钢材的val
export const STEEL_ENUM = materialClassificationEnum.STEEL_PLATE.V | materialClassificationEnum.SECTION_STEEL.V | materialClassificationEnum.STEEL_COIL.V

// 系统最小单位（默认）
export const MIN_UNIT = {
  LENGTH: 'mm',
  LENGTH_DP: 3,
  WEIGHT: 'g',
  WEIGHT_DP: 3,
  THICKNESS: 'mm',
  THICKNESS_DP: 3
}

// mes系统单位
export const MES_MATERIAL_UNIT = {}
MES_MATERIAL_UNIT[materialTypeEnum.STRUCTURE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[materialTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[materialTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = { unit: 'm', normal: 'm', smallest: 'mm', precision: 3 }
MES_MATERIAL_UNIT[materialTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = { unit: '㎡', normal: 'm2', smallest: 'mm2', precision: 3 }
MES_MATERIAL_UNIT[materialTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }

// mes系统，技术清单单位
export const MES_MATERIAL_LIST_UNIT = {}
MES_MATERIAL_UNIT[materialListTypeEnum.ARTIFACT.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[materialListTypeEnum.MACHINE_PART.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[materialListTypeEnum.ARTIFACT_TREE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[materialListTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[materialListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = { unit: 'm', normal: 'm', smallest: 'mm', precision: 3 }
MES_MATERIAL_UNIT[materialListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = { unit: '㎡', normal: 'm2', smallest: 'mm2', precision: 3 }
MES_MATERIAL_UNIT[materialListTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }
