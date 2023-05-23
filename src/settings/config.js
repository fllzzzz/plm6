import { projectTypeEnum } from '@/utils/enum/modules/contract'
import { componentListTypeEnum, componentTypeEnum, enclosureSettlementTypeEnum } from '@enum-ms/building-steel'
import { matClsEnum } from '@enum-ms/classification'
import { processMaterialListTypeEnum, wageQuotaTypeEnum } from '@enum-ms/mes'

// 标签默认颜色
export const TAG_DEF_COLOR = '#1682e6'

// 工厂标签默认颜色
export const TAG_FACTORY_DEF_COLOR = '#1682e6'

// 钢板密度
export const STEEL_DENSITY = 7.85

// 不锈钢密度
export const STAINLESS_STEEL_DENSITY = 7.93

// 钢材的val
export const STEEL_ENUM = matClsEnum.STEEL_PLATE.V | matClsEnum.SECTION_STEEL.V | matClsEnum.STEEL_COIL.V

// 产品类型对应默认计价方式
export const wageQuotaTypeMap = {
  [processMaterialListTypeEnum.ARTIFACT.V]: wageQuotaTypeEnum.WEIGHT.V | wageQuotaTypeEnum.LENGTH.V | wageQuotaTypeEnum.QUANTITY.V | wageQuotaTypeEnum.AREA.V,
  [processMaterialListTypeEnum.ASSEMBLE.V]: wageQuotaTypeEnum.WEIGHT.V | wageQuotaTypeEnum.LENGTH.V | wageQuotaTypeEnum.QUANTITY.V,
  [processMaterialListTypeEnum.MACHINE_PART.V]: wageQuotaTypeEnum.WEIGHT.V | wageQuotaTypeEnum.QUANTITY.V,
  [processMaterialListTypeEnum.ENCLOSURE.V]: wageQuotaTypeEnum.LENGTH.V | wageQuotaTypeEnum.QUANTITY.V | wageQuotaTypeEnum.AREA.V
}

// 项目类型：全部
export const allPT = Object.keys(projectTypeEnum.VL).reduce((res, cur) => {
  return res | cur
}, 0)

// 基础配置，钢材误差单位g
export const STEEL_DIFF_UNIT = 'kg'

// 基础配置，钢材尺寸误差单位mm
export const STEEL_SIZE_DIFF_UNIT = 'mm'

// 单位净量小数精度
export const UNIT_NET_PRECISION = 8

// 物料基础单位
export const MAT_BASE_UNIT = {}
MAT_BASE_UNIT[matClsEnum.STEEL_PLATE.V] = {
  measure: { unit: '张', precision: 0 },
  weight: { unit: 'kg', precision: 0 },
  length: { unit: 'mm', precision: 0 },
  width: { unit: 'mm', precision: 0 },
  thickness: { unit: 'mm', precision: 2 }
}
MAT_BASE_UNIT[matClsEnum.SECTION_STEEL.V] = {
  measure: { unit: '根', precision: 0 },
  weight: { unit: 'kg', precision: 0 },
  length: { unit: 'mm', precision: 0 },
  width: { unit: 'mm', precision: 0 }
}
MAT_BASE_UNIT[matClsEnum.STEEL_COIL.V] = {
  measure: { unit: 'm', precision: 0 },
  weight: { unit: 'kg', precision: 0 },
  length: { unit: 'm', precision: 3 },
  width: { unit: 'mm', precision: 0 },
  thickness: { unit: 'mm', precision: 3 }
}

MAT_BASE_UNIT[STEEL_ENUM] = {
  measure: { unit: '件', precision: 0 },
  weight: { unit: 'kg', precision: 0 },
  length: { unit: 'mm', precision: 0 },
  width: { unit: 'mm', precision: 0 }
}

// 钢材单位
export const STEEL_BASE_UNIT = MAT_BASE_UNIT[STEEL_ENUM]

// 系统最小单位（默认）
export const MIN_UNIT = {
  LENGTH: 'mm',
  LENGTH_DP: 3,
  WEIGHT: 'g',
  WEIGHT_DP: 3,
  THICKNESS: 'mm',
  THICKNESS_DP: 3,
  AREA: 'mm²',
  AREA_DP: 0,
  VOLUME: 'mm³',
  VOLUME_DP: 0
}

// 系统常用单位
export const DEF_UNIT = {
  T_WEIGHT: 't',
  T_WEIGHT_DP: 2,
  WEIGHT: 'kg',
  WEIGHT_DP: 2,
  LENGTH: 'mm',
  LENGTH_DP: 3
}

// 二维码拼接路径
export const specialPath = {
  QR_SCAN_ARTIFACT_TASK: '/s/s/a',
  QR_SCAN_ENCLOSURE_TASK: '/s/s/e',
  QR_SCAN_AUXILIARY_MATERIAL: '/s/s/m',
  QR_SCAN_BRIDGE_BOX_TASK: '/s/b/b',
  QR_SCAN_BRIDGE_SINGLE_ELEMENT_TASK: '/s/b/s',
  QR_SCAN_BRIDGE_AUXILIARY_MATERIAL: '/s/b/m'
}

export const MOBILE_MODEL_PATH = '/mobile-model-preview'

export const QR_SCAN_F_TYPE = {
  MEW_PRODUCTION: 1, // 建钢产品
  MES_PACKAGE_SHIP: 2 // 建钢打包发运
}

export const QR_SCAN_TYPE = {
  MES_PACKAGE: 1, // 建钢包
  MES_SHIP_LIST: 2 // 建钢发运清单
}

// mes系统单位
export const MES_MATERIAL_UNIT = {}
MES_MATERIAL_UNIT[componentTypeEnum.STRUCTURE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = {
  unit: 'm',
  normal: 'm',
  smallest: 'mm',
  precision: 3
}
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = {
  unit: '㎡',
  normal: 'm2',
  smallest: 'mm2',
  precision: 3
}
MES_MATERIAL_UNIT[componentTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }

// mes系统，技术清单单位
export const MES_MATERIAL_LIST_UNIT = {}
MES_MATERIAL_UNIT[componentListTypeEnum.ARTIFACT.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.MACHINE_PART.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.ARTIFACT_TREE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = {
  unit: 'm',
  normal: 'm',
  smallest: 'mm',
  precision: 3
}
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = {
  unit: '㎡',
  normal: 'm2',
  smallest: 'mm2',
  precision: 3
}
MES_MATERIAL_UNIT[componentListTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }

export const DP = {
  COM_WT__T: 5, // 通用_重量（t）
  COM_WT__KG: 2, // 通用_重量（kg）
  COM_L__M: 2, // 通用_长度（m）
  COM_L__MM: 0, // 通用_长度（mm）
  COM_WT__G: 3, // 通用_重量（kg）
  COM_AREA__M2: 2, // 通用_面积（㎡）
  COM_VOLUME__L: 2, // 通用_容积—（L）
  COM_T__MM: 3, // 通用_厚度（mm）
  COM_UNIT__KG_M3: 2, // 通用_单位净重（kg/m³）
  COM_UNIT__KG_M2: 2, // 通用_单位净重（kg/㎡）
  COM_UNIT__KG_M: 2, // 通用_单位净重（kg/m）
  MES_ARTIFACT_L__MM: 0, // mes_构件_长度（mm）
  MES_ARTIFACT_L__M: 2, // mes_构件_长度（mm）
  MES_MACHINE_PART_L__MM: 0, // mes_零件_长度（m）
  MES_MACHINE_PART_L__M: 2, // mes_零件_长度（m）
  MES_ENCLOSURE_L__MM: 0, // mes_围护_长度（mm）
  MES_ENCLOSURE_L__M: 3, // mes_围护_长度（m）
  MES_ENCLOSURE_W__MM: 0, // mes_围护_有效宽度（mm）
  MES_ENCLOSURE_T__MM: 3, // mes_围护_厚度（mm）
  YUAN: 2,
  ACCOUNTING: 2 // 会计算法小数点保留2位
}

export const PICKER_OPTIONS_SHORTCUTS = [
  {
    text: '最近一周',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 7)
      return [start, end]
    }
  },
  {
    text: '当前月份',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setDate(1)
      start.setHours(0)
      start.setSeconds(0)
      start.setMinutes(0)
      return [start, end]
    }
  },
  {
    text: '最近一个月',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 30)
      return [start, end]
    }
  },
  {
    text: '最近三个月',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 90)
      return [start, end]
    }
  },
  {
    text: '最近六个月',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setMonth(start.getMonth() - 6)
      return [start, end]
    }
  },
  {
    text: '今年至今',
    value: () => {
      const end = new Date()
      const start = new Date(new Date().getFullYear(), 0)
      return [start, end]
    }
  }
]

export const PICKER_OPTIONS_DATE = [
  {
    text: '最近一周',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 7)
      return [start, end]
    }
  },
  {
    text: '当前月份',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setDate(1)
      start.setHours(0)
      start.setSeconds(0)
      start.setMinutes(0)
      return [start, end]
    }
  },
  {
    text: '最近一个月',
    value: () => {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 30)
      return [start, end]
    }
  }
]
