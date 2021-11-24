import { projectTypeEnum } from '@/utils/enum/modules/contract'
import { componentListTypeEnum, componentTypeEnum, enclosureSettlementTypeEnum } from '@enum-ms/building-steel'
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

// 项目类型：全部
export const allPT = Object.keys(projectTypeEnum.VL).reduce((res, cur) => {
  return res | cur
}, 0)

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
MES_MATERIAL_UNIT[componentTypeEnum.STRUCTURE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = { unit: 'm', normal: 'm', smallest: 'mm', precision: 3 }
MES_MATERIAL_UNIT[componentTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = { unit: '㎡', normal: 'm2', smallest: 'mm2', precision: 3 }
MES_MATERIAL_UNIT[componentTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }

// mes系统，技术清单单位
export const MES_MATERIAL_LIST_UNIT = {}
MES_MATERIAL_UNIT[componentListTypeEnum.ARTIFACT.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.MACHINE_PART.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.ARTIFACT_TREE.V] = { unit: 'kg', normal: 'kg', smallest: 'g', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V] = {}
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.LENGTH.V] = { unit: 'm', normal: 'm', smallest: 'mm', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.ENCLOSURE.V][enclosureSettlementTypeEnum.AREA.V] = { unit: '㎡', normal: 'm2', smallest: 'mm2', precision: 3 }
MES_MATERIAL_UNIT[componentListTypeEnum.AUXILIARY_MATERIAL.V] = { unit: '件' }

export const DP = {
  COM_WT__T: 2, // 通用_重量（t）
  COM_WT__KG: 2, // 通用_重量（kg）
  COM_WT__G: 3, // 通用_重量（kg）
  COM_AREA__M2: 2, // 通用_面积（㎡）
  COM_UNIT__KG_M3: 2, // 通用_单位净重（kg/m³）
  COM_UNIT__KG_M2: 2, // 通用_单位净重（kg/㎡）
  COM_UNIT__KG_M: 2, // 通用_单位净重（kg/m）
  MES_ARTIFACT_L__MM: 0, // mes_构件_长度（mm）
  MES_ARTIFACT_L__M: 2, // mes_构件_长度（mm）
  MES_MACHINE_PART_L__MM: 0, // mes_零件_长度（m）
  MES_MACHINE_PART_L__M: 2, // mes_零件_长度（m）
  MES_ENCLOSURE_L__MM: 0, // mes_围护_长度（mm）
  MES_ENCLOSURE_L__M: 2, // mes_围护_长度（m）
  MES_ENCLOSURE_W__MM: 0, // mes_围护_有效宽度（mm）
  MES_ENCLOSURE_T__MM: 3 // mes_围护_厚度（mm）
}

export const PICKER_OPTIONS_SHORTCUTS = [{
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
