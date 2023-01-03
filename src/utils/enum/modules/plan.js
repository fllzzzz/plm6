import { constantize } from '../base'

/**
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// 制造类型
const manufactureTypeEnum = {
  HOMEMADE: { L: '自制', K: 'HOMEMADE', V: 0 },
  OUTSOURCE: { L: '外协', K: 'OUTSOURCE', V: 1 }
}
constantize(manufactureTypeEnum)

// 部门计划类型
const overallPlanTypeEnum = {
  DEEPEN: { L: '深化计划', K: 'DEEPEN', V: 0 },
  PROCESS: { L: '加工计划', K: 'PROCESS', V: 1 },
  INSTALL: { L: '安装计划', K: 'INSTALL', V: 2 },
  PURCHASE: { L: '采购计划', K: 'PURCHASE', V: 3 }
}
constantize(overallPlanTypeEnum)

// 部门计划类型
const areaPlanTypeEnum = {
  DEEPEN: { L: '深化计划', K: 'DEEPEN', V: 1 << 0 },
  PROCESS: { L: '加工计划', K: 'PROCESS', V: 1 << 1 },
  INSTALL: { L: '安装计划', K: 'INSTALL', V: 1 << 2 }
}
constantize(areaPlanTypeEnum)

// 计划状态
const overallPlanStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 0 },
  COMPLETE: { L: '已完成', K: 'COMPLETE', V: 1 },
  PARTIALLY_COMPLETED: { L: '部分完成', K: 'COMPLETE', V: 2 }
}
constantize(overallPlanStatusEnum)

// 零件剪板类型
const shearTypeEnum = {
  SHEARING: { L: '剪板', K: 'SHEARING', V: 1 },
  CNC: { L: '数控', K: 'CNC', V: 2 }
}
constantize(shearTypeEnum)

// 进行中/暂停
const processingEnum = {
  PROCESS: { L: '进行', K: 'TRUE', V: 1 },
  PAUSE: { L: '暂停', K: 'FALSE', V: 0 }
}
constantize(processingEnum)

// 图纸类型
const planTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 0 },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 2 }
}
constantize(planTypeEnum)

// 深化图纸类型
const deepenTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 1 << 1 },
  ASSEMBLY: { L: '部件', K: 'ASSEMBLY', V: 1 << 4 },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 << 0 }
}
constantize(deepenTypeEnum)

// 技术资料类型
const technicalDataTypeEnum = {
  DEEPEN: { L: '深化图', K: 'DEEPEN', V: 1 << 0 },
  NC_DRAWING: { L: '数控图纸', K: 'NC_DRAWING', V: 1 << 1 },
  CHANGE_FILE: { L: '变更文件', K: 'CHANGE_FILE', V: 1 << 2 },
  BLUEPRINT: { L: '施工蓝图', K: 'BLUEPRINT', V: 1 << 3 },
  XML_FILE: { L: '16XML文件', K: 'XML_FILE', V: 1 << 4 }
}
constantize(technicalDataTypeEnum)

// 围护创建类型
const enclosureCreateTypeEnum = {
  UPLOAD: { L: '导入', K: 'UPLOAD', V: 1 },
  TYPE_IN: { L: '输入', K: 'TYPE_IN', V: 2 }
}
constantize(enclosureCreateTypeEnum)

// 配套件使用属性
const auxiliaryMaterialUseTypeEnum = {
  CONSTRUCTION_SITE: { L: '工地使用', K: 'CONSTRUCTION_SITE', V: 1 },
  FACTORY: { L: '工厂使用', K: 'FACTORY', V: 2 }
}
constantize(auxiliaryMaterialUseTypeEnum)

// 构件编号修改范围
const serialNumChangeTypeEnum = {
  AREA: { L: '当前区域', K: 'AREA', V: 1 },
  MONOMER: { L: '当前单体', K: 'MONOMER', V: 2 }
}
constantize(serialNumChangeTypeEnum)

// 备料范围（类型）
const preparationRangeEnum = {
  PROJECT: { L: '项目', K: 'PROJECT', V: 1 },
  MONOMER: { L: '单体', K: 'MONOMER', V: 2 },
  AREA: { L: '区域', K: 'AREA', V: 3 }
}
constantize(preparationRangeEnum)

// 备料状态
const preparationStatusEnum = {
  UNFINISHED: { L: '未备料', K: 'UNFINISHED', V: 0 },
  FINISHED: { L: '已备料', K: 'FINISHED', V: 1 }
}
constantize(preparationStatusEnum)

// 涂装类型
const paintTypeEnum = {
  FACTORY: { L: '工厂涂装', K: 'FACTORY', V: 1 << 0 },
  CONSTRUCTION_SITE: { L: '工地涂装', K: 'CONSTRUCTION_SITE', V: 1 << 1 }
}
constantize(paintTypeEnum)

// 经典清单导入模式
const importTypeEnum = {
  UNIVERSAL: { L: '通用模板', K: 'UNIVERSAL', V: 1 << 0 },
  NET_WEIGHT: { L: '单重(净重)', K: 'NET_WEIGHT', V: 1 << 1 },
  UNIT_WEIGHT_ACTUAL: { L: '单重(实际)', K: 'UNIT_WEIGHT_ACTUAL', V: 1 << 2 },
  UNIT_WEIGHT_WEIGHTED: { L: '单重(加3%)', K: 'UNIT_WEIGHT_WEIGHTED', V: 1 << 3 }
  // UNIT_WEIGHT_ADD: { L: '经典模板-L+30单重', K: 'UNIT_WEIGHT_ADD', V: 1 << 4 }
}
constantize(importTypeEnum)

export {
  manufactureTypeEnum, // 制造类型
  overallPlanTypeEnum, // 部门计划类型
  overallPlanStatusEnum, // 计划状态
  shearTypeEnum, // 零件剪板类型
  processingEnum,
  planTypeEnum,
  technicalDataTypeEnum,
  areaPlanTypeEnum,
  enclosureCreateTypeEnum,
  auxiliaryMaterialUseTypeEnum,
  deepenTypeEnum,
  serialNumChangeTypeEnum,
  preparationRangeEnum, // 备料范围（类型）
  preparationStatusEnum, // 备料状态
  paintTypeEnum,
  importTypeEnum
}

export default {
  manufactureTypeEnum, // 制造类型
  overallPlanTypeEnum, // 部门计划类型
  overallPlanStatusEnum, // 计划状态
  shearTypeEnum, // 零件剪板类型
  processingEnum,
  planTypeEnum,
  technicalDataTypeEnum,
  areaPlanTypeEnum,
  enclosureCreateTypeEnum,
  auxiliaryMaterialUseTypeEnum,
  deepenTypeEnum,
  serialNumChangeTypeEnum,
  preparationRangeEnum, // 备料范围（类型）
  preparationStatusEnum, // 备料状态
  paintTypeEnum,
  importTypeEnum
}
