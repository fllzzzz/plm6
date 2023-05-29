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
  INSTALL: { L: '安装计划', K: 'INSTALL', V: 1 << 2 },
  DELIVERY: { L: '发运计划', K: 'DELIVERY', V: 1 << 3 }
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

// 工艺类型通用专用
const processUseTypeEnum = {
  NORMAL: { L: '通用', K: 'NORMAL', V: false },
  SPECIAL: { L: '专用', K: 'SPECIAL', V: true }
}
constantize(processUseTypeEnum)

// 工艺类型
const planProcessTypeEnum = {
  WELD: { L: '焊接工艺', K: 'WELD', V: 1 << 0 },
  ASSEMBLE: { L: '装配工艺', K: 'ASSEMBLE', V: 1 << 1 },
  PAINT: { L: '涂装工艺', K: 'PAINT', V: 1 << 2 }
}
constantize(planProcessTypeEnum)

// 工艺文件是否绑定构件
const isArtifactBindTypeEnum = {
  NO: { L: '未绑定', K: 'NO', V: false },
  YES: { L: '已绑定', K: 'YES', V: true }
}
constantize(isArtifactBindTypeEnum)

// 变更原因类型
const changeReasonTypeEnum = {
  CUSTOM_REASON: { L: '甲方原因', K: 'CUSTOM_REASON', V: 1 << 0 },
  DESIGN_CHANGE: { L: '设计变更', K: 'DESIGN_CHANGE', V: 1 << 1 },
  SELF_REASON: { L: '自身原因', K: 'SELF_REASON', V: 1 << 2 }
}
constantize(changeReasonTypeEnum)

// 构件绑定状态
const artifactBindTypeEnum = {
  NO: { L: '未绑定', K: 'NO', V: 1 << 0 },
  YES: { L: '已绑定', K: 'YES', V: 1 << 1 },
  BIND_OTHER: { L: '绑定其他', K: 'BIND_OTHER', V: 1 << 2 }
}
constantize(artifactBindTypeEnum)

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
  planProcessTypeEnum,
  processUseTypeEnum,
  isArtifactBindTypeEnum,
  changeReasonTypeEnum,
  artifactBindTypeEnum
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
  planProcessTypeEnum,
  processUseTypeEnum,
  isArtifactBindTypeEnum,
  changeReasonTypeEnum,
  artifactBindTypeEnum
}
