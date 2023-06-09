import { constantize } from '../base'

/**
 * @author dhh
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// TODO:项目状态
const projectStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 0, TAG: '' },
  SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 1 << 1, TAG: 'danger' },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 1 << 2, TAG: 'warning' },
  SETTLED: { L: '已结算', K: 'COMPLETE', V: 1 << 3, TAG: 'success' }
}
constantize(projectStatusEnum)

// 项目状态
// const projectStatusEnum = {
//   PROCESS: { L: '进行中', K: 'PROCESS', V: 1 },
//   SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 2 },
//   COMPLETE: { L: '已完工', K: 'COMPLETE', V: 3 }
// }
// constantize(projectStatusEnum)

// 项目展示状态
const projectNameArrangementModeEnum = {
  SERIAL_NUMBER_START: { L: '合同编号 项目名称', K: 'SERIAL_NUMBER_START', V: 1 },
  SERIAL_NUMBER_END: { L: '项目名称 合同编号', K: 'SERIAL_NUMBER_END', V: 2 }
}
constantize(projectNameArrangementModeEnum)

// 项目类型
const projectTypeEnum = {
  STEEL: { L: '建筑钢结构', SL: '建钢', K: 'STEEL', V: 1 << 0 },
  BRIDGE: { L: '桥梁钢结构', SL: '桥梁', K: 'BRIDGE', V: 1 << 1 },
  CARBARN: { L: '立体停车库', SL: '停车库', K: 'CARBARN', V: 1 << 2 },
  ENCLOSURE: { L: '建筑围护结构', SL: '围护', K: 'ENCLOSURE', V: 1 << 3 }
}
constantize(projectTypeEnum)

// 技术交底type
const TechnologyMainTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1 << 1 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 2 },
  AUXILIARY_MATERIAL: { L: '配套件', K: 'AUXILIARY_MATERIAL', V: 1 << 3 },
  BOX: { L: '箱体', K: 'BOX', V: 1 << 4 }
}
constantize(TechnologyMainTypeEnum)

// 技术交底类型1
const TechnologyTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1 << 0 },
  PROFILED_PLATE: { L: '压型彩板', K: 'PROFILED_PLATE', V: 1 << 1 },
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSURE_BEARING_PLATE: { L: '压型楼承板', K: 'PRESSURE_BEARING_PLATE', V: 1 << 4 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 }
}
constantize(TechnologyTypeEnum)

// all技术交底类型1
const TechnologyTypeAllEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1 << 0 },
  PROFILED_PLATE: { L: '压型彩板', K: 'PROFILED_PLATE', V: 1 << 1 },
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSURE_BEARING_PLATE: { L: '压型楼承板', K: 'PRESSURE_BEARING_PLATE', V: 1 << 4 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 },
  BENDING: { L: '折边件', K: 'BENDING', V: 1 << 5 },
  BRIDGE: { L: '箱体', K: 'BRIDGE', V: 1 << 7 }
}
constantize(TechnologyTypeAllEnum)

// 业务类型
const businessTypeEnum = {
  MACHINING: { L: '加工承揽', K: 'MACHINING', V: 1 },
  INSTALLATION: { L: '项目承包', K: 'INSTALLATION', V: 2 }
}
constantize(businessTypeEnum)

// 商务变更
const businessModifyTypeEnum = {
  // MODIFY_PRICE: { L: '修改价格', K: 'MODIFY_PRICE', V: 1 << 0 },
  SEPARATE: { L: '单独计价', K: 'SEPARATE', V: 1 << 1 },
  MERGER: { L: '合并计价', K: 'MERGER', V: 1 << 2 }
}
constantize(businessModifyTypeEnum)

// // 合同订单类型
// const orderTypeEnum = {
//   MACHINING: { L: '加工订单', K: 'MACHINING', V: 0 },
//   INSTALLATION: { L: '工程项目', K: 'INSTALLATION', V: 1 }
// }
// constantize(orderTypeEnum)

// 是否含税
const isTaxEnum = {
  YES: { L: '是', K: 'YES', V: true },
  NO: { L: '否', K: 'NO', V: false }
}
constantize(isTaxEnum)

// 是否含税
const isTaxContractEnum = {
  YES: { L: '是', SL: '含税', K: 'YES', V: 1 },
  NO: { L: '否', SL: '不含税', K: 'NO', V: 0 }
}
constantize(isTaxContractEnum)

// 工程结算类型新
const engineerSettlementTypeEnumN = {
  THEORY: { L: '理计结算', SL: '理计', K: 'THEORY', V: 1 << 0, COLOR: '#1682e6' },
  OVERWEIGHT: { L: '磅计结算', SL: '磅计', K: 'OVERWEIGHT', V: 1 << 1, COLOR: '#e64242' }
}
constantize(engineerSettlementTypeEnumN)

// 审核类型
const auditTypeEnum = {
  AUDITING: { L: '审核中', K: 'AUDITING', V: 1 << 0 },
  PASS: { L: '通过', K: 'PASS', V: 1 << 1 },
  REJECT: { L: '驳回', K: 'REJECT', V: 1 << 2 }
}
constantize(auditTypeEnum)

// 报销审核类型
const reimbursementTypeEnum = {
  AUDITING: { L: '确认中', K: 'AUDITING', V: 1 << 0 },
  PASS: { L: '已确认', K: 'PASS', V: 1 << 1 },
  REJECT: { L: '已退回', K: 'REJECT', V: 1 << 2 }
}
constantize(reimbursementTypeEnum)

// 变更类型1
const contractChangeTypeEnum = {
  CONTRACT_INFO: { L: '变更合同信息', K: 'CONTRACT_INFO', V: 1 << 0 },
  CONTRACT_AMOUNT: { L: '变更合同金额', K: 'CONTRACT_AMOUNT', V: 1 << 1 }
  // CONTRACT_SETTLE: { L: '项目结算', K: 'CONTRACT_SETTLE', V: 1 << 2 },
  // VARIATION_ORDER: { L: '签证变更', K: 'VARIATION_ORDER', V: 1 << 3 }
}
constantize(contractChangeTypeEnum)

// 围护结算类型
const enclosureSettlementTypeEnum = {
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1, UNIT: '米' },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 2, UNIT: '平方米' }
}
constantize(enclosureSettlementTypeEnum)

// TODO: 运输方式 与提货方式重复
const transportModeEnum = {
  HOME_DELIVERY: { L: '运送到场', SL: '到场', K: 'HOME_DELIVERY', V: 1 << 0 },
  SELF_DELIVERY: { L: '客户自提', SL: '自提', K: 'SELF_DELIVERY', V: 1 << 1 }
}
constantize(transportModeEnum)

// TODO: 系统
const systemTypeEnum = {
  PC: { L: '当前系统', K: 'PC', V: 1 },
  DING: { L: '钉钉', K: 'DING', V: 2 }
}
constantize(systemTypeEnum)

// TODO: 开票时间类型1
const contractDateTypeEnum = {
  UPDATE_DATE: { L: '填报日期', K: 'UPDATE_DATE', V: 1 },
  AUDIT_DATE: { L: '审核日期', K: 'AUDIT_DATE', V: 2 },
  INVOICE_DATE: { L: '开票日期', K: 'INVOICE_DATE', V: 3 }
}
constantize(contractDateTypeEnum)

// TODO: 收款时间类型1
const collectionDateTypeEnum = {
  UPDATE_DATE: { L: '填报日期', K: 'UPDATE_DATE', V: 1 },
  AUDIT_DATE: { L: '审核日期', K: 'AUDIT_DATE', V: 2 },
  INVOICE_DATE: { L: '填报日期', K: 'INVOICE_DATE', V: 3 }
}
constantize(collectionDateTypeEnum)

// TODO: 付款属性1
const supplierPayMentTypeEnum = {
  MATERIAL: { L: '原材料采购', K: 'MATERIAL', V: 1 << 0 },
  PRODUCT: { L: '制成品采购', K: 'PRODUCT', V: 1 << 1 },
  MATERIAL_TRANSPORT: { L: '原材料运输', K: 'MATERIAL_TRANSPORT', V: 1 << 2 },
  PRODUCT_TRANSPORT: { L: '制成品运输', K: 'PRODUCT_TRANSPORT', V: 1 << 3 },
  SUBCONTRACT: { L: '专业分包', K: 'SUBCONTRACT', V: 1 << 4 }
}
constantize(supplierPayMentTypeEnum)

// TODO: 付款时间类型1
const contractPayDateTypeEnum = {
  UPDATE_DATE: { L: '填报日期', K: 'UPDATE_DATE', V: 1 },
  AUDIT_DATE: { L: '审核日期', K: 'AUDIT_DATE', V: 2 },
  INVOICE_DATE: { L: '付款日期', K: 'INVOICE_DATE', V: 3 }
}
constantize(contractPayDateTypeEnum)

// TODO: 收票时间类型1
const contractReceiveDateTypeEnum = {
  UPDATE_DATE: { L: '填报日期', K: 'UPDATE_DATE', V: 1 },
  AUDIT_DATE: { L: '审核日期', K: 'AUDIT_DATE', V: 2 },
  INVOICE_DATE: { L: '收票日期', K: 'INVOICE_DATE', V: 3 }
}
constantize(contractReceiveDateTypeEnum)

// TODO: 报销时间类型1
const contractReimbursementDateEnum = {
  UPDATE_DATE: { L: '填报日期', K: 'UPDATE_DATE', V: 1 },
  AUDIT_DATE: { L: '确认日期', K: 'AUDIT_DATE', V: 2 },
  APPLY_DATE: { L: '申请日期', K: 'APPLY_DATE', V: 3 }
}
constantize(contractReimbursementDateEnum)

// TODO: 费用类别1
const contractPayForEnum = {
  GOODS_FEE: { L: '货款', K: 'GOODS_FEE', V: 1 },
  TRANSPORT_FEE: { L: '运费', K: 'TRANSPORT_FEE', V: 2 }
}
constantize(contractPayForEnum)

// 供应商付款方式
// const supplierPayModeEnum = {
//   PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: 1 << 0 },
//   PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: 1 << 1 },
//   ACCEPTANCE_DRAFT: { L: '承兑汇票', K: 'ACCEPTANCE_DRAFT', V: 1 << 2 },
//   TRANSFER_CHECK: { L: '转账支票', K: 'TRANSFER_CHECK', V: 1 << 3 }

// }
// constantize(supplierPayModeEnum)

// TODO: 付款属性1
const supplierPayTypeEnum = {
  PURCHASE: { L: '采购合同', K: 'PURCHASE', V: 1 << 0 },
  TRANSPORT: { L: '物流', K: 'TRANSPORT', V: 1 << 1 },
  PRODUCT: { L: '制成品', K: 'PURCHASE', V: 1 << 2 },
  SUBCONTRACT: { L: '分包订单', K: 'SUBCONTRACT', V: 1 << 3 }
}
constantize(supplierPayTypeEnum)

// TODO: 项目模式
const projectModeEnum = {
  STRUCTURE: { L: '简约模式', K: 'STRUCTURE', V: 1 << 0 },
  STRUCTURE_ASSEMBLE: { L: '标准模式', K: 'STRUCTURE_ASSEMBLE', V: 1 << 1 }
}
constantize(projectModeEnum)

// TODO: 应付查询属性
const payableSearchTypeEnum = {
  PURCHASE: { L: '采购合同', K: 'PURCHASE', V: 127 },
  TRANSPORT: { L: '物流', K: 'TRANSPORT', V: 1024 }
}
constantize(payableSearchTypeEnum)

// TODO: 有无应付
const hasPayEnum = {
  YES: { L: '有应付', K: 'YES', V: true },
  NO: { L: '无应付', K: 'NO', V: false }
}
constantize(hasPayEnum)

// TODO: 有无欠税
const hasTaxEnum = {
  YES: { L: '有欠税', K: 'YES', V: true },
  NO: { L: '无欠税', K: 'NO', V: false }
}
constantize(hasTaxEnum)

// 采购合同状态
const purchaseOrderStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 },
  COMPLETE: { L: '已完成', K: 'COMPLETE', V: 2 }
}
constantize(purchaseOrderStatusEnum)

// 物流搜索类型
const logisticsSearchTypeEnum = {
  PRODUCT: { L: '制成品运输', K: 'PRODUCT', V: 1 << 1 },
  MATERIAL: { L: '原材料运输', K: 'MATERIAL', V: 1 << 0 },
  COMPANY: { L: '物流公司', K: 'COMPANY', V: 1 << 2 }
}
constantize(logisticsSearchTypeEnum)

// 价格管理构件计价方式
const pricingMannerEnum = {
  WEIGHT: { L: '重量', K: 'WEIGHT', V: 0, UNIT: '吨' },
  LENGTH: { L: '长度', K: 'LENGTH', V: 1, UNIT: '米' }
}
constantize(pricingMannerEnum)
// 分包订单项目结算状态
const subOrderSettleEnum = {
  UNSETTLEMENT: { L: '进行中', K: 'UNSETTLEMENT', V: 0 },
  SETTLED: { L: '已结算', K: 'SETTLED', V: 1 }
}
constantize(subOrderSettleEnum)

// 分包订单项目状态
const subOrderStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 0, TAG: '' },
  SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 1 << 1, TAG: 'danger' },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 1 << 2, TAG: 'warning' }
}
constantize(subOrderStatusEnum)

// 订单来源
const orderSourceTypeEnum = {
  INSIDE: { L: '内部', SL: '内部订单', K: 'INSIDE', V: 1 << 0 },
  OUTSIDE: { L: '外部', SL: '外部订单', K: 'OUTSIDE', V: 1 << 1 }
}
constantize(orderSourceTypeEnum)

// 折旧类型
const depreciationTypeEnum = {
  PLANT: { L: '厂房折旧', K: 'PLANT', V: 1 << 0 },
  DEVICE: { L: '设备折旧', K: 'DEVICE', V: 1 << 1 }
}
constantize(depreciationTypeEnum)

// 费用类型
const costTypeEnum = {
  ELECTRIC_COST: { L: '电费', K: 'ELECTRIC_COST', V: 1 << 1 },
  WATER_COST: { L: '水费', K: 'WATER_COST', V: 1 << 0 }
  // GAS_COST: { L: '气体', K: 'GAS_COST', V: 1 << 2 }
}
constantize(costTypeEnum)

// 用电类型
const usedElectricityTypeEnum = {
  INDUSTRY_ELECTRIC: { L: '工业用电', K: 'INDUSTRY_ELECTRIC', V: 1 << 0 },
  CIVIL_ELECTRIC: { L: '民用用电', K: 'CIVIL_ELECTRIC', V: 1 << 1 }
}
constantize(usedElectricityTypeEnum)

// 气体类型
const gasTypeEnum = {
  GAS_O2: { L: '氧气', K: 'GAS_O2', V: 1 << 0 },
  GAS_C2H2: { L: '乙炔', K: 'GAS_C2H2', V: 1 << 1 },
  GAS_C3H8: { L: '丙烷', K: 'GAS_C3H8', V: 1 << 2 }
}
constantize(gasTypeEnum)

// 时间类型
const timeTypeEnum = {
  ALL_YEAR: { L: '全年', SL: '年', K: 'ALL_YEAR', V: 1 << 0 },
  CURRENT_MONTH: { L: '当月', SL: '月', K: 'CURRENT_MONTH', V: 1 << 1 }
}
constantize(timeTypeEnum)

// 不同人员类型工资
const managementSalaryTypeEnum = {
  MANAGEMENT_SALARY: { L: '管理人员工资', K: 'MANAGEMENT_SALARY', V: 1 << 0 },
  PRODUCTION_SALARY: { L: '生产人员工资', K: 'PRODUCTION_SALARY', V: 1 << 1 }
  // OTHER_SALARY: { L: '其他工资', K: 'OTHER_SALARY', V: 1 << 2 }
}
constantize(managementSalaryTypeEnum)

// 主材辅材
const mainAuxiliaryTypeEnum = {
  MAIN: { L: '主材', K: 'MAIN', V: 7 },
  AUXILIARY: { L: '辅材', K: 'AUXILIARY', V: 8 }
}
constantize(mainAuxiliaryTypeEnum)

// 结构类型
const structureTypeEnum = {
  WORKSHOP: { L: '厂房', K: 'WORKSHOP', V: 1 << 0 },
  FRAME: { L: '框架类', K: 'FRAME', V: 1 << 1 },
  SPACE: { L: '空间结构', K: 'SPACE', V: 1 << 2 },
  BEAM_TYPE: { L: '梁氏', K: 'BEAM_TYPE', V: 1 << 3 },
  ARCH_TYPE: { L: '拱式', K: 'ARCH_TYPE', V: 1 << 4 },
  STEEL_FRAME: { L: '钢架', K: 'STEEL_FRAME', V: 1 << 5 }
}
constantize(structureTypeEnum)

// 是否含有围护模块
const isEnclosureContainEnum = {
  YES: { L: '是', K: 'YES', V: 0 },
  NO: { L: '否', K: 'NO', V: 1 }
}
constantize(isEnclosureContainEnum)

export {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  businessTypeEnum, // 业务类型
  businessModifyTypeEnum, // 商务变更
  isTaxEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN, // 工程结算类型
  enclosureSettlementTypeEnum, // 围护结算类型
  transportModeEnum,
  auditTypeEnum, // 审核状态
  TechnologyTypeAllEnum,
  contractChangeTypeEnum,
  systemTypeEnum,
  contractDateTypeEnum,
  collectionDateTypeEnum,
  supplierPayMentTypeEnum,
  contractPayDateTypeEnum,
  contractReceiveDateTypeEnum,
  contractPayForEnum,
  supplierPayTypeEnum,
  contractReimbursementDateEnum,
  reimbursementTypeEnum,
  projectModeEnum,
  hasTaxEnum,
  hasPayEnum,
  purchaseOrderStatusEnum,
  logisticsSearchTypeEnum,
  payableSearchTypeEnum,
  TechnologyMainTypeEnum,
  pricingMannerEnum,
  subOrderSettleEnum,
  subOrderStatusEnum,
  orderSourceTypeEnum,
  depreciationTypeEnum,
  costTypeEnum,
  usedElectricityTypeEnum,
  gasTypeEnum,
  timeTypeEnum,
  managementSalaryTypeEnum,
  mainAuxiliaryTypeEnum,
  structureTypeEnum,
  isEnclosureContainEnum
}

export default {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  businessTypeEnum, // 业务类型
  businessModifyTypeEnum, // 商务变更
  isTaxEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN, // 工程结算类型
  enclosureSettlementTypeEnum, // 围护结算类型
  transportModeEnum,
  auditTypeEnum, // 审核状态
  TechnologyTypeAllEnum,
  contractChangeTypeEnum,
  systemTypeEnum,
  contractDateTypeEnum,
  collectionDateTypeEnum, // 收款日期
  supplierPayMentTypeEnum,
  contractPayDateTypeEnum,
  contractReceiveDateTypeEnum,
  contractPayForEnum,
  supplierPayTypeEnum,
  contractReimbursementDateEnum,
  reimbursementTypeEnum,
  projectModeEnum,
  hasTaxEnum,
  hasPayEnum,
  purchaseOrderStatusEnum,
  logisticsSearchTypeEnum,
  payableSearchTypeEnum,
  TechnologyMainTypeEnum,
  pricingMannerEnum,
  subOrderSettleEnum,
  subOrderStatusEnum,
  orderSourceTypeEnum,
  depreciationTypeEnum, // 折旧类型
  costTypeEnum, // 费用类型
  usedElectricityTypeEnum, // 用电类型
  gasTypeEnum, // 气体类型
  timeTypeEnum, // 时间类型
  managementSalaryTypeEnum, // 不同人员类型工资
  mainAuxiliaryTypeEnum,
  structureTypeEnum,
  isEnclosureContainEnum
}
