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
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 0 },
  SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 1 << 1 },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 1 << 2 },
  SETTLED: { L: '已结算', K: 'COMPLETE', V: 1 << 3 }
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
  CARBARN: { L: '立体停车库', SL: '停车库', K: 'CARBARN', V: 1 << 2 }
}
constantize(projectTypeEnum)

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
  BENDING: { L: '折边件', K: 'BENDING', V: 1 << 5 }
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

// 结算状态
const settlementStatusEnum = {
  UNSETTLEMENT: { L: '未结算', K: 'THEORY', V: 0, COLOR: '#e64242' },
  SETTLED: { L: '已结算', K: 'OVERWEIGHT', V: 1, COLOR: '#13ce66' }
}
constantize(settlementStatusEnum)

// 付款方式
const paymentModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: 1 << 0 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: 1 << 1 },
  OTHER_TRANSFER: { L: '其他转账', K: 'OTHER_TRANSFER', V: 1 << 2 }
}
constantize(paymentModeEnum)

// 票据类型
const invoiceTypeEnum = {
  SPECIAL: { L: '增值税专用发票', SL: '专票', K: 'SPECIAL', V: 1 << 0 },
  ORDINARY: { L: '增值税普通发票', SL: '普票', K: 'ORDINARY', V: 1 << 1 },
  RECEIPT: { L: '收据', SL: '收据', K: 'RECEIPT', V: 1 << 2 }
}
constantize(invoiceTypeEnum)

// 是否含税
const isTaxEnum = {
  YES: { L: '是', K: 'YES', V: true },
  NO: { L: '否', K: 'NO', V: false }
}
constantize(isTaxEnum)

// 是否含税
const isTaxContractEnum = {
  YES: { L: '是', K: 'YES', V: 1 },
  NO: { L: '否', K: 'NO', V: 0 }
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
  CONTRACT_AMOUNT: { L: '变更合同金额', K: 'CONTRACT_AMOUNT', V: 1 << 1 },
  CONTRACT_SETTLE: { L: '项目结算', K: 'CONTRACT_SETTLE', V: 1 << 2 },
  VARIATION_ORDER: { L: '签证变更', K: 'VARIATION_ORDER', V: 1 << 3 }
}
constantize(contractChangeTypeEnum)

// 围护结算类型
const enclosureSettlementTypeEnum = {
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1 },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 2 }
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
const supplierPayModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: 1 << 0 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: 1 << 1 },
  ACCEPTANCE_DRAFT: { L: '承兑汇票', K: 'ACCEPTANCE_DRAFT', V: 1 << 2 },
  TRANSFER_CHECK: { L: '转账支票', K: 'TRANSFER_CHECK', V: 1 << 3 }

}
constantize(supplierPayModeEnum)

// TODO: 付款属性1
const supplierPayTypeEnum = {
  MATERIAL: { L: '原材料采购', K: 'MATERIAL', V: 1 },
  PRODUCT: { L: '制成品采购', K: 'PRODUCT', V: 2 },
  TRANSPORT: { L: '原材料运输', K: 'MATERIAL_TRANSPORT', V: 3 },
  SUBCONTRACT: { L: '专业分包', K: 'SUBCONTRACT', V: 4 }
}
constantize(supplierPayTypeEnum)

// TODO: 项目模式
const projectModeEnum = {
  STRUCTURE: { L: '构件', K: 'STRUCTURE', V: 1 << 0 },
  STRUCTURE_ASSEMBLE: { L: '构件&组立', K: 'STRUCTURE_ASSEMBLE', V: 1 << 1 },
  STRUCTURE_PART_ASSEMBLE: { L: '构件&零件&组立', K: 'MATERIAL_TRANSPORT', V: 1 << 2 }
}
constantize(projectModeEnum)

// TODO: 有无应付
const hasPayEnum = {
  YES: { L: '有应付', K: 'YES', V: 1 },
  NO: { L: '无应付', K: 'NO', V: 2 }
}
constantize(hasPayEnum)

// TODO: 有无欠税
const hasTaxEnum = {
  YES: { L: '有欠税', K: 'YES', V: 1 },
  NO: { L: '无欠税', K: 'NO', V: 2 }
}
constantize(hasTaxEnum)

// 采购订单状态
const purchaseOrderStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 },
  COMPLETE: { L: '已完成', K: 'COMPLETE', V: 2 }
}
constantize(purchaseOrderStatusEnum)

// 物流搜索类型
const logisticsSearchTypeEnum = {
  PRODUCT: { L: '制成品物流', K: 'PRODUCT', V: 1 << 0 },
  MATERIAL: { L: '原材料物流', K: 'MATERIAL', V: 1 << 1 },
  COMPANY: { L: '物流公司', K: 'COMPANY', V: 1 << 2 }
}
constantize(logisticsSearchTypeEnum)

export {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  businessTypeEnum, // 业务类型
  businessModifyTypeEnum, // 商务变更
  settlementStatusEnum, // 结算状态
  paymentModeEnum, // 付款方式
  invoiceTypeEnum,
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
  supplierPayModeEnum,
  supplierPayTypeEnum,
  contractReimbursementDateEnum,
  reimbursementTypeEnum,
  projectModeEnum,
  hasTaxEnum,
  hasPayEnum,
  purchaseOrderStatusEnum,
  logisticsSearchTypeEnum
}

export default {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  businessTypeEnum, // 业务类型
  businessModifyTypeEnum, // 商务变更
  settlementStatusEnum, // 结算状态
  paymentModeEnum, // 付款方式
  invoiceTypeEnum,
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
  supplierPayModeEnum,
  supplierPayTypeEnum,
  contractReimbursementDateEnum,
  reimbursementTypeEnum,
  projectModeEnum,
  hasTaxEnum,
  hasPayEnum,
  purchaseOrderStatusEnum,
  logisticsSearchTypeEnum
}
