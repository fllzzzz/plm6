import { constantize } from '../base'

// 入库填写方式（金额及工厂在什么阶段填写）
const inboundFillWayEnum = {
  APPLICATION: { L: '入库提交时显示', K: 'APPLICATION', V: 1 << 0 },
  REVIEWING: { L: '入库审核时显示', K: 'REVIEWING', V: 1 << 1 }
}
constantize(inboundFillWayEnum)

// 物料加权方式
const materialWeightingWayEnum = {
  WHOLE: { L: '全库加权', K: 'WHOLE', V: 1 << 0 },
  SINGLE: { L: '单库加权', K: 'SINGLE', V: 1 << 1 }
}
constantize(materialWeightingWayEnum)

// 计量配置
const measureTypeEnum = {
  MEASURE: { L: '计量', K: 'MEASURE', V: 1 },
  ACCOUNTING: { L: '核算', K: 'ACCOUNTING', V: 2 }
}
constantize(measureTypeEnum)

// 退库状态
const returnStatusEnum = {
  TRUE: { L: '真实退库', K: 'TRUE', V: true },
  FALSE: { L: '虚拟退库', K: 'FALSE', V: false }
}
constantize(returnStatusEnum)

// 单位类型(报表查询)
const unitTypeEnum = {
  MEASURE: { L: '计量单位', K: 'MEASURE', V: measureTypeEnum.MEASURE.V },
  ACCOUNTING: { L: '核算单位', K: 'ACCOUNTING', V: measureTypeEnum.ACCOUNTING.V },
  OUTBOUND: { L: '出库单位', K: 'OUTBOUND', V: 3 }
}
constantize(unitTypeEnum)

// 项目仓库类型
const projectWarehouseTypeEnum = {
  PUBLIC: { L: '公共库', K: 'COMMON', V: 1 << 0 },
  PROJECT: { L: '项目库', K: 'PROJECT', V: 1 << 1 }
}
constantize(projectWarehouseTypeEnum)

// 物料类型（整料|余料）
const materialIsWholeEnum = {
  WHOLE: { L: '整料', K: 'WHOLE ', V: 1 << 0, COLOR: '#3a8ee6' },
  ODDMENT: { L: '余料', K: 'ODDMENT', V: 1 << 1, COLOR: '#e6a23c' }
}
constantize(materialIsWholeEnum)

// 物料出库方式（整出， 半出）
const materialOutboundModeEnum = {
  WHOLE: { L: '整出', K: 'WHOLE ', V: 1 << 0, COLOR: '#3a8ee6' },
  HALF: { L: '半出', K: 'HALF', V: 1 << 1, COLOR: '#e6a23c' }
}
constantize(materialOutboundModeEnum)

// 钢板半出方式
const steelPlateHalfModeEnum = {
  LENGTH: { L: '取长', K: 'LENGTH ', V: 1 << 0 },
  WIDTH: { L: '取宽', K: 'WIDTH', V: 1 << 1 }
}
constantize(steelPlateHalfModeEnum)

// 订单供货类型
const orderSupplyTypeEnum = {
  SELF: { L: '自采物料', SL: '自采', K: 'SELF', V: 1 << 0 },
  PARTY_A: { L: '甲供物料', SL: '甲供', K: 'PARTY_A', V: 1 << 1 }
}
constantize(orderSupplyTypeEnum)

// 甲供材料调拨方式
const partyAMatTransferEnum = {
  BORROW: { L: '借用', K: 'BORROW', V: 1 << 0, COLOR: '#ffa216' },
  BUY_IN: { L: '买入', K: 'BUY_IN', V: 1 << 1, COLOR: '#0f9747' }
}
constantize(partyAMatTransferEnum)

// 普通调拨类型
const transferNormalTypeEnum = {
  PUBLIC_WARE: { L: '公共库', K: 'PUBLIC_WARE', V: 1 << 0 },
  PROJECT_WARE: { L: '项目库', K: 'PROJECT_WARE', V: 1 << 1 },
  RETURN_PARTY_A: { L: '归还甲方', K: 'RETURN_PARTY_A', V: 1 << 2 }
}
constantize(transferNormalTypeEnum)

// 调拨类型
const transferTypeEnum = {
  PUBLIC_WARE: { L: '移至公共库', K: 'PUBLIC_WARE', V: 1 << 0 },
  PROJECT_WARE: { L: '移至项目库', K: 'PROJECT_WARE', V: 1 << 1 },
  RETURN_PARTY_A: { L: '归还甲方', K: 'RETURN_PARTY_A', V: 1 << 2 },
  BORROW_RETURN: { L: '借用归还', K: 'BORROW_RETURN', V: 1 << 3 }
}
constantize(transferTypeEnum)

// 调拨创建方式
const transferCreateTypeEnum = {
  NORMAL: { L: '手动调拨', K: 'NORMAL', V: 1 << 0 },
  OUTBOUND: { L: '出库调拨', K: 'OUTBOUND', V: 1 << 1 }
}
constantize(transferCreateTypeEnum)

// 出库关联类型
const outboundRelationTypeEnum = {
  ROUTINE: { L: '常规', K: 'ROUTINE', V: 1 << 0 },
  TRANSFER: { L: '调拨', K: 'TRANSFER', V: 1 << 1 },
  CUT: { L: '切割', K: 'CUT', V: 1 << 2 },
  CUT_TRANSFER: { L: '切割调拨', K: 'CUT_TRANSFER', V: 1 << 3 }
}
constantize(outboundRelationTypeEnum)

// 借用归还状态
const borrowReturnStatusEnum = {
  NOT_RETURNED: { L: '未归还完', K: 'SELF', V: 1 << 0 },
  // PENDING_REVIEW: { L: '待审核', K: 'PENDING_REVIEW', V: 1 << 1, TAG: 'warning' },
  RETURNED: { L: '已归还', K: 'RETURNED', V: 1 << 1, TAG: 'success' }
}
constantize(borrowReturnStatusEnum)

// 备料类型
const preparationTypeEnum = {
  PROJECT: { L: '项目备料', K: 'PROJECT', V: 1 << 0 },
  // PARTY_A: { L: '项目甲供备料', K: 'PARTY_A', V: 1 << 1 },
  PUBLIC: { L: '公共备料', K: 'PROJECT', V: 1 << 2 }
  // MANUFACTURED: { L: '外协加工', K: 'PROJECT', V: 1 << 3 }
}
constantize(preparationTypeEnum)

// 材料申购方式
const requisitionModeEnum = {
  PURCHASE: { L: '采购', K: 'PURCHASE', V: 1 << 0 },
  USE_INVENTORY: { L: '利用库存', K: 'USE_INVENTORY', V: 1 << 1 }
}
constantize(requisitionModeEnum)

// 基础材料类型/采购类型
const baseMaterialTypeEnum = {
  RAW_MATERIAL: { L: '原材料', K: 'RAW_MATERIAL', V: 1 << 0 },
  MANUFACTURED: { L: '制成品', K: 'MANUFACTURED', V: 1 << 1 }
}
constantize(baseMaterialTypeEnum)

// 单据类型
const receiptTypeEnum = {
  PREPARATION: { L: '备料', K: 'PREPARATION', V: 1, DOC: '备料单' },
  REQUISITIONS: { L: '申购', K: 'REQUISITIONS', V: 2, DOC: '申购单' },
  PURCHASE: { L: '采购', K: 'PURCHASE', V: 11, DOC: '采购合同' },
  INBOUND: { L: '入库', K: 'INBOUND', V: 12, DOC: '入库单' },
  OUTBOUND_APPLY: { L: '出库申请', K: 'OUTBOUND_APPLY', V: 13, DOC: '出库申请单' },
  OUTBOUND: { L: '出库', K: 'OUTBOUND', V: 14, DOC: '出库单' },
  TRANSFER: { L: '调拨', K: 'TRANSFER', V: 15, DOC: '调拨单' },
  RETURN: { L: '退库', K: 'RETURN', V: 16, DOC: '退库单' },
  REJECTED: { L: '退货', K: 'REJECTED', V: 17, DOC: '退货单' },
  SUPPLEMENT: { L: '调整', K: 'SUPPLEMENT', V: 25, DOC: '调整单' },
  CUT_SURPLUS: { L: '切割余料', K: 'CUT_SURPLUS', V: 26, DOC: '切割余料' }
}
constantize(receiptTypeEnum)

// 冻结类型
const materialFreezeTypeEnum = {
  PREPARATION: receiptTypeEnum.PREPARATION,
  REQUISITIONS: receiptTypeEnum.REQUISITIONS,
  OUTBOUND_APPLY: { L: '出库', K: 'OUTBOUND_APPLY', V: receiptTypeEnum.OUTBOUND_APPLY.V, DOC: '出库申请单' },
  TRANSFER: receiptTypeEnum.TRANSFER,
  REJECTED: receiptTypeEnum.REJECTED,
  NESTING: { L: '套料', K: 'NESTING', V: 18, DOC: '套料工单' }
}
constantize(materialFreezeTypeEnum)

// 物料打印标签类型
const materialLabelPrintTypeEnum = {
  INBOUND: { L: '入库', K: 'INBOUND', V: 1 << 0, DOC: '申购单' },
  HALF_OUTBOUND: { L: '半出', K: 'HALF_OUTBOUND', V: 1 << 1, DOC: '出库单' },
  TRANSFER: { L: '调拨', K: 'TRANSFER', V: 1 << 2, DOC: '调拨单' },
  RETURN: { L: '退库', K: 'RETURN', V: 1 << 3, DOC: '退库单' }
}
constantize(materialLabelPrintTypeEnum)

// 提货方式
const pickUpModeEnum = {
  SELF: { L: '自提', K: 'SELF', V: 1 << 0 },
  SUPPLIER: { L: '到厂', K: 'SUPPLIER', V: 1 << 1 }
}
constantize(pickUpModeEnum)

// 采购合同付款付款方式
const purchaseOrderPaymentModeEnum = {
  FUTURES: { L: '期货订单', K: 'FUTURES', V: 1 << 0 },
  STOCK: { L: '现款订单', K: 'STOCK', V: 1 << 1 },
  ARRIVAL: { L: '到货订单', K: 'ARRIVAL', V: 1 << 2 }
}
constantize(purchaseOrderPaymentModeEnum)

// 采购状态(订单采购状态/申购单采购状态)
const purchaseStatusEnum = {
  UNFINISHED: { L: '采购中', K: 'UNFINISHED', V: 1 },
  FINISHED: { L: '已完成', K: 'FINISHED', V: 2 }
}
constantize(purchaseStatusEnum)

// (申购单采购状态)
const requisitionStatusEnum = {
  NOT_STARTED: { L: '未开始', K: 'NOT_STARTED', V: 1 << 0, T: '' },
  PARTIALLY_COMPLETED: { L: '部分采购', K: 'PARTIALLY_COMPLETED', V: 1 << 1, T: 'warning' },
  COMPLETED: { L: '已采购', K: 'COMPLETED', V: 1 << 2, T: 'success' }
}
constantize(requisitionStatusEnum)

// 物料退货状态
const materialRejectStatusEnum = {
  ALL: { L: '全部退货', K: 'ALL', V: 1 << 3, COLOR: '#f56c6c' },
  PART: { L: '部分退货', K: 'PART', V: 1 << 2, COLOR: '#f7b551' },
  PENDING_REVIEW: { L: '退货待审', K: 'PENDING_REVIEW', V: 1 << 1, COLOR: '#409eff' },
  NONE: { L: '无退货', K: 'NONE', V: 1 << 0, COLOR: '#17db9b' }
}
constantize(materialRejectStatusEnum)

// 入库单：单据退货状态
const receiptRejectStatusEnum = {
  ALL: { L: '全部退货', K: 'ALL', V: 1 << 3, COLOR: '#f56c6c' },
  PART: { L: '部分退货', K: 'PART', V: 1 << 2, COLOR: '#f7b551' },
  PENDING_REVIEW: { L: '退货待审', K: 'PENDING_REVIEW', V: 1 << 1, COLOR: '#409eff' },
  NONE: { L: '无退货', K: 'NONE', V: 1 << 0, COLOR: '#17db9b' }
}
constantize(receiptRejectStatusEnum)

// 质检状态
const inspectionStatusEnum = {
  UNREVIEWED: { L: '待质检', SL: '待质检', K: 'UNREVIEWED', V: 1 << 0, TAG: '' },
  PART_PASS: { L: '部分质检通过', SL: '部分通过', K: 'PART_PASS', V: 1 << 2, TAG: 'warning' },
  ALL_PASS: { L: '全部质检通过', SL: '全部通过', K: 'ALL_PASS', V: 1 << 1, TAG: 'success' },
  ALL_REFUSE: { L: '全部退回', SL: '全部退回', K: 'ALL_REFUSE', V: 1 << 3, TAG: 'danger' },
  NO: { L: '无质检', SL: '无质检', K: 'NO', V: 1 << 4, TAG: 'info' }
}
constantize(inspectionStatusEnum)

// 质检详情状态
const inspectionDetailStatusEnum = {
  UNREVIEWED: { L: '待质检', K: 'UNREVIEWED', V: inspectionStatusEnum.UNREVIEWED.V, TAG: '' },
  PASS: { L: '通过', K: 'PASS', V: inspectionStatusEnum.ALL_PASS.V, TAG: 'success' },
  REFUSE: { L: '退回', K: 'REFUSE', V: inspectionStatusEnum.ALL_REFUSE.V, TAG: 'danger' }
}
constantize(inspectionDetailStatusEnum)

// 配置仓库类型
const warehouseTypeEnum = {
  WORKSHOP: { L: '车间', K: 'WORKSHOP', V: 1 << 0 },
  NORMAL: { L: '普通', K: 'NORMAL', V: 1 << 1 }
}
constantize(warehouseTypeEnum)

export {
  receiptTypeEnum, // 单据类型
  returnStatusEnum,
  inboundFillWayEnum,
  measureTypeEnum,
  unitTypeEnum,
  projectWarehouseTypeEnum,
  materialIsWholeEnum,
  materialOutboundModeEnum,
  steelPlateHalfModeEnum,
  orderSupplyTypeEnum,
  partyAMatTransferEnum,
  borrowReturnStatusEnum,
  outboundRelationTypeEnum,
  preparationTypeEnum,
  requisitionModeEnum,
  baseMaterialTypeEnum,
  purchaseStatusEnum,
  requisitionStatusEnum,
  purchaseOrderPaymentModeEnum,
  materialFreezeTypeEnum,
  pickUpModeEnum,
  transferNormalTypeEnum,
  transferTypeEnum,
  transferCreateTypeEnum,
  materialRejectStatusEnum,
  receiptRejectStatusEnum,
  inspectionStatusEnum,
  inspectionDetailStatusEnum,
  materialWeightingWayEnum,
  materialLabelPrintTypeEnum,
  warehouseTypeEnum
}

export default {
  receiptTypeEnum, // 单据类型
  returnStatusEnum,
  inboundFillWayEnum,
  measureTypeEnum,
  unitTypeEnum,
  projectWarehouseTypeEnum,
  materialIsWholeEnum,
  materialOutboundModeEnum,
  steelPlateHalfModeEnum,
  orderSupplyTypeEnum,
  partyAMatTransferEnum,
  borrowReturnStatusEnum,
  outboundRelationTypeEnum,
  preparationTypeEnum,
  requisitionModeEnum,
  baseMaterialTypeEnum,
  purchaseStatusEnum,
  requisitionStatusEnum,
  purchaseOrderPaymentModeEnum,
  materialFreezeTypeEnum,
  pickUpModeEnum,
  transferNormalTypeEnum,
  transferTypeEnum,
  transferCreateTypeEnum,
  materialRejectStatusEnum,
  receiptRejectStatusEnum,
  inspectionStatusEnum,
  inspectionDetailStatusEnum,
  materialWeightingWayEnum,
  materialLabelPrintTypeEnum,
  warehouseTypeEnum
}
