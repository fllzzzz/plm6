import { commonPM as SCM_COMMON_PM } from './supply-chain'

// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: SCM_COMMON_PM.purchaseOrderDetail,
  // 原材料-入库单详情：
  rawMatInboundReceiptDetail: ['wms_inbound_rawMaterial_review:detail'],
  // 原材料-出库单详情
  rawMatOutboundReceiptDetail: ['wms_outbound_rawMaterial_review:detail'],
  // 原材料-退库单详情
  rawMatReturnReceiptDetail: ['wms_return_rawMaterial_review:detail'],
  // 原材料-调拨详情
  rawMatTransferReceiptDetail: ['wms_transfer_rawMaterial_review:detail'],
  // 原材料-查看原材料冻结列表
  rawMatFreezeList: ['wms_rawMaterial_freeze_list:get']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 入库办理 start ------------------------------

// 入库办理/钢材入库办理
export const steelInboundApplicationPM = ['wms_steelInboundApplication:submit']

// 入库办理/辅材入库办理
export const auxMatInboundApplicationPM = ['wms_auxMatInboundApplication:submit']

// 入库办理/气体入库办理
export const gasInboundApplicationPM = ['wms_gasInboundApplication:submit']

// 入库办理/入库申请
export const rawMaterialInboundRecordPM = {
  get: ['wms_rawMaterial_inbound_record:get'],
  edit: ['wms_rawMaterial_inbound_record:edit'],
  del: ['wms_rawMaterial_inbound_record:del']
}

// 出入库审核/入库审核
export const rawMaterialInboundReviewPM = {
  get: ['wms_rawMaterial_inbound_review:get'],
  review: ['wms_rawMaterial_inbound_review:review']
}

// --------------------------- 入库办理 end --------------------------------

// ########################################################################

// --------------------------- 物料仓 start ------------------------------

// 物料仓/钢材仓库
export const steelMaterialWarehousePM = {
  get: ['wms_matWarehouse_steel:get'], // 查看 钢材仓库
  outbound: ['wms_matWarehouse_steel:outbound'], // 钢材出库
  transfer: ['wms_matWarehouse_steel:transfer'], // 钢材调拨
  freezeList: commonPM.rawMatFreezeList // 查看原材料冻结列表
}

// 物料仓/辅材仓库
export const auxMatMaterialWarehousePM = {
  get: ['wms_matWarehouse_auxMaterial:get'], // 查看 辅材仓库
  outbound: ['wms_matWarehouse_auxMaterial:outbound'], // 辅材出库
  transfer: ['wms_matWarehouse_auxMaterial:transfer'], // 辅材调拨
  freezeList: commonPM.rawMatFreezeList // 查看原材料冻结列表
}

// 物料仓/气体仓库
export const gasMaterialWarehousePM = {
  get: ['wms_matWarehouse_gas:get'], // 查看 气体仓库
  outbound: ['wms_matWarehouse_gas:outbound'], // 气体出库
  transfer: ['wms_matWarehouse_gas:transfer'], // 气体调拨
  freezeList: commonPM.rawMatFreezeList // 查看原材料冻结列表
}

//
export const permission = {
  get: ['wms_outboundApplication_record:get']
}

// --------------------------- 物料仓 end ---------------------------------

// ########################################################################

// --------------------------- 退货办理 start ------------------------------

// 退货办理/退货申请
export const rawMaterialRejectApplicationPM = {
  get: ['wms_rawMaterial_reject_application:get'], // 查看
  showAmount: ['wms_rawMaterial_reject_application:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单
}

// 退货办理/退货记录
export const rawMaterialRejectRecordPM = {
  get: ['wms_rawMaterial_reject_record:get'], // 查看
  del: ['wms_rawMaterial_reject_record:del'], // 删除
  showAmount: ['wms_rawMaterial_reject_record:showAmount'], // 显示金额
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单
}

// 出入库审核/退货审核
export const rawMaterialRejectReviewPM = {
  get: ['wms_rawMaterial_reject_review:get'], // 查看
  review: ['wms_rawMaterial_reject_review:review'], // 审核
  showAmount: ['wms_rawMaterial_reject_review:showAmount'], // 显示金额
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单
}

// --------------------------- 退货办理 end --------------------------------

// ########################################################################

// --------------------------- 报表中心 start ------------------------------

// 报表中心/原材料/归还甲方
export const operateRecordReturnToPartyAPM = {
  get: ['wms_rawMaterial_partyABorrow_return:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/原材料/甲供买入
export const operateRecordPartyABuyInPM = {
  get: ['wms_rawMaterial_partyABorrow_buyIn:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/原材料/入库单表
export const reportRawMaterialInboundReceiptPM = {
  get: ['wms_report_rawMaterial_inbound_receipt:get'], // 查看
  showAmount: ['wms_report_rawMaterial_inbound_receipt:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单详情
}

// 报表中心/原材料/入库单表
export const reportRawMaterialInboundDetailsPM = {
  get: ['wms_report_rawMaterial_inbound_details:get'], // 查看
  showAmount: ['wms_report_rawMaterial_inbound_details:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 采购订单详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail // 入库详情
}

// 报表中心/原材料/出库明细
export const reportRawMaterialOutboundDetailsPM = {
  get: ['wms_report_rawMaterial_outbound_details:get'], // 查看
  showAmount: ['wms_report_rawMaterial_outbound_details:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// 报表中心/原材料/退库明细
export const reportRawMaterialReturnDetailsPM = {
  get: ['wms_report_rawMaterial_return_details:get'], // 查看
  showAmount: ['wms_report_rawMaterial_return_details:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  returnReceiptDetail: commonPM.rawMatReturnReceiptDetail // 退库详情
}

// 报表中心/原材料/调拨明细
export const reportRawMaterialTransferDetailsPM = {
  get: ['wms_report_rawMaterial_transfer_details:get'], // 查看
  showAmount: ['wms_report_rawMaterial_transfer_details:showAmount'], // 显示金额
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 退库详情
}

// 报表中心/原材料/退货明细
export const reportRawMaterialRejectReceiptPM = {
  get: ['wms_report_rawMaterial_reject_details:get'], // 查看
  showAmount: ['wms_report_rawMaterial_reject_details:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 采购订单详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail // 入库详情
}

// 报表中心/原材料/收发存报表
export const reportRawMaterialSendAndReceiveStoragePM = {
  get: ['wms_report_rawMaterial_sendAndReceiveStorage:get'], // 查看
  showAmount: ['wms_report_rawMaterial_sendAndReceiveStorage:showAmount'] // 显示金额
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################
