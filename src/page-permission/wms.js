// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: ['wms_purchaseOrder:detail'],
  // 原材料-入库单详情：
  rawMatInboundReceiptDetail: ['wms_inbound_rawMaterial_review:detail'],
  // 原材料-出库单详情
  rawMatOutboundReceiptDetail: ['wms_outbound_rawMaterial_review:detail'],
  // 原材料-退库单详情
  rawMatReturnReceiptDetail: ['wms_return_rawMaterial_review:detail'],
  // 原材料-调拨详情
  rawMatTransferReceiptDetail: ['wms_transfer_rawMaterial_review:detail']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 报表中心 start ------------------------------

// 报表中心/归还甲方
export const operateRecordReturnToPartyAPM = {
  get: ['wms_rawMaterial_partyABorrow_return:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/甲供买入
export const operateRecordPartyABuyInPM = {
  get: ['wms_rawMaterial_partyABorrow_buyIn:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/原材料-入库单表
export const reportRawMaterialInboundReceiptPM = {
  get: ['wms_inbound_rawMaterial_receipt_report:get'], // 查看
  showAmount: ['wms_inbound_rawMaterial_receipt_report:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单详情
}

// 报表中心/原材料-入库单表
export const reportRawMaterialInboundDetailsPM = {
  get: ['wms_inbound_rawMaterial_details_report:get'], // 查看
  showAmount: ['wms_inbound_rawMaterial_details_report:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 采购订单详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail // 入库详情
}

// 报表中心/原材料-出库明细
export const reportRawMaterialOutboundDetailsPM = {
  get: ['wms_outbound_rawMaterial_details_report:get'], // 查看
  showAmount: ['wms_outbound_rawMaterial_details_report:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// 报表中心/原材料-退库明细
export const reportRawMaterialReturnDetailsPM = {
  get: ['wms_return_rawMaterial_details_report:get'], // 查看
  showAmount: ['wms_return_rawMaterial_details_report:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  returnReceiptDetail: commonPM.rawMatReturnReceiptDetail // 退库详情
}

// 报表中心/原材料-调拨明细
export const reportRawMaterialTransferDetailsPM = {
  get: ['wms_transfer_rawMaterial_details_report:get'], // 查看
  showAmount: ['wms_transfer_rawMaterial_details_report:showAmount'], // 显示金额
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 退库详情
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################
