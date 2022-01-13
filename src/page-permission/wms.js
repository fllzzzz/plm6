// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: ['wms_purchaseOrder:detail'],
  // 原材料-入库单详情：
  rawMatInboundDetail: ['wms_inbound_rawMaterial_review:detail'],
  // 原材料-出库单详情
  rawMatOutboundReceiptDetail: ['wms_outbound_rawMaterial_review:detail'],
  // 原材料-调拨详情
  rawMatTransferDetail: ['wms_transfer_rawMaterial_review:detail']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 报表中心 start ------------------------------

// 报表中心/归还甲方
export const operateRecordReturnToPartyAPM = {
  get: ['wms_rawMaterial_partyABorrow_return:get'], // 查看
  transferDetail: commonPM.rawMatTransferDetail // 调拨详情
}

// 报表中心/甲供买入
export const operateRecordPartyABuyInPM = {
  get: ['wms_rawMaterial_partyABorrow_buyIn:get'], // 查看
  transferDetail: commonPM.rawMatTransferDetail // 调拨详情
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
  inboundDetail: commonPM.rawMatInboundDetail // 入库详情
}

// 报表中心/原材料-出库明细
export const reportRawMaterialOutboundDetailsPM = {
  get: ['wms_outbound_rawMaterial_details_report:get'], // 查看
  showAmount: ['wms_outbound_rawMaterial_details_report:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################
