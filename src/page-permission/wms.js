// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: ['wms_purchaseOrder:detail'],
  // 入库单详情：
  inboundDetail: ['wms_inbound_rawMaterial_review:detail'],
  // 调拨详情
  transferDetail: ['wms_transfer_rawMaterial_review:detail']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 报表中心 start ------------------------------

// 报表中心/归还甲方
export const operateRecordReturnToPartyAPM = {
  get: ['wms_rawMaterial_partyABorrow_return:get'], // 查看
  transferDetail: commonPM.transferDetail // 调拨详情
}

// 报表中心/甲供买入
export const operateRecordPartyABuyInPM = {
  get: ['wms_rawMaterial_partyABorrow_buyIn:get'], // 查看
  transferDetail: commonPM.transferDetail // 调拨详情
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
  inboundDetail: commonPM.inboundDetail // 入库详情
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################
