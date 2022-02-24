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
