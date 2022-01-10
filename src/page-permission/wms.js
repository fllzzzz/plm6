// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
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

// 报表中心/原材料-入库报表
export const reportRawMaterialInboundPM = {
  get: ['wms_inbound_rawMaterial_report:get'], // 查看
  showAmount: ['wms_inbound_rawMaterial_report:showAmount'] // 显示金额
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################

