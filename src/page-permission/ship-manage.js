// --------------------------- 发运管理 start --------------------------------

// 发运管理/建钢-发运统计
export const mesShipSummaryPM = {
  get: ['mes_ship_summary:get'], // 列表
  detail: ['mes_ship_summary:detail'], // 详情
  print: ['mes_ship_summary:print'] // 发运详情打印
}
// 发运管理/围护-发运统计
export const enclosureShipSummaryPM = {
  get: ['enclosure_ship_summary:get'], // 列表
  detail: ['enclosure_ship_summary:detail'], // 详情
  print: ['enclosure_ship_summary:print'] // 发运详情打印
}

// 发运管理/手工打包:手工打包
export const manualPackPM = {
  pack: ['mes_manual_pack:pack'] // 手工打包
}

// 发运管理/手工打包:结构
export const artifactManualPackPM = {
  get: ['mes_manual_pack_artifact:get'], // 结构打包列表
  pack: manualPackPM.pack // 手工打包
}

// 发运管理/手工打包:围护
export const enclosureManualPackPM = {
  get: ['mes_manual_pack_enclosure:get'], // 围护打包列表
  pack: manualPackPM.pack // 手工打包
}

// 发运管理/打包记录
export const mesPackPM = {
  get: ['mes_pack:get'], // 打包列表
  detail: ['mes_pack:detail'], // 查看打包清单
  // pack: manualPackPM.pack, // 手工打包
  edit: manualPackPM.pack, // 编辑打包清单
  del: ['mes_pack:del'], // 删除打包清单
  print: ['mes_pack:print'], // 查看及打印标签
  printRecords: ['mes_pack:printRecords'], // 查看打印记录
  printPackList: ['mes_pack:printPackList'] // 打印包单清单
}

// 发运管理/发运记录
export const mesShipPM = {
  get: ['mes_ship:get'], // 发运列表
  detail: ['mes_ship:detail'], // 查看车次详情
  print: ['mes_ship:print'], // 打印车次汇总
  detailPrint: ['mes_ship:detailPrint'] // 打印车次详情
}

// 发运管理/收货状态
export const receiptStatusPM = {
  get: ['mes_receipt_status:get'], // 收货列表
  detail: ['mes_receipt_status:detail'], // 查看收货详情
  print: ['mes_receipt_status:print'], // 打印收货汇总
  detailPrint: ['mes_receipt_status:detailPrint'], // 打印收货详情
  cancelDelivery: ['mes_receipt_status:cancelDelivery'], // 取消发运
  confirmDelivery: ['mes_receipt_status:confirmDelivery'] // 确定签收
}

// 发运管理/物流记录
export const logisticsPM = {
  get: ['mes_logistics:get'], // 物流列表
  edit: ['mes_logistics:edit'], // 录入物流信息
  print: ['mes_logistics:print'] // 打印物流汇总
}

// 发运管理/发运审核
export const shipAuditPM = {
  get: ['mes_ship_audit:get'], // 审核列表
  detail: ['mes_ship_audit:detail'], // 装车明细
  print: ['mes_ship_audit:print'], // 打印发运审核
  download: ['mes_ship_audit:download'], // 下载发运详情
  audit: ['mes_ship_audit:audit'] // 发运审核
}

// 发运管理/结构制成品入发存
export const mesProductSendReceiveStoragePM = {
  get: ['mes_product-send-receive-storage:get'], // 列表
  print: ['mes_product-send-receive-storage:print'], // 制成品入发存打印
  detail: ['mes_product-send-receive-storage:detail'], // 制成品入发存详情
  detailPrint: ['mes_product-send-receive-storage:detailPrint'] // 制成品入发存详情打印
}
// 发运管理/围护制成品入发存
export const enclosureProductSendReceiveStoragePM = {
  get: ['enclosure_product-send-receive-storage:get'], // 列表
  print: ['enclosure_product-send-receive-storage:print'], // 制成品入发存打印
  detail: ['enclosure_product-send-receive-storage:detail'], // 制成品入发存详情
  detailPrint: ['enclosure_product-send-receive-storage:detailPrint'] // 制成品入发存详情打印
}
// --------------------------- 发运管理 end --------------------------------
