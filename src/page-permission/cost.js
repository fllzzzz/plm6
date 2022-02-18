// 建刚mes
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 商务管理 start ------------------------------

// 商务管理/车次跟踪
export const tripTrackingPM = {
  get: ['cost_trip_tracking:get'], // 列表信息
  detail: ['cost_trip_tracking:detail'], // 查看详情
  print: ['cost_trip_tracking:print'] // 打印
}

// 商务管理/商务跟踪
export const businessTrackingPM = {
  get: ['cost_business_tracking:get'], // 列表信息
  summary: ['cost_business_tracking:summary'], // 汇总信息
  print: ['cost_business_tracking:print'] // 打印
}

// 商务管理/发运台账
export const shipmentLedgerPM = {
  get: ['cost_shipment_ledger:get'], // 列表信息
  print: ['cost_shipment_ledger:print'] // 打印
}
// --------------------------- 商务管理 end --------------------------------
