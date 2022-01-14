import request from '@/utils/request'

// 打包与发运
/**
 * 打包清单
 */
export function packingList(id) {
  return request({
    url: `api/mes/building/package/${id}/print`,
    method: 'get'
  })
}

/**
 * 发运汇总
 */
export function shipmentSummary(params) {
  return request({
    url: `api/mes/building/cargo/ship/print`,
    method: 'get',
    params
  })
}

/**
   * 发运详情
   */
export function shipmentDetail(id) {
  return request({
    url: `api/mes/building/cargo/ship/${id}/print`,
    method: 'get'
  })
}

/**
 * 收货状态汇总
 */
export function receiptStatusSummary(params) {
  return request({
    url: `api/mes/building/cargo/receipt/print`,
    method: 'get',
    params
  })
}

/**
 * 发运详情
 */
export function shippingList(id) {
  return request({
    url: `api/mes/building/cargo/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 物流汇总
 */
export function logisticsSummary(params) {
  return request({
    url: `api/mes/building/cargo/logistics/print`,
    method: 'get',
    params
  })
}

export default {
  // 打包与发运
  packingList, // 打包清单
  shipmentSummary, // 发运汇总
  shipmentDetail, // 发运详情
  receiptStatusSummary, // 收货状态汇总
  shippingList, // 发货清单
  logisticsSummary // 物流汇总
}

