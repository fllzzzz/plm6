import request from '@/utils/request'

/**
 * 入库单
 */
export function wmsRmInboundReceipt(id) {
  return request({
    url: `api/wms/inbound/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 出库单
 */
export function wmsRmOutboundReceipt(id) {
  return request({
    url: `api/wms/outbound/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 退库单
 */
export function wmsRmReturnReceipt(id) {
  return request({
    url: `api/wms/return/receipt/${id}/print`,
    method: 'get'
  })
}

export default {
  wmsRmInboundReceipt, // 入库单
  wmsRmOutboundReceipt, // 出库（领料）单
  wmsRmReturnReceipt // 退库单
}
