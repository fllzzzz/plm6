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

/**
 * 退货单
 */
export function wmsRmRejectReceipt(id) {
  return request({
    url: `api/wms/reject/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 调拨单
 */
export function wmsRmTransferReceipt(id) {
  return request({
    url: `api/wms/transfer/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 红冲记录
 */
export function wmsRmSupplementReceipt(params) {
  return request({
    url: `api/wms/supplement/print`,
    method: 'get',
    params
  })
}

export default {
  wmsRmInboundReceipt, // 入库单
  wmsRmOutboundReceipt, // 出库（领料）单
  wmsRmReturnReceipt, // 退库单
  wmsRmRejectReceipt, // 退货单
  wmsRmTransferReceipt, // 调拨单
  wmsRmSupplementReceipt // 红冲记录
}
