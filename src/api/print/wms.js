import request from '@/utils/request'

// 打包与发运
/**
 * 打包清单
 */
export function wmsRmOutboundReceipt(id) {
  return request({
    url: `api/wms/outbound/receipt/${id}/print`,
    method: 'get'
  })
}

export default {
  wmsRmOutboundReceipt // 出库（领料）单
}
