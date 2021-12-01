import request from '@/utils/request'

/**
 * 钢材入库申请
 *
 * @export
 * @returns
 */
export function steelInboundApplication(data) {
  return request({
    module: 'wms',
    url: 'inbound/application/steel',
    method: 'post',
    data
  })
}

/**
 * 辅材入库申请
 *
 * @export
 * @returns
 */
export function auxMatInboundApplication(data) {
  return request({
    module: 'wms',
    url: 'inbound/application/auxiliary-material',
    method: 'post',
    data
  })
}

