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
