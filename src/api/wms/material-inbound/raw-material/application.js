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

/**
 * 其它入库申请
 *
 * @export
 * @returns
 */
export function otherInboundApplication(data) {
  return request({
    module: 'wms',
    url: 'inbound/application/other-material',
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
export function gasInboundApplication(data) {
  return request({
    module: 'wms',
    url: 'inbound/application/gas',
    method: 'post',
    data
  })
}

