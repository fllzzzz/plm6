import request from '@/utils/request'

/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'material-outbound/steel-plate',
    method: 'put',
    data
  })
}

/**
 * 型钢出库办理
 * @returns
 */
export function sectionSteelOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'material-outbound/section-steel',
    method: 'put',
    data
  })
}

/**
 * 钢卷出库办理
 * @returns
 */
export function steelCoilOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'material-outbound/steel-coil',
    method: 'put',
    data
  })
}

/**
 * 辅材出库办理
 * @returns
 */
export function auxMatOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'material-outbound/auxiliary-material',
    method: 'put',
    data
  })
}

/**
 * 气体出库办理
 * @returns
 */
export function gasOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'material-outbound/gas',
    method: 'put',
    data
  })
}
