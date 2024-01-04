import request from '@/utils/request'

/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/steel-plate',
    method: 'put',
    data
  })
}

/**
 * 型材出库办理
 * @returns
 */
export function sectionSteelOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/section-steel',
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
    url: 'outbound/application/steel-coil',
    method: 'put',
    data
  })
}

/**
 * 钢卷条板转换
 * @returns
 */
export function steelCoilConvertHandling(data) {
  return request({
    module: 'wms',
    url: 'convert/saveConvert',
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
    url: 'outbound/application/auxiliary-material',
    method: 'put',
    data
  })
}

/**
 * 其它出库办理
 * @returns
 */
export function otherOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/other-material',
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
    url: 'outbound/application/gas',
    method: 'put',
    data
  })
}

// -----------------------------批量出库----------------------------
/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/steel-plate/batch',
    method: 'put',
    data
  })
}

/**
 * 型材出库办理
 * @returns
 */
export function sectionSteelBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/section-steel/batch',
    method: 'put',
    data
  })
}

/**
 * 钢卷出库办理
 * @returns
 */
export function steelCoilBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/steel-coil/batch',
    method: 'put',
    data
  })
}

/**
 * 辅材出库办理
 * @returns
 */
export function auxMatBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/auxiliary-material/batch',
    method: 'put',
    data
  })
}

/**
 * 其它出库办理
 * @returns
 */
export function otherBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/other-material/batch',
    method: 'put',
    data
  })
}

/**
 * 气体出库办理
 * @returns
 */
export function gasBatchOutboundHandling(data) {
  return request({
    module: 'wms',
    url: 'outbound/application/gas/batch',
    method: 'put',
    data
  })
}
