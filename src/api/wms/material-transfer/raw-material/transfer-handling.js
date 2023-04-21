import request from '@/utils/request'

/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/steel-plate',
    method: 'put',
    data
  })
}

/**
 * 型材出库办理
 * @returns
 */
export function sectionSteelTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/section-steel',
    method: 'put',
    data
  })
}

/**
 * 钢卷出库办理
 * @returns
 */
export function steelCoilTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/steel-coil',
    method: 'put',
    data
  })
}

/**
 * 辅材出库办理
 * @returns
 */
export function auxMatTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/auxiliary-material',
    method: 'put',
    data
  })
}

/**
 * 其他调拨办理
 * @returns
 */
export function otherTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/other-material',
    method: 'put',
    data
  })
}

/**
 * 气体出库办理
 * @returns
 */
export function gasTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/gas',
    method: 'put',
    data
  })
}

// -----------------------------批量出库----------------------------
/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/steel-plate/batch',
    method: 'put',
    data
  })
}

/**
 * 型材出库办理
 * @returns
 */
export function sectionSteelBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/section-steel/batch',
    method: 'put',
    data
  })
}

/**
 * 钢卷出库办理
 * @returns
 */
export function steelCoilBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/steel-coil/batch',
    method: 'put',
    data
  })
}

/**
 * 辅材出库办理
 * @returns
 */
export function auxMatBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/auxiliary-material/batch',
    method: 'put',
    data
  })
}

/**
 * 其它调拨办理
 * @returns
 */
export function otherBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/other-material/batch',
    method: 'put',
    data
  })
}

/**
 * 气体出库办理
 * @returns
 */
export function gasBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'transfer/application/gas/batch',
    method: 'put',
    data
  })
}
