import request from '@/utils/request'

/**
 * 钢板出库办理
 * @returns
 */
export function steelPlateTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'material-transfer/steel-plate',
    method: 'put',
    data
  })
}

/**
 * 型钢出库办理
 * @returns
 */
export function sectionSteelTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'material-transfer/section-steel',
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
    url: 'material-transfer/steel-coil',
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
    url: 'material-transfer/auxiliary-material',
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
    url: 'material-transfer/gas',
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
    url: 'material-transfer/steel-plate/batch',
    method: 'put',
    data
  })
}

/**
 * 型钢出库办理
 * @returns
 */
export function sectionSteelBatchTransferHandling(data) {
  return request({
    module: 'wms',
    url: 'material-transfer/section-steel/batch',
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
    url: 'material-transfer/steel-coil/batch',
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
    url: 'material-transfer/auxiliary-material/batch',
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
    url: 'material-transfer/gas/batch',
    method: 'put',
    data
  })
}
