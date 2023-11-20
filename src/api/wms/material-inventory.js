import request from '@/utils/request'

/**
 * 钢板库存
 * @returns
 */
export function getSteelPlateInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/steel-plate',
    method: 'get',
    params
  })
}

/**
 * 型材库存
 * @returns
 */
export function getSectionSteelInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/section-steel',
    method: 'get',
    params
  })
}

/**
 * 钢卷库存
 * @returns
 */
export function getSteelCoilInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/steel-coil',
    method: 'get',
    params
  })
}

/**
 * 辅材库存
 * @returns
 */
export function getAuxMatInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/auxiliary-material',
    method: 'get',
    params
  })
}

/**
 * 其它库存
 * @returns
 */
export function getOtherInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/other-material',
    method: 'get',
    params
  })
}

/**
 * 气体库存
 * @returns
 */
export function getGasInventory(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/gas',
    method: 'get',
    params
  })
}

// 根据ids获取钢板库存
export function getStock(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/ids',
    method: 'get',
    params
  })
}
