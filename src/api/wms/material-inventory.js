import request from '@/utils/request'

/**
 * 钢板库存
 * @returns
 */
export function getSteelPlateInventory(data) {
  return request({
    module: 'wms',
    url: 'material-inventory/steel-plate',
    method: 'get',
    data
  })
}

/**
 * 型钢库存
 * @returns
 */
export function getSectionSteelInventory(data) {
  return request({
    module: 'wms',
    url: 'material-inventory/section-steel',
    method: 'get',
    data
  })
}

/**
 * 钢卷库存
 * @returns
 */
export function getSteelCoilInventory(data) {
  return request({
    module: 'wms',
    url: 'material-inventory/steel-coil',
    method: 'get',
    data
  })
}

/**
 * 辅材库存
 * @returns
 */
export function getAuxMatInventory(data) {
  return request({
    module: 'wms',
    url: 'material-inventory/auxiliary-material',
    method: 'get',
    data
  })
}

/**
 * 气体库存
 * @returns
 */
export function getGasInventory(data) {
  return request({
    module: 'wms',
    url: 'material-inventory/gas',
    method: 'get',
    data
  })
}
