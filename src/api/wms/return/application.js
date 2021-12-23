import request from '@/utils/request'

/**
 * 钢板入库申请
 *
 * @export
 * @returns
 */
export function steelPlateReturnApplication(data) {
  return request({
    module: 'wms',
    url: 'return/application/steel-plate',
    method: 'post',
    data
  })
}

/**
 * 型钢入库申请
 *
 * @export
 * @returns
 */
export function sectionSteelReturnApplication(data) {
  return request({
    module: 'wms',
    url: 'return/application/section-steel',
    method: 'post',
    data
  })
}
/**
 * 钢卷入库申请
 *
 * @export
 * @returns
 */
export function steelCoilReturnApplication(data) {
  return request({
    module: 'wms',
    url: 'return/application/steel-coil',
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
export function auxMatReturnApplication(data) {
  return request({
    module: 'wms',
    url: 'return/application/auxiliary-material',
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
export function gasReturnApplication(data) {
  return request({
    module: 'wms',
    url: 'return/application/gas',
    method: 'post',
    data
  })
}

