import request from '@/utils/request'

/**
 * 物料仓-钢材库存
 * @param {*} params
 * @returns
 */
export function getSteelMaterialWarehouse(params) {
  return request({
    module: 'wms',
    url: `material-inventory/steel/${params.basicClass}`,
    method: 'get',
    params
  })
}
