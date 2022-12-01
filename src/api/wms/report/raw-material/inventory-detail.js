import request from '@/utils/request'

/**
 * 进销存记录
 */
export function get(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inventory-details`,
    method: 'get',
    params
  })
}

/**
 * 进销存详情
 * @param {*} {...row} 当前行的所有数据
 */
export function detail(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inventory-details/detail`,
    method: 'get',
    params
  })
}

/**
 * 进销存详情excel导出
 */
export function excel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inventory-details/detail/excel`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default { get, detail }
