import request from '@/utils/request'

/**
 * 调整记录
 */
export function get(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/supplement/details`,
    method: 'get',
    params
  })
}

/**
 * 调整d单详情
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `report/raw-materials/supplement/${id}`,
    method: 'get'
  })
}
