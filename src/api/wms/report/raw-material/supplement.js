import request from '@/utils/request'

/**
 * 红冲记录
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
 * 红冲d单详情
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `report/raw-materials/supplement/${id}`,
    method: 'get'
  })
}
