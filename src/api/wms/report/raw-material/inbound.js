import request from '@/utils/request'

/**
 * 获取原材料入库单列表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'report/raw-materials/inbound/receipt',
    method: 'get',
    params
  })
}

/**
 * 入库单详情（含退货记录）
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inbound/receipt/${id}`,
    method: 'get'
  })
}

export default { get, detail }
