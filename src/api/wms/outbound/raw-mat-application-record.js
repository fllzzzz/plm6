import request from '@/utils/request'

/**
 * 获取出库记录列表
 * @returns
 */
export function get() {
  return request({
    module: 'wms',
    url: 'outbound/application/record/raw-materials',
    method: 'get'
  })
}

/**
 * 获取出库清单详情
 * @returns
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `outbound/application/record/raw-materials/${id}`,
    method: 'get'
  })
}

export default { get, detail }
