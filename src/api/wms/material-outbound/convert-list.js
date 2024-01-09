import request from '@/utils/request'

/**
 * 获取转换单列表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'convert/convert',
    method: 'get',
    params
  })
}

/**
 * 转换单详情
 */
export function getDetail(id) {
  return request({
    module: 'wms',
    url: `convert/convertDetail/${id}`,
    method: 'get'
  })
}

export default { get }
