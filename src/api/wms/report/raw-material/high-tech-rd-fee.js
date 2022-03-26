import request from '@/utils/request'

/**
 * 获取高新研发费列表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'report/raw-materials/high-tech-rd-fee',
    method: 'get',
    params
  })
}

export default { get }
