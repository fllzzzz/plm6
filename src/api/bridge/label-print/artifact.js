import request from '@/utils/request'

/**
 * 分段标签列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'print/box',
    method: 'get',
    params
  })
}

export default { get }
