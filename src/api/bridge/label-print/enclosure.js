import request from '@/utils/request'

/**
 * 围护标签列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'print/enclosure',
    method: 'get',
    params
  })
}

export default { get }
