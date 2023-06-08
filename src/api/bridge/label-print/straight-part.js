import request from '@/utils/request'

/**
 * 直发件标签列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'print/part',
    method: 'get',
    params
  })
}

export default { get }
