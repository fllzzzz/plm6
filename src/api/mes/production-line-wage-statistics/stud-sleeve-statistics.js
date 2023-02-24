import request from '@/utils/request'

/**
 * @description: 栓钉套筒
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'price/stud/sleeve/page',
    method: 'get',
    params
  })
}

export default { get }
