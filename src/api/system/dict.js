import request from '@/utils/request'

/**
 * 获取字典
 *
 * @export
 * @param {*} name
 * @returns
 */

export function get(params) {
  return request({
    module: 'system',
    url: 'dict',
    method: 'get',
    params
  })
}

export default { get }
