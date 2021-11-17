import request from '@/utils/request'

/**
 * @description: 围护可排产列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'enclosure/scheduling/page',
    method: 'get',
    params
  })
}

export default { get }
