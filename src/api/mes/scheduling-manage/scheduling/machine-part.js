import request from '@/utils/request'

/**
 * @description: 零件可排产列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/page',
    method: 'get',
    params
  })
}

export default { get }
