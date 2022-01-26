import request from '@/utils/request'

/**
 * @description: 零件可工单列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/machine_part/page',
    method: 'get',
    params
  })
}

export default { get }
