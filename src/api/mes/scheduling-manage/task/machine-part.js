import request from '@/utils/request'

/**
 * @description:零件工单汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/machine_part/summary',
    method: 'get',
    params
  })
}

export default { get }
