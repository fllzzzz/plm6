import request from '@/utils/request'

/**
 * @description:围护工单汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/enclosure/summary',
    method: 'get',
    params
  })
}

export default { get }
