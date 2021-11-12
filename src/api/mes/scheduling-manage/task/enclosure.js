import request from '@/utils/request'

/**
 * @description:围护排产汇总
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
