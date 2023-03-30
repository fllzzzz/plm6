
import request from '@/utils/request'

/**
 * @description: 排产数据列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/artifact/scheduling/list',
    method: 'get',
    params
  })
}
/**
 * @description: 排产数据汇总
 */
export function getScheduleSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/artifact/scheduling/summary',
    method: 'get',
    params
  })
}

export default { get }

