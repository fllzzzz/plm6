import request from '@/utils/request'

/**
 * @description: 生产统计
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics',
    method: 'get',
    params
  })
}

export default { get }
