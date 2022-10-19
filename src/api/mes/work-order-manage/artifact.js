
import request from '@/utils/request'

/**
 * @description: 任务工单列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/order/page',
    method: 'get',
    params
  })
}

export default { get }

