import request from '@/utils/request'

/**
 * @description: 可变更的任务工单列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/change/task_order/page',
    method: 'get',
    params
  })
}

export default { get }
