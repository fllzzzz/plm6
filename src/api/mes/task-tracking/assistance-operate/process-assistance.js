import request from '@/utils/request'

/**
 * @description: 工序协同列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'assist/top_task_order/page',
    method: 'get',
    params
  })
}

export default { get }
