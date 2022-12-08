import request from '@/utils/request'

/**
 * @description: 工序协同列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'assist/top_task_order/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取可以班组协同的任务工单
 */
export function getTask({ topTaskOrderId }) {
  return request({
    module: 'bridge',
    url: `assist/${topTaskOrderId}/task_order/list`,
    method: 'get'
  })
}

/**
 * @description: 获取可以班组协同的产品
 */
export function detail(params) {
  return request({
    module: 'bridge',
    url: `assist/task_order/product/list`,
    method: 'get',
    params
  })
}

/**
 * @description: 添加班组协同
 */
export function save(data) {
  return request({
    module: 'bridge',
    url: `assist`,
    method: 'post',
    data
  })
}

/**
 * @description: 删除班组协同
 */
export function del(data) {
  return request({
    module: 'bridge',
    url: `assist`,
    method: 'delete',
    data
  })
}

export default { get }
