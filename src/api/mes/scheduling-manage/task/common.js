import request from '@/utils/request'

/**
 * @description: 获取排产详情
 * @param {*} params
 * @return {*}
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling',
    method: 'get',
    params
  })
}

/**
 * @description: 修改排产数
 * @param {*} data
 * @return {*}
 */
export function modifyQuantity(data) {
  return request({
    module: 'mes',
    url: 'scheduling/schedulingQuantity',
    method: 'put',
    data
  })
}

/**
 * @description: 任务下发
 * @param {*} data
 * @return {*}
 */
export function taskIssue(data) {
  return request({
    module: 'mes',
    url: 'task',
    method: 'post',
    data
  })
}

export default { get }
