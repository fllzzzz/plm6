import request from '@/utils/request'

/**
 * @description: 协同任务列表
 * @return {*}
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'assist',
    method: 'get',
    params
  })
}

/**
 * @description: 添加协同任务
 * @return {*}
 */
export function add(data) {
  return request({
    module: 'mes',
    url: 'assist',
    method: 'post',
    data
  })
}

/**
 * @description: 删除协同任务
 * @return {*}
 */
export function del(data) {
  return request({
    module: 'mes',
    url: 'assist',
    method: 'delete',
    data
  })
}

/**
 * @description: 可协同的生产线-班组列表
 * @return {*}
 */
export function teamList(params) {
  return request({
    module: 'mes',
    url: 'task/assist_task',
    method: 'get',
    params
  })
}

export default { get, add, del }
