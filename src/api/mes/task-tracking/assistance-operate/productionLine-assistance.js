import request from '@/utils/request'

/**
 * @description: 可变更的任务工单详情列表
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: `task/change/task_order/${params.taskOrderId}/list`,
    method: 'get',
    params
  })
}

/**
 * @description: 变更的任务工单
 */
export function save(data) {
  return request({
    module: 'mes',
    url: `task/change`,
    method: 'post',
    data
  })
}

/**
 * @description: 变更的切割任务
 */
export function saveNest(data) {
  return request({
    module: 'mes',
    url: `task/change/nest`,
    method: 'post',
    data
  })
}

/**
 * @description: 获取变更记录-分页
 */
export function record(params) {
  return request({
    module: 'mes',
    url: `task/change/page`,
    method: 'get',
    params
  })
}

/**
 * @description: 获取变更记录详情-列表
 */
export function recordDetail(params) {
  return request({
    module: 'mes',
    url: `task/change/${params.changeId}/details/list`,
    method: 'get',
    params
  })
}
