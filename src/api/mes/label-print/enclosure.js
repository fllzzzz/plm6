import request from '@/utils/request'

/**
 * 围护标签列表
 */
export function get(params) {
  return request({
    url: '/api/mes/enclosure',
    method: 'get',
    params
  })
}

/**
 * 获取计划信息
 */
export function getPlanInfo(params) {
  return request({
    url: `/api/mes/enclosure/listByProjectId`,
    method: 'get',
    params
  })
}

/**
 * 获取任务产线
 */
export function getEnclosureLines(params) {
  return request({
    url: `/api/mes/enclosure/productionLine/hasTask`,
    method: 'get',
    params
  })
}

// 获取打印记录
export function getEnclosureRecord(params) {
  return request({
    url: `/api/mes/enclosure/print`,
    method: 'get',
    params
  })
}

// 新增打印记录
export function addEnclosureRecord({ taskId, quantity }) {
  return request({
    url: `/api/mes/enclosure/print`,
    method: 'post',
    data: { taskId, quantity }
  })
}

export default { get }
