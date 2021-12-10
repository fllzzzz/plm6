import request from '@/utils/request'

/**
 *
 * 获取项目报表
 */
export function artifact(params) {
  return request({
    module: 'mes',
    url: 'kanban/form/artifact/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取项目报表
 */
export function enclosure(params) {
  return request({
    module: 'mes',
    url: 'kanban/form/enclosure/page',
    method: 'get',
    params
  })
}
