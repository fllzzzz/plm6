import request from '@/utils/request'

/**
 *
 * 获取项目报表
 */
export function artifact(params) {
  return request({
    module: 'mes',
    url: 'kanban/peoject/artifact/summary',
    method: 'get',
    params
  })
}
