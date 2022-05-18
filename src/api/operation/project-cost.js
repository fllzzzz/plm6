import request from '@/utils/request'

/**
 * 项目直接成本
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getProjectCost(params) {
  return request({
    url: `/api/operational/analysis/cost`,
    method: 'get',
    params: params
  })
}
