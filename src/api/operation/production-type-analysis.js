import request from '@/utils/request'

/**
 * 构件类型
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getProductionTypeAnalysis(params) {
  return request({
    url: `/api/operational/analysis/production_type`,
    method: 'get',
    params: params
  })
}
