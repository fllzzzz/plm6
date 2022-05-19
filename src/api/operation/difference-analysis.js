import request from '@/utils/request'

/**
 * 差异分析
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getDiffAnalysis(params) {
  return request({
    url: `/api/operational/analysis/difference`,
    method: 'get',
    params: params
  })
}

/**
 * 差异率
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getDiffAverageAnalysis(params) {
  return request({
    url: `/api/operational/analysis/difference/average`,
    method: 'get',
    params: params
  })
}
