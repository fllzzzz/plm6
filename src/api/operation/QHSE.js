import request from '@/utils/request'

/**
 * QHSE分析
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getQHSEAnalysis(params) {
  return request({
    url: `/api/operational/analysis/qhse`,
    method: 'get',
    params: params
  })
}

