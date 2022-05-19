import request from '@/utils/request'

/**
 * 应收款分析
 * @param {number} type
 * @returns
 */
export function getCollectionAnalysis(params) {
  return request({
    url: `/api/operational/analysis/collection`,
    method: 'get',
    params: params
  })
}

