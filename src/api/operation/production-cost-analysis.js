import request from '@/utils/request'

/**
 * 生产成本分析
 */
export function getProductAnalysis(params) {
  return request({
    url: `/api/contract/finance/operationAnalysis/cost-analysis`,
    method: 'get',
    params
  })
}

export function getProductAnalysisPrint(params) {
  return request({
    url: `/api/contract/finance/operationAnalysis/cost-analysis/print`,
    method: 'get',
    params
  })
}

