import request from '@/utils/request'

/**
 * 收支分析
 * @param {string} dataTime
 * @param {number} branchCompanyId 分支机构
 * @returns
 */
export function getIncomeAnalysis({ branchCompanyId, dateTime }) {
  return request({
    url: `/api/operational/analysis/income`,
    method: 'get',
    params: { branchCompanyId, dateTime }
  })
}
