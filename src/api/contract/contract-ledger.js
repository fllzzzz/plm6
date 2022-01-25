import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/pageProjectLedger',
    method: 'get',
    params
  })
}

// 获取项目金额变更记录
export function moneyChangeLog(params) {
  return request({
    module: 'contract',
    url: 'project/listContractAmountRecord',
    method: 'get',
    params
  })
}

// 获取项目金额累计
export function ledgerSum() {
  return request({
    module: 'contract',
    url: 'project/getLedgerSum',
    method: 'get'
  })
}

export default { get }
