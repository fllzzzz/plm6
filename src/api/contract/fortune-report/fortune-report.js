import request from '@/utils/request'

// 业财报表
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/project-finance',
    method: 'get',
    params
  })
}

// 查询项目收款记录
export function getCollectionList(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/getCollection',
    method: 'get',
    params
  })
}

// 查询项目开票记录
export function getInvoiceList(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/getInvoice',
    method: 'get',
    params
  })
}

// 综合成本-直接费用：固定费用（主、辅、其它）
export function getOutDetail(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getOutDetail',
    method: 'get',
    params
  })
}

// 综合成本-摊销列表
export function getAmortizationList(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getAmortizationList',
    method: 'get',
    params
  })
}

// 综合成本-费用层级分类
export function getExpenseSubjectAll(params) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse/getExpenseSubjectAll',
    method: 'get',
    params
  })
}

// 综合成本-费用科目详情
export function getExpenseSubject(params) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse/getExpenseSubject',
    method: 'get',
    params
  })
}

// 综合成本-报销费用
export function getExpenseReimburse(params) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse/getExpenseReimburse',
    method: 'get',
    params
  })
}
export default { get }
