import request from '@/utils/request'

// 摊销管理列表
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getAmortizedPageList',
    method: 'get',
    params
  })
}

/**
 * 摊销详情
 * @export
 * @param {number} id 摊销列表id
 * @returns
 */
export function detail(params) {
  return request({
    module: 'contract',
    url: `contract/amortization/getAmortizationDetail`,
    method: 'get',
    params
  })
}

/**
 * 摊销分类树
 * @export
 * @param {boolean} enable 是否开启：true开启，false关闭
 * @param {number} expenseClassEnum 摊销枚举类型
 * @returns
 */
export function amortizationClassTree(params) {
  return request({
    module: 'contract',
    url: `contract/amortizationClass/tree`,
    method: 'get',
    params
  })
}

// 保存摊销分类
export function saveAmortizationClass(data) {
  return request({
    module: 'contract',
    url: 'contract/amortizationClass/save',
    method: 'post',
    data
  })
}

// 查询摊销汇总列表数据
export function getAmortizationSummaryList(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getToBeAmortized',
    method: 'get',
    params
  })
}

// 查询手动待摊销条数
export function getManualAmortizationCount(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getToBeAmortizedCount',
    method: 'get',
    params
  })
}

// 单个摊销
export function singleAmortize(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/amortization',
    method: 'post',
    params
  })
}

// 一键摊销
export function amortizationAll(data) {
  return request({
    module: 'contract',
    url: 'contract/amortization/amortizationAll',
    method: 'post',
    data
  })
}

// 一键自动摊销
export function autoAmortization(data) {
  return request({
    module: 'contract',
    url: 'contract/amortization/autoAmortization',
    timeout: 6000000, // 后台处理数据过慢
    method: 'post',
    data
  })
}

export default { get }
