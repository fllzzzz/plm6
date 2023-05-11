import request from '@/utils/request'

// 摊销管理列表
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/page',
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
 * @param {number} amortizationClassEnum 摊销枚举类型
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

// 查询自动待摊销列表数据
export function getAutoAmortization(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getAutoAmortization',
    method: 'get',
    params
  })
}

// 查询手动待摊销列表数据
export function getManualAmortization(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getManualPendingAmortization',
    method: 'get',
    params
  })
}

// 查询手动待摊销条数
export function getManualAmortizationCount(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/getManualPendingAmortizationCount',
    method: 'get',
    params
  })
}

// 单个摊销
export function singleAmortize(data) {
  return request({
    module: 'contract',
    url: 'contract/amortization/amortization',
    method: 'post',
    data
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

export default { get }
