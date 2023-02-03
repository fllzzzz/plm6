import request from '@/utils/request'

/**
 *
 * 工价调整-构件类型汇总列表
 */
export function typeGet(params) {
  return request({
    module: 'mes',
    url: 'price/change/product/type',
    method: 'get',
    params
  })
}

/**
 *
 * 工价调整-构件类型汇总列表
 */
export function detailGet(params) {
  return request({
    module: 'mes',
    url: 'price/change/product/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取工序
 */
export function processGet({ id, configId, taskTypeEnum }) {
  return request({
    module: 'mes',
    url: `price/change/product/type/${id}/process`,
    method: 'get',
    params: { configId, taskTypeEnum }
  })
}

/**
 *
 * 工价调整-批量修改工价
 */
export function edit(data) {
  return request({
    module: 'mes',
    url: 'price/change/product',
    method: 'put',
    data
  })
}
