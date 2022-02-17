import request from '@/utils/request'

/**
 *
 * 获取工序
 */
export function process({ id, productType }) {
  return request({
    module: 'mes',
    url: 'wages/product/process',
    method: 'get',
    params: { id, productType }
  })
}

/**
 *
 * 获取班组工价
 */
export function teamPrice({ processId, productId, productType, factoryId, productionLineId, workshopId, organizationType }) {
  return request({
    module: 'mes',
    url: 'wages/product/process/team',
    method: 'get',
    params: { processId, productId, productType, factoryId, productionLineId, workshopId, organizationType }
  })
}

/**
 *
 * 修改班组工价
 */
export function edit(data) {
  return request({
    module: 'mes',
    url: 'wages/product/process/wage',
    method: 'put',
    data
  })
}

/**
 *
 * 审核工序价格列表
 */
export function checkList(params) {
  return request({
    module: 'mes',
    url: 'wages/audit/page',
    method: 'get',
    params
  })
}

/**
 *
 * 审核工序价格详情
 */
export function checkDetail(id) {
  return request({
    module: 'mes',
    url: `wages/audit/${id}/list`,
    method: 'get'
  })
}

/**
 *
 * 审核工序价格数量
 */
export function checkNumber({ projectId, organizationType }) {
  return request({
    module: 'mes',
    url: 'wages/audit/number',
    method: 'get',
    params: { projectId, organizationType }
  })
}

/**
 *
 * 审核工序价格
 */
export function check(data) {
  return request({
    module: 'mes',
    url: 'wages/status',
    method: 'put',
    data
  })
}
