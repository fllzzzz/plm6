import request from '@/utils/request'

// 获取所有工厂
export function getFactoriesAllSimple(params) {
  return request({
    module: 'mes',
    url: 'factory',
    method: 'get',
    params: {
      boolEnabledEnum: true,
      ...params
    }
  })
}

// 获取所有车间
export function getWorkshopsAllSimple(params) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'get',
    params: {
      boolEnabledEnum: true,
      ...params
    }
  })
}

// 获取所有工序
export function getProcessAllSimple(params) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'get',
    params
  })
}

/**
 * 获取所有生产线
 * @export
 * @returns
 */
export function getLinesAllSimple(params) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'get',
    params: {
      boolEnabledEnum: true,
      ...params
    }
  })
}

/**
 * 层级：工厂-车间-生产线
 * @export
 * @returns
 */
export function getAllFactoryWorkshopLines(params) {
  return request({
    module: 'mes',
    url: 'factory/production_line_group',
    method: 'get',
    params: {
      boolEnabledEnum: true,
      ...params
    }
  })
}

/**
 * 所有包单号列表
 * @export
 * @returns
 */
export function getAllPackage(params) {
  return request({
    module: 'mes',
    url: 'package/list',
    method: 'get',
    params
  })
}

/**
 * 获取区域下有任务的生产线idsArr
 * @param {number} monomerId 单体id
 * @param {number} areaId 区域id
 * @param {number} type 物料清单类型
 * @returns
 */
export function getHasTaskLine({ areaId, monomerId, type }) {
  return request({
    module: 'mes',
    url: 'task/productionLine/hasTask',
    method: 'get',
    params: { areaId, monomerId, type }
  })
}
