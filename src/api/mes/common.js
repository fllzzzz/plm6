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

// 所有生产班组
export function getProductionTeamAllSimple(params) {
  return request({
    module: 'mes',
    url: 'team',
    method: 'get',
    params
  })
}

// 所有质检班组
export function getInspectionTeamAllSimple(params) {
  return request({
    module: 'mes',
    url: 'inspectionTeam',
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
 * @description: 切割配置列表
 */
export function getAllCutConfigs() {
  return request({
    module: 'mes',
    url: `cut/list/cut`,
    method: 'get'
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
 * 围护：所有包单号列表
 * @export
 * @returns
 */
export function getEnclosureAllPackage(params) {
  return request({
    url: '/api/mes/enclosure/package/list',
    method: 'get',
    params
  })
}

/**
 * 获取区域下有任务的生产线idsArr
 * @param {number} monomerId 单体id
 * @param {number} areaId 区域id
 * @param {number} productType 类型
 * @returns
 */
export function getHasTaskLine({
  areaId,
  monomerId,
  productType
}) {
  return request({
    module: 'mes',
    url: 'task/productionLine/hasTask',
    method: 'get',
    params: {
      areaId,
      monomerId,
      productType
    }
  })
}

// 围护：获取所有批次
export function getEnclosureBatch(projectId) {
  return request({
    url: `/api/enclosurePlanDetail/tech/${projectId}`,
    method: 'get'
  })
}
