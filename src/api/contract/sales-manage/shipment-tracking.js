import request from '@/utils/request'

/**
 * 箱体发运跟踪列表
 * @param {number} pageNumber|required 页码
 * @param {number} pageSize|required 页大小
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 * @param {string} serialNumber 编号
 * @param {string} specification 规则
 * @param {string} material 材质
 * @returns
 */
export function bridgeList(params) {
  return request({
    module: 'contract',
    url: 'business/ship/bridge/detail',
    method: 'get',
    params
  })
}

/**
 * 结构发运跟踪列表
 * @param {number} pageNumber|required 页码
 * @param {number} pageSize|required 页大小
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 * @param {string} serialNumber 编号
 * @param {string} specification 规则
 * @param {string} material 材质
 * @returns
 */
export function structureList(params) {
  return request({
    module: 'contract',
    url: 'business/ship/detail',
    method: 'get',
    params
  })
}

/**
 * 直发件发运跟踪列表
 * @param {number} pageNumber|required 页码
 * @param {number} pageSize|required 页大小
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 * @param {string} serialNumber 编号
 * @param {string} specification 规则
 * @param {string} material 材质
 * @returns
 */
export function directList(params) {
  return request({
    module: 'contract',
    url: 'business/ship/direct/detail',
    method: 'get',
    params
  })
}

/**
 * 围护发运跟踪列表
 * @param {number} pageNumber|required 页码
 * @param {number} pageSize|required 页大小
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 * @param {string} serialNumber 编号
 * @param {string} specification 规则
 * @param {string} material 材质
 * @returns
 */
export function enclosureList(params) {
  return request({
    module: 'contract',
    url: 'business/ship/enclosure/detail',
    method: 'get',
    params
  })
}

/**
 * 配套件发运跟踪列表
 * @param {number} pageNumber|required 页码
 * @param {number} pageSize|required 页大小
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 * @param {string} serialNumber 编号
 * @param {string} specification 规则
 * @param {string} material 材质
 * @returns
 */
export function auxiliaryMaterialList(params) {
  return request({
    module: 'contract',
    url: 'business/ship/detail/standard',
    method: 'get',
    params
  })
}

/**
 * 建刚发运汇总
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 */
export function shipSummary(params) {
  return request({
    module: 'contract',
    url: 'business/ship/summary',
    method: 'get',
    params
  })
}

/**
 * 桥梁发运汇总
 * @param {Number} projectId | required 项目id
 * @param {Number} productType | required 产品类型
 * @param {Number} startDate | required 开始时间
 * @param {Number} endDate | required 结束时间
 */
export function bridgeShipSummary(params) {
  return request({
    module: 'contract',
    url: 'business/ship/bridge/summary',
    method: 'get',
    params
  })
}
