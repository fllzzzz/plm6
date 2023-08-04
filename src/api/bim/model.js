import request from '@/utils/request'

/**
 * 模型导入
 * @param {*} data
 */
export function upload(data) {
  return request({
    url: 'api/model/3DModel/upload',
    method: 'post',
    timeout: 6000000, // 模型导入
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 单体模型信息
 */
export function monomerModelInfo({ monomerId }) {
  return request({
    url: 'api/model/3DModel/monomer',
    method: 'get',
    params: { monomerId }
  })
}

/**
 * 区域模型信息
 */
export function areaModelInfo({ monomerId }) {
  return request({
    url: 'api/model/3DModel/area',
    method: 'get',
    params: { monomerId }
  })
}

/**
 * 模型删除
 */
export function modelDel({ monomerId, areaId }) {
  return request({
    url: 'api/model/3DModel',
    method: 'delete',
    params: { monomerId, areaId }
  })
}

/**
 * 集成模型
 */
export function integrationModel({ monomerId, areaIds }) {
  return request({
    url: 'api/model/3DModel/model/integration',
    method: 'post',
    params: { monomerId },
    data: areaIds
  })
}

/**
 * 集成模型删除
 */
export function integrationModelDel({ monomerId }) {
  return request({
    url: 'api/model/3DModel/remove/integration',
    method: 'delete',
    params: { monomerId }
  })
}

/**
 * 模型版本号变更
 * @param {*} data
 */
export function editEdition({ monomerId, edition }) {
  return request({
    url: 'api/model/3DModel/upload/edition',
    method: 'post',
    params: { monomerId, edition }
  })
}

/**
 * 模型配置
 * @param {*} data
 */
export function modelConfig(data) {
  return request({
    url: 'api/model/configure',
    method: 'put',
    data
  })
}

/**
 * 获取集成3d模型viewToken
 * @param {number} projectId 项目id
 */
export function getIMTranslate(projectId) {
  return request({
    url: 'api/model/3DModel/integrate',
    method: 'get',
    params: { projectId }
  })
}

/**
 * 获取3d模型viewToken
 * @param {number} monomerId 单体id
 */
export function getTranslate(monomerId, areaId) {
  return request({
    url: 'api/model/3DModel/translate',
    method: 'get',
    params: { monomerId, areaId }
  })
}

export function getArtifactStatus({ fileId, menuBar }) {
  return request({
    url: 'api/model/3DModel/artifact/status',
    method: 'get',
    timeout: 600000, // 后台处理数据过慢
    params: { fileId, menuBar }
  })
}

export function getBridgeArtifactStatus({ fileId, menuBar }) {
  return request({
    url: 'api/model/3DModel/bridge/status',
    method: 'get',
    timeout: 600000, // 后台处理数据过慢
    params: { fileId, menuBar }
  })
}

/**
 * 集成模型状态
 */
export function getIntegrationArtifactStatus({ projectId, menuBar }) {
  return request({
    url: '/api/model/3DModel/artifact/integrate/status',
    method: 'get',
    timeout: 600000, // 后台处理数据过慢
    params: { projectId, menuBar }
  })
}

/**
 * 获取集成模型中的文件信息
 * @param {number} projectId 项目id
 */
export function getIntegrateMonomer(projectId) {
  return request({
    url: '/api/model/3DModel/integrate/monomer',
    method: 'get',
    timeout: 600000,
    params: { projectId }
  })
}

/**
 * 获取项目树构件信息
 * @param {number} fileId 单体id
 */
export function getProjectTree(fileId) {
  return request({
    url: '/api/model/3DModel/project/tree',
    method: 'get',
    timeout: 600000,
    params: { fileId }
  })
}

/**
 * 获取项目树构件信息
 * @param {number} fileId 单体id
 */
export function getBridgeProjectTree(fileId) {
  return request({
    url: '/api/model/3DModel/bridge/project/tree',
    method: 'get',
    timeout: 600000,
    params: { fileId }
  })
}

/**
 * 获取构件信息
 * @param {number} fileId 文件id
 * @param {number} elementId 元件id
 */
export function getArtifactInfo({ fileId, elementId, menuBar }) {
  return request({
    url: '/api/model/3DModel/artifact',
    method: 'get',
    timeout: 600000,
    params: { fileId, elementId, menuBar }
  })
}

/**
 * 获取构件信息
 * @param {number} fileId 文件id
 * @param {number} elementId 元件id
 */
export function getBridgeArtifactInfo({ fileId, elementId, menuBar }) {
  return request({
    url: '/api/model/3DModel/bridge/box',
    method: 'get',
    timeout: 600000,
    params: { fileId, elementId, menuBar }
  })
}

/**
 * 获取构件信息
 * @param {number} elementId 元件id
 */
export function getIntegrateArtifactInfo({ projectId, elementId, menuBar }) {
  return request({
    url: '/api/model/3DModel/artifact/integrate',
    method: 'get',
    timeout: 600000,
    params: { projectId, elementId, menuBar }
  })
}

/**
 * 通过构件编号搜索
 * @param {number} serialNumber 构件编号
 * @param {number} fileId 元件id
 */
export function getArtifactSearch({ serialNumber, fileId }) {
  return request({
    url: '/api/model/3DModel/artifact/element',
    method: 'get',
    timeout: 600000,
    params: { serialNumber, fileId }
  })
}

/**
 * 通过构件编号搜索
 * @param {number} serialNumber 构件编号
 * @param {number} fileId 元件id
 */
export function getBridgeArtifactSearch({ serialNumber, fileId }) {
  return request({
    url: '/api/model/3DModel/bridge/element',
    method: 'get',
    timeout: 600000,
    params: { serialNumber, fileId }
  })
}

/**
 * 通过构件编号搜索
 * @param {number} serialNumber 构件编号
 * @param {number} projectId 元件id
 */
export function getIntegrateArtifactSearch({ serialNumber, projectId }) {
  return request({
    url: '/api/model/3DModel/artifact/element/integrate',
    method: 'get',
    timeout: 600000,
    params: { serialNumber, projectId }
  })
}

/**
 * 获取生产信息
 * @param {number} fileId 文件id
 */
export function getArtifactProduction({ fileId, menuBar }) {
  return request({
    url: '/api/model/3DModel/artifact/production',
    method: 'get',
    timeout: 600000,
    params: { fileId, menuBar }
  })
}

/**
 * 获取生产信息
 * @param {number} fileId 文件id
 */
export function getBridgeArtifactProduction({ fileId, menuBar }) {
  return request({
    url: '/api/model/3DModel/bridge/production',
    method: 'get',
    timeout: 600000,
    params: { fileId, menuBar }
  })
}

/**
 * 获取生产信息
 * @param {number} projectId
 */
export function getIntegrateArtifactProduction({ projectId, menuBar }) {
  return request({
    url: '/api/model/3DModel/artifact/production/integrate',
    method: 'get',
    timeout: 600000,
    params: { projectId, menuBar }
  })
}

/**
 * 获取状态下的信息
 * @param {number} fileId 文件id
 */
export function getStatusDetail({ fileId, status, menuBar }) {
  return request({
    url: '/api/model/3DModel/status/details',
    method: 'get',
    timeout: 600000,
    params: { fileId, status, menuBar }
  })
}

/**
 * 获取状态下的信息
 * @param {number} fileId 文件id
 */
export function getBridgeStatusDetail({ fileId, status, menuBar }) {
  return request({
    url: '/api/model/3DModel/bridge/status/details',
    method: 'get',
    timeout: 600000,
    params: { fileId, status, menuBar }
  })
}

/**
 * 获取状态下的信息
 * @param {number} projectId 文件id
 */
export function getIntegrateStatusDetail({ projectId, status, menuBar }) {
  return request({
    url: '/api/model/3DModel/status/details/integrate',
    method: 'get',
    timeout: 600000,
    params: { projectId, status, menuBar }
  })
}

/**
 * 获取物流信息
 * @param {number} fileId 单体id
 */
export function getLogistics({ fileId }) {
  return request({
    url: '/api/model/3DModel/logistics',
    method: 'get',
    timeout: 600000,
    params: { fileId }
  })
}

/**
 * 获取物流信息
 * @param {number} fileId 单体id
 */
export function getBridgeLogistics({ fileId }) {
  return request({
    url: '/api/model/3DModel/bridge/logistics',
    method: 'get',
    timeout: 600000,
    params: { fileId }
  })
}

/**
 * 获取集成模型物流信息
 * @param {number} projectId 项目id
 */
export function getIntegrateLogistics({ projectId }) {
  return request({
    url: '/api/model/3DModel/logistics/integrate',
    method: 'get',
    timeout: 600000,
    params: { projectId }
  })
}

/**
 * 获取物流信息
 * @param {number} productId 产品id
 * @param {number} productType 产品类型
 */
export function getBimDrawing({ productId, productType }) {
  return request({
    url: '/api/plan/drawing/product/bim',
    method: 'get',
    timeout: 600000,
    params: { productId, productType }
  })
}
