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
 * 集成模型
 */
export function integrationModel({ projectId, monomerIds }) {
  return request({
    url: 'api/model/3DModel/model/integration',
    method: 'post',
    params: { projectId },
    data: monomerIds
  })
}

/**
 * 集成模型删除
 */
export function integrationModelDel({ projectId }) {
  return request({
    url: 'api/model/3DModel/remove/integration',
    method: 'delete',
    params: { projectId }
  })
}

/**
 * 模型版本号变更
 * @param {*} data
 */
export function editEdition({ monomerId, edition }) {
  return request({
    url: '/api/model/3DModel/upload/edition',
    method: 'post',
    params: { monomerId, edition }
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
export function getTranslate(monomerId) {
  return request({
    url: 'api/model/3DModel/translate',
    method: 'get',
    params: { monomerId }
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
 * @param {number} monomerId 单体id
 */
export function getProjectTree(monomerId) {
  return request({
    url: '/api/model/3DModel/project/tree',
    method: 'get',
    timeout: 600000,
    params: { monomerId }
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
 * @param {number} monomerId 元件id
 */
export function getArtifactSearch({ serialNumber, monomerId }) {
  return request({
    url: '/api/model/3DModel/artifact/element',
    method: 'get',
    timeout: 600000,
    params: { serialNumber, monomerId }
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
 * @param {number} monomerId 单体id
 */
export function getLogistics({ monomerId }) {
  return request({
    url: '/api/model/3DModel/logistics',
    method: 'get',
    timeout: 600000,
    params: { monomerId }
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
