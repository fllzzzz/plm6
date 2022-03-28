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

export function getArtifactStatus(fileId) {
  return request({
    url: 'api/model/3DModel/model/3DModel/artifact/status',
    method: 'get',
    timeout: 600000, // 后台处理数据过慢
    params: { fileId }
  })
}
