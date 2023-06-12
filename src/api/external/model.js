import request from '@/utils/request'

/**
 * 根据文件id获取大屏模型
 * @param {number} fileId | required 文件id
 */
export function getModel(url, params) {
  return request({
    url: `${url}/api/largeScreen/getModelInfoByFileId`,
    method: 'get',
    params
  })
}

/**
 * 根据单体id获取大屏模型
 * @param {number} monomerId | required 单体id
 * @param {number} areaId | required 区域id
 */
export function getTranslate(url, params) {
  return request({
    url: `${url}/api/largeScreen/translate`,
    method: 'get',
    params
  })
}
