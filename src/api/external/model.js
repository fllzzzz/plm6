import request from '@/utils/request'

/**
 * 获取大屏模型
 * @param {number} fileId | required 文件id
 */
export function getModel(url, params) {
  return request({
    url: `${url}/api/largeScreen/getModelInfoByFileId`,
    method: 'get',
    params
  })
}
