import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `standardPart`,
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'standardPart',
    method: 'put',
    data: [data]
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'standardPart',
    method: 'post',
    data: data.list
  })
}

export function del(data) {
  return request({
    module: 'plan',
    url: `standardPart`,
    method: 'delete',
    data
  })
}

// 导入标准件
export function standardPartUpload(data) {
  return request({
    module: 'plan',
    url: 'standardPart/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 下载零构件关联清单
 */
export function downloadStandardPart(params) {
  return request({
    module: 'plan',
    url: 'standardPart/download-template',
    responseType: 'blob',
    method: 'get',
    params
  })
}

// 围护：导入标准件
export function enclosureStandardPartUpload(data) {
  return request({
    module: 'plan',
    url: 'enclosureStandardPart/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 围护：下载零构件关联清单
 */
export function enclosureDownloadStandardPart(params) {
  return request({
    module: 'plan',
    url: 'enclosureStandardPart/download-template',
    responseType: 'blob',
    method: 'get',
    params
  })
}
export default { get, edit, del, add }
