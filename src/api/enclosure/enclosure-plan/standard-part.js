import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `enclosureStandardPart`,
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'enclosureStandardPart',
    method: 'put',
    data: [data]
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'enclosureStandardPart',
    method: 'post',
    data
  })
}

export function del(data) {
  return request({
    module: 'plan',
    url: `enclosureStandardPart`,
    method: 'delete',
    data
  })
}

// 导入标准件
export function standardPartUpload(data) {
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
 * 下载零构件关联清单
 */
export function downloadStandardPart(params) {
  return request({
    module: 'plan',
    url: 'enclosureStandardPart/download-template',
    responseType: 'blob',
    method: 'get',
    params
  })
}
export default { get, edit, del, add }
