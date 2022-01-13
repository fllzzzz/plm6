import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `enclosure/listPage`,
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'enclosure/update',
    method: 'put',
    data
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'enclosure/save',
    method: 'post',
    data
  })
}

export function editStatus(id) {
  return request({
    module: 'plan',
    url: `enclosure/updateStatus/${id}`,
    method: 'put'
  })
}

export function del(id) {
  return request({
    module: 'plan',
    url: `enclosure/deleteEnclosure/${id}`,
    method: 'delete'
  })
}

/**
 *围护导入2021
 */
export function listUpload(data) {
  return request({
    module: 'plan',
    url: 'enclosure/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 下载围护模板
 */
export function downloadEnclosureTemplate(params) {
  return request({
    module: 'plan',
    url: `enclosure/download/${params.category}`,
    responseType: 'blob',
    method: 'get'
  })
}

/**
 * 下载围护数据
 */
export function downloadEnclosureData(params) {
  return request({
    module: 'plan',
    url: `enclosure/export`,
    responseType: 'blob',
    method: 'get',
    params
  })
}

// 折边件上传画图
export function saveDraw(data) {
  return request({
    module: 'plan',
    url: 'enclosure/saveDrawing',
    method: 'post',
    data
  })
}

// 折边件预览图片
export function previewImg(attachmentId) {
  return request({
    module: 'plan',
    url: `enclosure/preview/${attachmentId}`,
    responseType: 'blob',
    timeout: 6000000,
    method: 'get'
  })
}
export default { get, edit, del, add }
