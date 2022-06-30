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

// export function add(data) {
//   return request({
//     module: 'plan',
//     url: 'enclosure/save',
//     method: 'post',
//     data
//   })
// }

export function add(data) {
  return request({
    module: 'plan',
    url: 'enclosure/saveList',
    method: 'post',
    params: { areaId: data.areaId },
    data: data.list
  })
}

export function editStatus(id, params) {
  return request({
    module: 'plan',
    url: `enclosure/updateStatus/${id}`,
    method: 'put',
    params: params
  })
}

export function del(id) {
  return request({
    module: 'plan',
    url: `enclosure/deleteEnclosure/${id}`,
    method: 'delete'
  })
}

// 按区域一键清空
export function delEnclosureByArea(areaId) {
  return request({
    module: 'plan',
    url: `enclosure/clearByAreaId/${areaId}`,
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

// 获取总计
export function getTotalSum(params) {
  return request({
    module: 'plan',
    url: `enclosure/getSum`,
    method: 'get',
    params
  })
}
export default { get, edit, del, add }
