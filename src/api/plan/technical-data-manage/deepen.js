import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'drawing/struct',
    method: 'get',
    params
  })
}

// export function add(data) {
//   return request({
//     module: 'plan',
//     url: 'drawing/struct',
//     method: 'post',
//     data
//   })
// }

// export function edit(data) {
//   return request({
//     module: 'plan',
//     url: 'drawing/struct',
//     method: 'put',
//     data
//   })
// }

export function del(ids) {
  return request({
    module: 'plan',
    url: 'drawing/struct',
    method: 'delete',
    data: ids
  })
}

export function download({ id }) {
  return request({
    module: 'plan',
    url: `drawing/struct/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

export function downloadByMonomer(monomerId, type) {
  return request({
    module: 'plan',
    url: `drawing/struct/export`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob',
    params: { monomerId, type }
  })
}

export function upload(data) {
  return request({
    module: 'plan',
    url: `drawing/struct/upload`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 预览图纸
 *
 * @export
 * @param {*}  serialNumber|required 物料编号
 * @param {*}  type|required 物料类型
 * @param {*}  areaId|required 区域id
 * @returns
 */
export function previewPDF({ serialNumber, monomerId, type, enclosureCategory }) {
  return request({
    module: 'plan',
    url: 'drawing/pdf/preview',
    method: 'get',
    responseType: 'blob',
    timeout: 6000000,
    params: { serialNumber, monomerId, type, enclosureCategory }
  })
}
/**
 * 覆盖图纸
 * @param {*} serialNumber|required 物料编号
 * @param {*} monomerId|required 单体id
 * @param {*} projectId|required 项目id
 * @param {*} fileName|required 文件名
 * @param {*} type|required 物料类型
 * @param {*} img|required 图纸
 * @returns
 */
export function overrideJPG(data) {
  return request({
    module: 'plan',
    url: 'drawing/struct/compile',
    method: 'post',
    data
  })
}

export function checkSerialNumber(data) {
  return request({
    module: 'plan',
    url: 'drawing/product/checkSerialNumber',
    method: 'post',
    data
  })
}

export function getSerialNumber(params) {
  return request({
    module: 'plan',
    url: 'enclosure/list',
    method: 'get',
    params
  })
}

export default { get, del, download }