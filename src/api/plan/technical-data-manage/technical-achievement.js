import request from '@/utils/request'

/**
 * 获取项目下单体信息
 * @export
 * @param {number}  projectId|required 项目id
 * @param {number}  productType|required 生产类型
 * @param {number}  dataType|required 资料类型
 * @returns
 */
export function get(params) {
  return request({
    module: 'plan',
    url: 'drawing/drawing/data',
    method: 'get',
    params
  })
}

/**
 * 获取单体图纸信息
 * @export
 * @param {number}  monomerId|required 项目id
 * @param {number}  productType|required 生产类型
 * @param {number}  dataType|required 资料类型
 * @returns
 */
export function detail(params) {
  return request({
    module: 'plan',
    url: 'drawing/drawing/contrast',
    method: 'get',
    params
  })
}

/**
 * 删除图纸
 * @export
 * @param {array}  ids|required id
 * @returns
 */
export function del(ids) {
  return request({
    module: 'plan',
    url: 'drawing/delete',
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

export function downloadByMonomer(params) {
  return request({
    module: 'plan',
    url: `drawing/struct/export`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob',
    params
  })
}

/**
 * 上传技术资料压缩包
 * @export
 * @param {number}  monomerId|required 单体id
 * @param {number}  dataType|required 资料类型
 * @param {number}  productType|required 生产类型
 * @param {string} fileName|required 文件名
 * @param {*} file|required 文件流
 * @returns
 */
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
 * 上传技术资料压缩包(只导入未匹配的文件)
 * @export
 * @param {number}  monomerId|required 单体id
 * @param {number}  dataType|required 资料类型
 * @param {number}  productType|required 生产类型
 * @param {string} fileName|required 文件名
 * @param {*} file|required 文件流
 * @returns
 */
export function uploadChoose(data) {
  return request({
    module: 'plan',
    url: `drawing/struct/upload/choose`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 覆盖技术资料（单个文件，必须是覆盖）
 * @export
 * @param {number}  monomerId|required 单体id
 * @param {number}  dataType|required 资料类型
 * @param {number}  productType|required 生产类型
 * @param {string} fileName|required 文件名
 * @param {*} file|required 文件流
 * @returns
 */
export function update(data) {
  return request({
    module: 'plan',
    url: `drawing/struct/update`,
    method: 'put',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

export default { get, del, download }
