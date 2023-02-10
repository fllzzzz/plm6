import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'spacer/parallel/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'spacer/parallel',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'spacer/parallel',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'spacer/parallel',
    method: 'delete',
    data: ids
  })
}

export function uploadFun(data) {
  return request({
    module: 'mes',
    url: `spacer/parallel/struct/upload`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

// 临时上传dxf文件
export function shortTimeUploadFun(data) {
  return request({
    module: 'mes',
    url: `spacer/parallel/upload/temporary/files`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

// 获取厚度材质筛选
export function getParallelParams() {
  return request({
    module: 'mes',
    url: 'spacer/parallel/wait/condition',
    method: 'get'
  })
}
export default { get, add, edit, del }
