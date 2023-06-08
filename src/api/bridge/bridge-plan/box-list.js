import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/tech-box',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    url: '/api/bridge/tech-box',
    method: 'put',
    data
  })
}

// export function editStatus(type, id, params) {
//   return request({
//     module: 'plan',
//     url: `artifactMachinePart/updateStatus/${type}/${id}`,
//     method: 'put',
//     params
//   })
// }

export function del(data) {
  return request({
    url: '/api/bridge/tech-box',
    method: 'delete',
    data
  })
}

export function listUpload(data) {
  return request({
    url: '/api/bridge/tech-box/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 下载分段单元关联清单
 */
export function downloadBoxCell(params) {
  return request({
    url: '/api/bridge/tech-box/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}

/**
 * 下载零分段技术清单模板
 */
export function downloadBoxCellTemplate() {
  return request({
    url: '/api/bridge/tech-box/template-export',
    responseType: 'blob',
    method: 'get'
  })
}

// 获取分段对应的单元
export function boxCellDetail(params) {
  return request({
    url: '/api/bridge/tech-element/listByBoxId',
    method: 'get',
    params
  })
}

// 按区域一键清空
export function delBoxCellByArea(params) {
  return request({
    url: '/api/bridge/tech-box/clear-by-areaId',
    method: 'delete',
    params
  })
}

export default { edit, del, get }
