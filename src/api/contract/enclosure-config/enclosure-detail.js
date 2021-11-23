import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetail/all',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetailSave',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetail/upDate',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `enclosure/dictionaries/dictDetail/${id}`,
    method: 'delete'
  })
}
export default { get, add, edit, del }
