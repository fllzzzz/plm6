import request from '@/utils/request'

/**
 * 根据字典名称获取对应所有内容
 *
 * @export
 * @param {*} name
 * @returns
 */
export function getAll(name) {
  const params = {
    name
  }
  return request({
    url: 'api/dictDetail/all',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    url: 'api/dictDetail',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: 'api/dictDetail',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: 'api/dictDetail',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    url: 'api/dictDetail/' + id,
    method: 'delete'
  })
}

export default { add, edit, del, get }
