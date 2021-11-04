import request from '@/utils/request'

// // 加载菜单
// export function fetchMenus() {
//   return request({
//     module: 'common',
//     url: 'menu/build',
//     method: 'get'
//   })
// }

// 获取所有用户
export function getUserAllSimple(params) {
  return request({
    url: 'api/user/all/simple',
    method: 'get',
    params,
    cancelKey: false
  })
}
