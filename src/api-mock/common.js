// import configRouter from '@/router/modules/config'

// // 用户菜单
// const userMenu = {
//   url: '/api/common/v1/menu/',
//   method: 'post',
//   response: () => {
//     return {
//       code: 20000,
//       message: '成功',
//       data: {
//         'content': [
//           configRouter
//         ]
//       }
//     }
//   }
// }

// export default [
//   userMenu
// ]

// Mock.mock(requestUrl + '/api/common/v1/menu/build', 'get', {
//   'code': 20000,
//   'message': '成功',
//   'data': {
//     'totalElements': 1,
//     'content': [{
//       'id': 8,
//       'path': null,
//       'redirect': null,
//       'name': null,
//       'component': null,
//       'alwaysShow': false,
//       'hidden': false,
//       'meta': {
//         'title': 'WMS',
//         'icon': '',
//         'noCache': true
//       },
//       'children': null
//     },
//     {
//       'id': 10,
//       'path': null,
//       'redirect': null,
//       'name': null,
//       'component': null,
//       'alwaysShow': false,
//       'hidden': false,
//       'meta': {
//         'title': '配置管理',
//         'icon': '',
//         'noCache': true
//       },
//       'children': null
//     }
//     ]
//   }
// })

// 获取所有用户
const getUserAllSimple = {
  url: '/api/user/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': { 'totalElements': 81, 'content': [{ 'id': 11, 'name': '小黄', 'deptName': null, 'phone': '13100000009', 'email': null }, { 'id': 69, 'name': '吕冰工人一', 'deptName': null, 'phone': '19152100660', 'email': null }, { 'id': 3, 'name': '李冰冰', 'deptName': null, 'phone': '13100000001', 'email': null }, { 'id': 4, 'name': '王中磊', 'deptName': null, 'phone': '13100000002', 'email': null }, { 'id': 5, 'name': '冯小刚', 'deptName': null, 'phone': '13100000003', 'email': null }, { 'id': 6, 'name': '川建国', 'deptName': null, 'phone': '13100000004', 'email': null }, { 'id': 7, 'name': '小强', 'deptName': null, 'phone': '13100000005', 'email': null }, { 'id': 8, 'name': '小张', 'deptName': null, 'phone': '13100000006', 'email': null }, { 'id': 9, 'name': '小李', 'deptName': null, 'phone': '13100000007', 'email': null }, { 'id': 10, 'name': '小刘', 'deptName': null, 'phone': '13100000008', 'email': null }, { 'id': 21, 'name': '张波', 'deptName': null, 'phone': '13000000003', 'email': null }, { 'id': 12, 'name': '小周', 'deptName': null, 'phone': '13800000000', 'email': null }, { 'id': 13, 'name': '小陈', 'deptName': null, 'phone': '13800000001', 'email': null }, { 'id': 14, 'name': '小侯', 'deptName': null, 'phone': '13800000002', 'email': null }, { 'id': 15, 'name': '小虎', 'deptName': null, 'phone': '13800000003', 'email': null }, { 'id': 16, 'name': '小米', 'deptName': null, 'phone': '13800000004', 'email': null }, { 'id': 17, 'name': '小赵', 'deptName': null, 'phone': '13800000005', 'email': null }, { 'id': 70, 'name': '吕冰工人二', 'deptName': null, 'phone': '18868689101', 'email': null }, { 'id': 19, 'name': '小王', 'deptName': null, 'phone': '13000000001', 'email': null }, { 'id': 71, 'name': '吕冰质检一', 'deptName': null, 'phone': '17718189181', 'email': null }, { 'id': 72, 'name': '吕冰车间管理', 'deptName': null, 'phone': '15888908987', 'email': null }, { 'id': 22, 'name': '王元阳', 'deptName': null, 'phone': '13971521825', 'email': null }, { 'id': 74, 'name': '吕冰仓库管理', 'deptName': null, 'phone': '16666666668', 'email': null }, { 'id': 24, 'name': '周斌', 'deptName': null, 'phone': '15195206466', 'email': null }, { 'id': 25, 'name': '李勇', 'deptName': null, 'phone': '13900000000', 'email': null }, { 'id': 75, 'name': '周庄民', 'deptName': null, 'phone': '13200000000', 'email': null }, { 'id': 27, 'name': '刘德华', 'deptName': null, 'phone': '13000000007', 'email': null }, { 'id': 28, 'name': '范冰冰', 'deptName': null, 'phone': '13000000004', 'email': null }, { 'id': 29, 'name': '胡军', 'deptName': null, 'phone': '13000000006', 'email': null }, { 'id': 30, 'name': '张学友', 'deptName': null, 'phone': '13000000008', 'email': null }, { 'id': 31, 'name': '向华强', 'deptName': null, 'phone': '13000000009', 'email': null }, { 'id': 32, 'name': '古天乐', 'deptName': null, 'phone': '13100000000', 'email': null }, { 'id': 33, 'name': '老董 ', 'deptName': null, 'phone': '13588228309', 'email': null }, { 'id': 34, 'name': '王凯', 'deptName': null, 'phone': '13900000003', 'email': null }, { 'id': 35, 'name': '刘再道', 'deptName': null, 'phone': '13900000005', 'email': null }, { 'id': 80, 'name': 'c111', 'deptName': null, 'phone': '15336579006', 'email': null }, { 'id': 37, 'name': '谢勇', 'deptName': null, 'phone': '13900000009', 'email': null }, { 'id': 38, 'name': '陈朝阳', 'deptName': null, 'phone': '13545906231', 'email': null }, { 'id': 39, 'name': '001', 'deptName': null, 'phone': '13500000000', 'email': null }, { 'id': 40, 'name': 'ttemp', 'deptName': null, 'phone': '18227189608', 'email': null }, { 'id': 41, 'name': '张斌', 'deptName': null, 'phone': '13308669976', 'email': null }, { 'id': 43, 'name': '彭凯', 'deptName': null, 'phone': '13297000000', 'email': null }, { 'id': 44, 'name': '王凯', 'deptName': null, 'phone': '13297000001', 'email': null }, { 'id': 45, 'name': '李二', 'deptName': null, 'phone': '17826808075', 'email': null }, { 'id': 46, 'name': '王勤', 'deptName': null, 'phone': '13297000004', 'email': null }, { 'id': 47, 'name': '刘鸿云', 'deptName': null, 'phone': '13297000002', 'email': null }, { 'id': 48, 'name': '李四', 'deptName': null, 'phone': '13297000005', 'email': null }, { 'id': 49, 'name': '彭继明', 'deptName': null, 'phone': '13297000006', 'email': null }, { 'id': 50, 'name': '史正阳', 'deptName': null, 'phone': '15035482678', 'email': null }, { 'id': 55, 'name': '张锐', 'deptName': null, 'phone': '15892482997', 'email': null }, { 'id': 56, 'name': '123', 'deptName': null, 'phone': '18072908276', 'email': null }, { 'id': 57, 'name': '曾万明', 'deptName': null, 'phone': '15982941763', 'email': null }, { 'id': 63, 'name': '李巍', 'deptName': null, 'phone': '18855321342', 'email': null }, { 'id': 65, 'name': '李工', 'deptName': null, 'phone': '18855321349', 'email': null }, { 'id': 66, 'name': '测试1', 'deptName': null, 'phone': '18855321322', 'email': null }, { 'id': 67, 'name': '测试', 'deptName': null, 'phone': '18855321256', 'email': null }, { 'id': 68, 'name': '张三', 'deptName': null, 'phone': '18855321356', 'email': null }, { 'id': 2, 'name': '张磊', 'deptName': '公司', 'phone': '18888888888', 'email': null }, { 'id': 1, 'name': '超级管理员', 'deptName': '公司', 'phone': '18005719019', 'email': null }, { 'id': 58, 'name': '杨娟', 'deptName': '公司', 'phone': '13264259482', 'email': null }, { 'id': 59, 'name': '王子豪', 'deptName': '公司', 'phone': '13000000789', 'email': null }, { 'id': 61, 'name': '周建桥', 'deptName': '公司', 'phone': '13000000123', 'email': null }, { 'id': 62, 'name': '董志鑫', 'deptName': '公司', 'phone': '13000000234', 'email': null }, { 'id': 83, 'name': 'zzmtest002', 'deptName': '公司', 'phone': '18700000000', 'email': null }, { 'id': 42, 'name': '张晓', 'deptName': '公司', 'phone': '15822668350', 'email': null }, { 'id': 76, 'name': '杜昊昊', 'deptName': '公司', 'phone': '17767223047', 'email': null }, { 'id': 77, 'name': 'ZZM', 'deptName': '公司', 'phone': '18767785201', 'email': null }, { 'id': 78, 'name': '林华', 'deptName': '公司', 'phone': '17859523616', 'email': null }, { 'id': 79, 'name': 'jlf', 'deptName': '公司', 'phone': '13685743812', 'email': null }, { 'id': 51, 'name': '张飞波', 'deptName': '公司', 'phone': '18627102865', 'email': null }, { 'id': 81, 'name': 'c001', 'deptName': '公司', 'phone': '15336579002', 'email': null }, { 'id': 82, 'name': '老男人001', 'deptName': '公司', 'phone': '18767089930', 'email': null }, { 'id': 18, 'name': '王欣', 'deptName': '生产部', 'phone': '13000000000', 'email': null }, { 'id': 26, 'name': '李亮', 'deptName': '业务部', 'phone': '13900000001', 'email': null }, { 'id': 53, 'name': 'xxxxx', 'deptName': '业务部', 'phone': '18072908275', 'email': null }, { 'id': 64, 'name': '李巍', 'deptName': '业务部', 'phone': '13000000321', 'email': null }, { 'id': 23, 'name': '洪太枫', 'deptName': '111', 'phone': '13855190301', 'email': null }, { 'id': 36, 'name': '刘文成', 'deptName': '111', 'phone': '13900000006', 'email': null }, { 'id': 52, 'name': '齐训利', 'deptName': '111', 'phone': '15882151448', 'email': null }, { 'id': 20, 'name': '张飞', 'deptName': '222', 'phone': '13000000002', 'email': null }, { 'id': 73, 'name': '吕冰商务', 'deptName': '222', 'phone': '18888888886', 'email': null }] }}
  }
}

export default [
  getUserAllSimple
]
