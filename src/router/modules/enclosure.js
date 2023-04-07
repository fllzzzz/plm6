// 路由：人员管理
export default {
  id: 888,
  name: '围护MES',
  children: [
    {
      path: 'enclosure-manage',
      component: 'Layout',
      hidden: false,
      name: 'enclosureManage',
      alwaysShow: false,
      redirect: '/enclosure-manage/enclosure-plan',
      meta: { title: '计划管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'enclosurePlan',
          path: 'enclosure-plan',
          hidden: false,
          component: '/enclosure/enclosure-plan/area/index',
          meta: { title: '计划列表', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
