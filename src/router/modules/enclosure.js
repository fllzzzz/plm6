// 路由：围护
export default {
  id: 800,
  name: '围护MES',
  children: [
    {
      path: 'enclosure/production-manage',
      component: 'Layout',
      hidden: false,
      name: 'EnclosureManage',
      alwaysShow: false,
      redirect: '/enclosure/production-manage/scheduling-manage',
      meta: { title: '生产管理', icon: 'project', noCache: true },
      children: [
        {
          name: 'EnclosureSchedulingManage',
          path: 'scheduling-manage',
          hidden: false,
          component: '/enclosure/production-manage/scheduling-manage/index',
          meta: { title: '排产管理', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
