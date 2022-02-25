// 路由：项目管理
export default {
  id: 7,
  name: '项目管理',
  children: [
    {
      path: 'project-manage/data-manage',
      component: 'Layout',
      hidden: false,
      name: 'DataManage',
      alwaysShow: false,
      redirect: '/project-manage/data-manage/construction-log',
      meta: { title: '资料管理', icon: 'list', noCache: true },
      children: [
        // {
        //   name: 'ImageProgress',
        //   path: 'image-progress',
        //   hidden: false,
        //   component: '/project-manage/data-manage/image-progress/index',
        //   meta: { title: '形象进度', icon: 'list', noCache: true }
        // },
        {
          name: 'ConstructionLog',
          path: 'construction-log',
          hidden: false,
          component: '/project-manage/data-manage/construction-log/index',
          meta: { title: '施工日志', icon: 'list', noCache: true }
        }
      ]
    }
  ]
}

