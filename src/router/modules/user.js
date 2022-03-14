// 路由：人员管理
export default {
  id: 794,
  name: '人员管理',
  children: [
    {
      path: 'user-manage',
      component: 'Layout',
      hidden: false,
      name: 'userManage',
      alwaysShow: false,
      redirect: '/user-manage/user',
      meta: { title: '人员管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'User',
          path: 'user',
          hidden: false,
          component: '/user-manage/user/index',
          meta: { title: '用户配置', icon: 'user', noCache: true }
        }
      ]
    },
    {
      path: 'dept-manage',
      component: 'Layout',
      hidden: false,
      name: 'deptManage',
      alwaysShow: false,
      redirect: '/dept-manage/dept',
      meta: { title: '部门管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'Dept',
          path: 'dept',
          hidden: false,
          component: '/user-manage/dept/index',
          meta: { title: '部门配置', icon: 'user', noCache: true }
        }
      ]
    },
    {
      path: 'job-manage',
      component: 'Layout',
      hidden: false,
      name: 'jobManage',
      alwaysShow: false,
      redirect: '/job-manage/job',
      meta: { title: '岗位管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'Job',
          path: 'job',
          hidden: false,
          component: '/user-manage/job/index',
          meta: { title: '岗位配置', icon: 'user', noCache: true }
        }
      ]
    },
    {
      path: 'role-manage',
      component: 'Layout',
      hidden: false,
      name: 'roleManage',
      alwaysShow: false,
      redirect: '/role-manage/role',
      meta: { title: '角色管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'Role',
          path: 'role',
          hidden: false,
          component: '/user-manage/role/index',
          meta: { title: '角色配置', icon: 'user', noCache: true }
        }
      ]
    }
  ]
}
