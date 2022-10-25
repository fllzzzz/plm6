// 路由：项目管理
export default {
  id: 1116,
  name: '智能车间',
  children: [
    {
      path: '/intellect-workshop-manage/assembly-manage',
      component: 'Layout',
      hidden: false,
      name: 'AssemblyManage',
      alwaysShow: false,
      redirect: '/intellect-workshop-manage/assembly-manage/assembly-send-receive-storage',
      meta: { title: '部件入发存管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'AssemblySendReceiveStorage',
          path: 'assembly-send-receive-storage',
          hidden: false,
          component: '/intellect-workshop-manage/assembly-send-receive-storage/index',
          meta: { title: '部件入发存', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/intellect-workshop-manage/part-manage',
      component: 'Layout',
      hidden: false,
      name: 'PartManage',
      alwaysShow: false,
      redirect: '/intellect-workshop-manage/part-manage/part-send-receive-storage',
      meta: { title: '零件入发存管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PartReceiveStorage',
          path: 'part-send-receive-storage',
          hidden: false,
          component: '/intellect-workshop-manage/part-send-receive-storage/index',
          meta: { title: '零件入发存', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
