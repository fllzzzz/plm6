// 路由：BIM
export default {
  id: 9,
  name: 'BIM',
  children: [
    {
      path: '/bim',
      component: 'Layout',
      hidden: false,
      name: 'BIM',
      alwaysShow: false,
      redirect: '/bim/model-show',
      meta: { title: 'BIM模型', icon: 'project', noCache: true },
      children: [
        {
          path: '/bim/model-show',
          component: '/bim/model-show/index',
          hidden: false,
          name: 'bimModelShow',
          alwaysShow: false,
          meta: { title: '模型展示', icon: 'project', noCache: true }
        }
        // {
        //   path: '/bim/integration-model-show',
        //   component: '/bim/integration-model-show/index',
        //   hidden: false,
        //   name: 'bimIntegrationModelShow',
        //   alwaysShow: false,
        //   meta: { title: '集成模型展示', icon: 'project', noCache: true }
        // }
      ]
    }
  ]
}
