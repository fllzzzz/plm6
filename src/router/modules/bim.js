// 路由：BIM
export default {
  id: 951,
  name: 'BIM',
  children: [
    {
      path: '/bim',
      component: 'Layout',
      hidden: false,
      name: 'BIM',
      alwaysShow: false,
      redirect: '/bim/model-show',
      meta: { title: 'BIM模型', icon: 'bim', noCache: true },
      children: [
        {
          path: '/bim/model-show',
          component: '/bim/model-show/index',
          hidden: false,
          name: 'bimModelShow',
          alwaysShow: false,
          meta: { title: '模型展示', icon: 'bim', noCache: true }
        }
      ]
    }
  ]
}
