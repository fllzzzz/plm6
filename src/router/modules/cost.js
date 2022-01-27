// 路由：成本管理
export default {
  id: 4,
  name: '成本管理',
  children: [
    {
      path: '/cost/business-manage',
      component: 'Layout',
      hidden: false,
      name: 'BusinessManage',
      alwaysShow: false,
      redirect: '/cost/business-manage/trip-tracking/index',
      meta: { title: '商务管理', icon: 'list', noCache: true },
      children: [
        // {
        //   name: 'BusinessEntry',
        //   path: 'business-entry',
        //   hidden: false,
        //   component: '/cost/business-manage/business-entry/index',
        //   meta: { title: '商务录入', icon: 'list', noCache: true }
        // },
        {
          name: 'TripTracking',
          path: 'trip-tracking',
          hidden: false,
          component: '/cost/business-manage/trip-tracking/index',
          meta: { title: '车次跟踪', icon: 'list', noCache: true }
        },
        {
          name: 'BusinessTracking',
          path: 'business-tracking',
          hidden: false,
          component: '/cost/business-manage/business-tracking/index',
          meta: { title: '商务跟踪', icon: 'list', noCache: true }
        },
        {
          name: 'ShipmentLedger',
          path: 'shipment-ledger',
          hidden: false,
          component: '/cost/business-manage/shipment-ledger/index',
          meta: { title: '发运台账', icon: 'list', noCache: true }
        }
      ]
    }
  ]
}
